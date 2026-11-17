// Project archives (.fsz: a zip, as the core's Data.Zip reads and writes
// it), compressed and inflated by the browser rather than the engine.
//
// The engine can do it itself (zlib, compiled into its wasm), but Safari
// runs that code very slowly: Per_7's 92 MB table took 38 s to inflate
// there, against a quarter of a second elsewhere, and saving it took 53 s.
// The browser's own DecompressionStream and CompressionStream take a
// fraction of a second everywhere. So the worker (engine.ts) gives the
// engine an archive to open with every entry stored (which Data.Zip reads
// as it is), and the engine saves archives stored (Data.Zip's `Stored`),
// for the worker to deflate before the page saves them.
//
// Only what Data.Zip can read is rewritten (no zip64; nothing over 4 GB):
// anything else is left as it was (for the engine to explain).

const LOCAL = 0x04034b50, CENTRAL = 0x02014b50, END = 0x06054b50;

/** Whether some bytes are a zip archive (they start with a local file header). */
export function isZip(bytes: Uint8Array): boolean {
  return bytes.length >= 4 && new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength).getUint32(0, true) === LOCAL;
}

/** Inflate or deflate raw deflate data, with the browser's own zlib.
    (Written in pieces: WebKit can't stream a Blob of a hundred MB or so.) */
async function squeeze(data: Uint8Array, how: "inflate" | "deflate"): Promise<Uint8Array> {
  const stream = how === "inflate" ? new DecompressionStream("deflate-raw") : new CompressionStream("deflate-raw");
  const writer = stream.writable.getWriter();
  const reading = new Response(stream.readable).arrayBuffer();
  const piece = 1 << 20;
  const writing = (async () => {
    for (let at = 0; at < data.length; at += piece) await writer.write(data.subarray(at, Math.min(data.length, at + piece)));
    await writer.close();
  })();
  const [inflated] = await Promise.all([reading, writing]);
  return new Uint8Array(inflated);
}

/** An entry: its name (as the archive has it), and its contents, packed
    by its method (0, stored; 8, deflated); its CRC-32 is its contents' as
    they are. */
interface Entry { name: Uint8Array; flags: number; time: number; date: number; crc: number;
                  method: number; size: number; packed: Uint8Array }

/** The archive, with every entry stored rather than deflated; or the
    archive as it was, if it has nothing deflated, or isn't one this can
    rewrite. */
export async function inflateArchive(bytes: Uint8Array): Promise<Uint8Array> {
  return repack(bytes, 0);
}

/** The archive, with every entry deflated rather than stored (as the
    desktop app saves them); or as it was, if it isn't one this can rewrite. */
export async function deflateArchive(bytes: Uint8Array): Promise<Uint8Array> {
  return repack(bytes, 8);
}

async function repack(bytes: Uint8Array, method: 0 | 8): Promise<Uint8Array> {
  try {
    const entries = readEntries(bytes);
    if (!entries || entries.every((e) => e.method === method)) return bytes;
    for (const e of entries) {
      if (e.method === method) continue;
      e.packed = await squeeze(e.packed, method === 0 ? "inflate" : "deflate");
      e.method = method;
    }
    return writeArchive(entries) ?? bytes;
  } catch {
    // (damaged, or past what a zip without zip64 holds: the engine says why)
    return bytes;
  }
}

/** An archive's entries, in its order; or null, if it isn't one this can rewrite. */
function readEntries(bytes: Uint8Array): Entry[] | null {
  const v = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const n = bytes.length;
  // the end of central directory record: 22 bytes, at most 64 KB of comment after it
  let end = -1;
  for (let i = n - 22; i >= Math.max(0, n - 22 - 65535); i--) if (v.getUint32(i, true) === END) { end = i; break; }
  if (end < 0) return null;
  const count = v.getUint16(end + 10, true);
  let at = v.getUint32(end + 16, true);
  if (count === 0xffff || at === 0xffffffff) return null;
  const entries: Entry[] = [];
  for (let k = 0; k < count; k++) {
    if (at + 46 > n || v.getUint32(at, true) !== CENTRAL) return null;
    const method = v.getUint16(at + 10, true);
    if (method !== 0 && method !== 8) return null;
    const packedSize = v.getUint32(at + 20, true), size = v.getUint32(at + 24, true);
    const nameLen = v.getUint16(at + 28, true), extraLen = v.getUint16(at + 30, true), commentLen = v.getUint16(at + 32, true);
    const local = v.getUint32(at + 42, true);
    if (local + 30 > n || v.getUint32(local, true) !== LOCAL) return null;
    const start = local + 30 + v.getUint16(local + 26, true) + v.getUint16(local + 28, true);
    if (start + packedSize > n) return null;
    entries.push({ name: bytes.subarray(at + 46, at + 46 + nameLen), flags: v.getUint16(at + 8, true),
                   time: v.getUint16(at + 12, true), date: v.getUint16(at + 14, true),
                   crc: v.getUint32(at + 16, true), method, size, packed: bytes.subarray(start, start + packedSize) });
    at += 46 + nameLen + extraLen + commentLen;
  }
  return entries;
}

/** An archive of these entries (as Data.Zip writes one: no extra fields,
    no comments); or null, if it would be past 4 GB. */
function writeArchive(entries: Entry[]): Uint8Array | null {
  if (entries.some((e) => e.size > 0xfffffffe || e.packed.length > 0xfffffffe)) return null;
  const localSize = (e: Entry) => 30 + e.name.length + e.packed.length;
  const centralSize = entries.reduce((s, e) => s + 46 + e.name.length, 0);
  const cdStart = entries.reduce((s, e) => s + localSize(e), 0);
  if (cdStart + centralSize + 22 > 0xffffffff) return null;
  const out = new Uint8Array(cdStart + centralSize + 22);
  const v = new DataView(out.buffer);
  // (version, flags, method, time, date, CRC-32, sizes, name's length, no extra)
  const header = (p: number, e: Entry) => {
    v.setUint16(p, 20, true); v.setUint16(p + 2, e.flags & 0x0800, true); v.setUint16(p + 4, e.method, true);
    v.setUint16(p + 6, e.time, true); v.setUint16(p + 8, e.date, true); v.setUint32(p + 10, e.crc, true);
    v.setUint32(p + 14, e.packed.length, true); v.setUint32(p + 18, e.size, true);
    v.setUint16(p + 22, e.name.length, true); v.setUint16(p + 24, 0, true);
  };
  let p = 0;
  const offsets: number[] = [];
  for (const e of entries) {
    offsets.push(p);
    v.setUint32(p, LOCAL, true); header(p + 4, e);
    out.set(e.name, p + 30); out.set(e.packed, p + 30 + e.name.length);
    p += localSize(e);
  }
  entries.forEach((e, i) => {
    v.setUint32(p, CENTRAL, true); v.setUint16(p + 4, 20, true); header(p + 6, e);
    // (no comment, disk 0, no attributes; where its local header is)
    v.setUint16(p + 32, 0, true); v.setUint16(p + 34, 0, true); v.setUint16(p + 36, 0, true); v.setUint32(p + 38, 0, true);
    v.setUint32(p + 42, offsets[i], true);
    out.set(e.name, p + 46);
    p += 46 + e.name.length;
  });
  v.setUint32(p, END, true); v.setUint16(p + 4, 0, true); v.setUint16(p + 6, 0, true);
  v.setUint16(p + 8, entries.length, true); v.setUint16(p + 10, entries.length, true);
  v.setUint32(p + 12, centralSize, true); v.setUint32(p + 16, cdStart, true); v.setUint16(p + 20, 0, true);
  return out;
}
