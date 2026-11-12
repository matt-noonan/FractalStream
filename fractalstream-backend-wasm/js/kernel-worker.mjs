// A worker that renders blocks with the wasm backend's kernels (see
// worker-pool.mjs). It works as a browser Web Worker and as a Node worker
// thread.
//
// Each worker has its own (unshared) memory, which every kernel it's given
// imports: a job's arguments are copied in, the kernel renders the block
// into it, and the pixels go back to the pool in a buffer of their own
// (transferred, not copied). So nothing needs shared memory.
//
// The memory holds a job's arguments at ARGS_BASE (lists in them point
// into them, assuming they're there: Backend.Wasm's workerArgsBase), then
// its copy of the autocolor snapshot, the block's pixels, the kernel's
// arena (for lists) of ARENA_SIZE bytes (Backend.Wasm's arenaSize), and
// the list of points the kernel would add to autocolor groups (misses),
// which go back with the pixels.
//
// A tool's handler (a module exporting `tool`: see Backend.Wasm.Code) runs
// the same way, once: its arguments at ARGS_BASE, then its arena (of the
// size the job asks for), where it makes its lists from the bottom up and
// puts its draw commands from the top down; it writes where those end and
// begin at addresses 0 and 4. What goes back is its arguments as it left
// them, and the used parts of the arena (with where the arena was, since
// lists point into it).
//
// Tables (see Backend.Wasm.Table) are kept from job to job, in a region
// above the jobs' scratch memory: a job lists the tables it uses, by
// version, and brings the contents of any this worker doesn't have yet;
// each descriptor in the job's arguments gets the address of its table's
// elements. Beyond TABLE_BUDGET bytes, the least recently used tables the
// job doesn't need are dropped (and the reply says which, so the pool
// sends them again if they're needed).
//
// Messages in:
//   { kernel: id, bytes }          a kernel or tool (its module's bytes) to keep
//   { job, kernel: id, args, snapshot, width, height, subsamples, x0, y0, dx, dy, tables, tableData }
//   { job, kernel: id, tool: true, args, arenaSize, tables, tableData }
// (tables: [{ version, offset }], tableData: { version: bytes })
// Messages out:
//   { job, issues, pixels, misses, evicted }
//   { job, status, args, arenaBase, lists, draws, evicted }   (a tool's: no lists or draws, if bit 0 is set)
//   or { job, error, evicted }

const ARGS_BASE = 16;
const ARENA_SIZE = 1024 * 1024;
const MISS_CAPACITY = 256;   // Backend.Wasm's missCapacity
const MISS_SIZE = 24;
const align8 = (n) => (n + 7) & ~7;

const memory = new WebAssembly.Memory({ initial: 16 });

// ---------------------------------------------------------------------
// Tables

const TABLE_BUDGET = 256 * 1024 * 1024;
const PAGE = 65536;
const placed = new Map();   // version -> { offset, size, used }
let tablesBase = 0, tablesEnd = 0, clock = 0;
// (dropped since the last reply, which says so, however the job went)
let dropped = [];
const takeDropped = () => { const d = dropped; dropped = []; return d; };

function ensureMemory(end) {
  if (memory.buffer.byteLength < end) memory.grow(Math.ceil((end - memory.buffer.byteLength) / PAGE));
}

/** Put the job's tables in place, above its scratch memory (which ends at
    scratchEnd), noting any it drops. */
function placeTables(m, scratchEnd) {
  // (the region moves up, with room to spare, if the scratch memory has
  // grown into it)
  if (tablesBase < scratchEnd) {
    const base = Math.ceil((scratchEnd + 16 * 1024 * 1024) / PAGE) * PAGE;
    ensureMemory(base + (tablesEnd - tablesBase));
    new Uint8Array(memory.buffer).copyWithin(base, tablesBase, tablesEnd);
    for (const t of placed.values()) t.offset += base - tablesBase;
    tablesEnd += base - tablesBase;
    tablesBase = base;
  }
  for (const [version, bytes] of Object.entries(m.tableData ?? {})) {
    const v = Number(version);
    if (placed.has(v)) continue;
    const offset = (tablesEnd + 7) & ~7;
    ensureMemory(offset + bytes.length);
    new Uint8Array(memory.buffer, offset, bytes.length).set(bytes);
    placed.set(v, { offset, size: bytes.length, used: 0 });
    tablesEnd = offset + bytes.length;
  }
  const needed = new Set((m.tables ?? []).map((t) => t.version));
  for (const v of needed) { const t = placed.get(v); if (t) t.used = ++clock; }
  let total = 0;
  for (const t of placed.values()) total += t.size;
  if (total > TABLE_BUDGET) {
    for (const [v, t] of [...placed].sort((a, b) => a[1].used - b[1].used)) {
      if (total <= TABLE_BUDGET) break;
      if (needed.has(v)) continue;
      placed.delete(v);
      total -= t.size;
      dropped.push(v);
    }
    // (what's left, packed together again)
    const heap = new Uint8Array(memory.buffer);
    let cursor = tablesBase;
    for (const t of [...placed.values()].sort((a, b) => a.offset - b.offset)) {
      cursor = (cursor + 7) & ~7;
      heap.copyWithin(cursor, t.offset, t.offset + t.size);
      t.offset = cursor;
      cursor += t.size;
    }
    tablesEnd = cursor;
  }
}

/** Point each of the job's table descriptors (in its arguments, now at
    ARGS_BASE) at its table's elements. */
function patchTables(m) {
  const view = new DataView(memory.buffer);
  for (const { version, offset } of m.tables ?? []) {
    const t = placed.get(version);
    if (!t) throw new Error(`table ${version} isn't in this worker`);
    view.setInt32(ARGS_BASE + offset, t.offset, true);
  }
}

// What a kernel imports: the math functions, from libm.wasm (built next
// to this file by dev-tools/wasm/build.sh), as Backend.Wasm's in-engine
// instances get them too. Not JavaScript's Math: calling JavaScript from
// wasm on several worker threads at once made Node (V8) on macOS/ARM
// crash now and then (see WEB_PLAN.md), and this way every host computes
// them alike.
//
// And the autocolor lookup: autocolor.wasm (also built here), over this
// worker's memory.
const isNode = typeof process !== "undefined" && process.versions?.node;
async function load(name) {
  const url = new URL(name, import.meta.url);
  return isNode ? (await import("node:fs")).readFileSync(url)
                : await (await fetch(url)).arrayBuffer();
}
const imports = (async () => ({
  math: (await WebAssembly.instantiate(await load("./libm.wasm"))).instance.exports,
  autocolor: (await WebAssembly.instantiate(await load("./autocolor.wasm"), { env: { memory } }))
    .instance.exports,
}))();

const kernels = new Map();

function handle(m, { math, autocolor }) {
  if (m.bytes) {
    // (each worker compiles its own copy: it takes well under a millisecond)
    const module = new WebAssembly.Module(m.bytes);
    const instance = new WebAssembly.Instance(module, { env: { memory }, math, autocolor });
    kernels.set(m.kernel, instance.exports);
    return null;
  }
  try {
    const exports = kernels.get(m.kernel);
    if (!exports) throw new Error(`no kernel ${m.kernel} in this worker`);
    if (m.tool) return runTool(m, exports.tool);
    const kernel = exports.kernel;
    const args = new Uint8Array(m.args);
    const snapshot = new Uint8Array(m.snapshot);
    const snap = align8(ARGS_BASE + args.length);
    const out = align8(snap + snapshot.length);
    const size = 3 * m.width * m.height;
    const arena = align8(out + size);
    const misses = arena + ARENA_SIZE;
    const need = misses + 8 + MISS_SIZE * MISS_CAPACITY;
    ensureMemory(need);
    placeTables(m, need);
    new Uint8Array(memory.buffer, ARGS_BASE, args.length).set(args);
    patchTables(m);
    new Uint8Array(memory.buffer, snap, snapshot.length).set(snapshot);
    new Int32Array(memory.buffer, misses, 2).set([0, MISS_CAPACITY]);
    const issues = kernel(out, m.width, m.height, m.subsamples, ARGS_BASE, arena, ARENA_SIZE,
                          snapshot.length > 0 ? snap : 0, misses, m.x0, m.y0, m.dx, m.dy);
    const pixels = new Uint8Array(memory.buffer, out, size).slice();
    const missCount = new Int32Array(memory.buffer, misses, 1)[0];
    const missed = new Uint8Array(memory.buffer, misses + 8, MISS_SIZE * missCount).slice();
    return [{ job: m.job, issues, pixels, misses: missed, evicted: takeDropped() }, [pixels.buffer, missed.buffer]];
  } catch (e) {
    return [{ job: m.job, error: String(e?.stack ?? e), evicted: takeDropped() }, []];
  }
}

function runTool(m, tool) {
  const args = new Uint8Array(m.args);
  const arena = align8(ARGS_BASE + args.length);
  const top = arena + m.arenaSize;
  ensureMemory(top);
  placeTables(m, top);
  new Uint8Array(memory.buffer, ARGS_BASE, args.length).set(args);
  patchTables(m);
  const status = tool(ARGS_BASE, arena, m.arenaSize, 0);
  if (status & 1) return [{ job: m.job, status, evicted: takeDropped() }, []];
  const [bump, end] = new Int32Array(memory.buffer, 0, 2);
  const bytes = (from, to) => new Uint8Array(memory.buffer, from, to - from).slice();
  const out = bytes(ARGS_BASE, ARGS_BASE + args.length), lists = bytes(arena, bump), draws = bytes(end, top);
  return [{ job: m.job, status, args: out, arenaBase: arena, lists, draws, evicted: takeDropped() }, [out.buffer, lists.buffer, draws.buffer]];
}

if (typeof process !== "undefined" && process.versions?.node) {
  const { parentPort } = await import("node:worker_threads");
  parentPort.on("message", async (m) => {
    const reply = handle(m, await imports);
    if (reply) parentPort.postMessage(...reply);
  });
} else {
  self.onmessage = async (e) => {
    const reply = handle(e.data, await imports);
    if (reply) self.postMessage(...reply);
  };
}
