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
// Messages in:
//   { kernel: id, bytes }          a kernel or tool (its module's bytes) to keep
//   { job, kernel: id, args, snapshot, width, height, subsamples, x0, y0, dx, dy }
//   { job, kernel: id, tool: true, args, arenaSize }
// Messages out:
//   { job, issues, pixels, misses }
//   { job, status, args, arenaBase, lists, draws }   (a tool's: just status, if bit 0 is set)
//   or { job, error }

const ARGS_BASE = 16;
const ARENA_SIZE = 1024 * 1024;
const MISS_CAPACITY = 256;   // Backend.Wasm's missCapacity
const MISS_SIZE = 24;
const align8 = (n) => (n + 7) & ~7;

const memory = new WebAssembly.Memory({ initial: 16 });

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
    if (memory.buffer.byteLength < need) {
      memory.grow(Math.ceil((need - memory.buffer.byteLength) / 65536));
    }
    new Uint8Array(memory.buffer, ARGS_BASE, args.length).set(args);
    new Uint8Array(memory.buffer, snap, snapshot.length).set(snapshot);
    new Int32Array(memory.buffer, misses, 2).set([0, MISS_CAPACITY]);
    const issues = kernel(out, m.width, m.height, m.subsamples, ARGS_BASE, arena, ARENA_SIZE,
                          snapshot.length > 0 ? snap : 0, misses, m.x0, m.y0, m.dx, m.dy);
    const pixels = new Uint8Array(memory.buffer, out, size).slice();
    const missCount = new Int32Array(memory.buffer, misses, 1)[0];
    const missed = new Uint8Array(memory.buffer, misses + 8, MISS_SIZE * missCount).slice();
    return [{ job: m.job, issues, pixels, misses: missed }, [pixels.buffer, missed.buffer]];
  } catch (e) {
    return [{ job: m.job, error: String(e?.stack ?? e) }, []];
  }
}

function runTool(m, tool) {
  const args = new Uint8Array(m.args);
  const arena = align8(ARGS_BASE + args.length);
  const top = arena + m.arenaSize;
  if (memory.buffer.byteLength < top) memory.grow(Math.ceil((top - memory.buffer.byteLength) / 65536));
  new Uint8Array(memory.buffer, ARGS_BASE, args.length).set(args);
  const status = tool(ARGS_BASE, arena, m.arenaSize, 0);
  if (status & 1) return [{ job: m.job, status }, []];
  const [bump, end] = new Int32Array(memory.buffer, 0, 2);
  const bytes = (from, to) => new Uint8Array(memory.buffer, from, to - from).slice();
  const out = bytes(ARGS_BASE, ARGS_BASE + args.length), lists = bytes(arena, bump), draws = bytes(end, top);
  return [{ job: m.job, status, args: out, arenaBase: arena, lists, draws }, [out.buffer, lists.buffer, draws.buffer]];
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
