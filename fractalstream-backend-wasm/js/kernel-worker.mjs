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
// the block's pixels, then the kernel's arena (for lists) of ARENA_SIZE
// bytes (Backend.Wasm's arenaSize).
//
// Messages in:
//   { kernel: id, bytes }          a kernel (its module's bytes) to keep
//   { job, kernel: id, args, width, height, subsamples, x0, y0, dx, dy }
// Messages out:
//   { job, issues, pixels }        or { job, error }

// What a kernel imports: the math functions, from libm.wasm (built next
// to this file by dev-tools/wasm/build.sh), as Backend.Wasm's in-engine
// instances get them too. Not JavaScript's Math: calling JavaScript from
// wasm on several worker threads at once made Node (V8) on macOS/ARM
// crash now and then (see WEB_PLAN.md), and this way every host computes
// them alike.
const libmUrl = new URL("./libm.wasm", import.meta.url);
const math = (async () => {
  const bytes = typeof process !== "undefined" && process.versions?.node
    ? (await import("node:fs")).readFileSync(libmUrl)
    : await (await fetch(libmUrl)).arrayBuffer();
  return (await WebAssembly.instantiate(bytes)).instance.exports;
})();

const ARGS_BASE = 16;
const ARENA_SIZE = 1024 * 1024;
const align8 = (n) => (n + 7) & ~7;

const memory = new WebAssembly.Memory({ initial: 16 });
const kernels = new Map();

function handle(m, math) {
  if (m.bytes) {
    // (each worker compiles its own copy: it takes well under a millisecond)
    const module = new WebAssembly.Module(m.bytes);
    const instance = new WebAssembly.Instance(module, { env: { memory }, math });
    kernels.set(m.kernel, instance.exports.kernel);
    return null;
  }
  try {
    const kernel = kernels.get(m.kernel);
    if (!kernel) throw new Error(`no kernel ${m.kernel} in this worker`);
    const args = new Uint8Array(m.args);
    const out = align8(ARGS_BASE + args.length);
    const size = 3 * m.width * m.height;
    const arena = align8(out + size);
    const need = arena + ARENA_SIZE;
    if (memory.buffer.byteLength < need) {
      memory.grow(Math.ceil((need - memory.buffer.byteLength) / 65536));
    }
    new Uint8Array(memory.buffer, ARGS_BASE, args.length).set(args);
    const issues = kernel(out, m.width, m.height, m.subsamples, ARGS_BASE, arena, ARENA_SIZE,
                          m.x0, m.y0, m.dx, m.dy);
    const pixels = new Uint8Array(memory.buffer, out, size).slice();
    return [{ job: m.job, issues, pixels }, [pixels.buffer]];
  } catch (e) {
    return [{ job: m.job, error: String(e?.stack ?? e) }, []];
  }
}

if (typeof process !== "undefined" && process.versions?.node) {
  const { parentPort } = await import("node:worker_threads");
  parentPort.on("message", async (m) => {
    const reply = handle(m, await math);
    if (reply) parentPort.postMessage(...reply);
  });
} else {
  self.onmessage = async (e) => {
    const reply = handle(e.data, await math);
    if (reply) self.postMessage(...reply);
  };
}
