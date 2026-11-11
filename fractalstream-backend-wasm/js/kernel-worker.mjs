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

// What a kernel imports (as Backend.Wasm's in-engine instances get).
// These are wrappers, not Math's own functions: V8 calls an imported
// Math.sin (and the like) from wasm by a fast path of its own, and with
// kernels using it on two or more worker threads at once, Node (V8) on
// macOS/ARM crashed now and then with SIGILL (9 runs in 40 of a replayed
// twisted-mandelbrot render; none in 40 with these). See WEB_PLAN.md.
const math = {
  sin: (x) => Math.sin(x), cos: (x) => Math.cos(x), tan: (x) => Math.tan(x),
  sinh: (x) => Math.sinh(x), cosh: (x) => Math.cosh(x), tanh: (x) => Math.tanh(x),
  asin: (x) => Math.asin(x), acos: (x) => Math.acos(x), atan: (x) => Math.atan(x),
  asinh: (x) => Math.asinh(x), acosh: (x) => Math.acosh(x), atanh: (x) => Math.atanh(x),
  exp: (x) => Math.exp(x), log: (x) => Math.log(x),
  pow: (x, y) => Math.pow(x, y), atan2: (y, x) => Math.atan2(y, x), hypot: (x, y) => Math.hypot(x, y),
  fmod: (a, b) => a % b,
};

const ARGS_BASE = 16;
const ARENA_SIZE = 1024 * 1024;
const align8 = (n) => (n + 7) & ~7;

const memory = new WebAssembly.Memory({ initial: 16 });
const kernels = new Map();

function handle(m) {
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
  parentPort.on("message", (m) => {
    const reply = handle(m);
    if (reply) parentPort.postMessage(...reply);
  });
} else {
  self.onmessage = (e) => {
    const reply = handle(e.data);
    if (reply) self.postMessage(...reply);
  };
}
