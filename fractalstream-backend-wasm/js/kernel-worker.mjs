// A worker that renders blocks with the wasm backend's kernels (see
// worker-pool.mjs). It works as a browser Web Worker and as a Node worker
// thread.
//
// Each worker has its own (unshared) memory, which every kernel it's given
// imports: a job's arguments are copied in, the kernel renders the block
// into it, and the pixels go back to the pool in a buffer of their own
// (transferred, not copied). So nothing needs shared memory.
//
// Messages in:
//   { kernel: id, module }         a kernel (a WebAssembly.Module) to keep
//   { job, kernel: id, args, width, height, subsamples, x0, y0, dx, dy }
// Messages out:
//   { job, issues, pixels }        or { job, error }

// What a kernel imports (as Backend.Wasm's in-engine instances get).
const math = {
  sin: Math.sin, cos: Math.cos, tan: Math.tan,
  sinh: Math.sinh, cosh: Math.cosh, tanh: Math.tanh,
  asin: Math.asin, acos: Math.acos, atan: Math.atan,
  asinh: Math.asinh, acosh: Math.acosh, atanh: Math.atanh,
  exp: Math.exp, log: Math.log,
  pow: Math.pow, atan2: Math.atan2, hypot: Math.hypot,
  fmod: (a, b) => a % b,
};

const memory = new WebAssembly.Memory({ initial: 16 });
const kernels = new Map();

function handle(m) {
  if (m.module) {
    const instance = new WebAssembly.Instance(m.module, { env: { memory }, math });
    kernels.set(m.kernel, instance.exports.kernel);
    return null;
  }
  try {
    const kernel = kernels.get(m.kernel);
    if (!kernel) throw new Error(`no kernel ${m.kernel} in this worker`);
    const args = new Uint8Array(m.args);
    const out = (args.length + 7) & ~7;
    const size = 3 * m.width * m.height;
    const need = out + size;
    if (memory.buffer.byteLength < need) {
      memory.grow(Math.ceil((need - memory.buffer.byteLength) / 65536));
    }
    new Uint8Array(memory.buffer, 0, args.length).set(args);
    const issues = kernel(out, m.width, m.height, m.subsamples, 0, m.x0, m.y0, m.dx, m.dy);
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
