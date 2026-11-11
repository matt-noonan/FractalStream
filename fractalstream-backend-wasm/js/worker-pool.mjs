// A pool of kernel workers (kernel-worker.mjs) for the wasm backend: the
// host (Node, or a browser page) makes one and puts it at
// globalThis.fractalstreamWorkers before starting the engine, which then
// renders a viewer's blocks on it, several at once. Without one, the
// engine renders on its own thread.
//
//   const pool = createPool(workers)
//
// where each worker is { post(message, transfer), onMessage(callback),
// terminate() }, so the pool doesn't care how the host makes them.
//
// pool.size        how many workers
// pool.addKernel(bytes) -> id    a kernel (its module's bytes) to use
// pool.run(id, args, width, height, subsamples, x0, y0, dx, dy)
//                  -> Promise<{ issues, pixels }>   a block, on the next
//                  idle worker (queued if none is)
// pool.terminate()

export function createPool(workers) {
  const kernels = new Map();     // kernel id -> its module's bytes
  let nextKernel = 0;
  let nextJob = 0;
  const queue = [];              // jobs waiting for a worker
  const pending = new Map();     // job -> { resolve, reject }
  const state = workers.map((w) => ({ worker: w, busy: false, kernels: new Set() }));

  function dispatch() {
    for (const s of state) {
      if (s.busy || queue.length === 0) continue;
      const job = queue.shift();
      if (!s.kernels.has(job.kernel)) {
        s.worker.post({ kernel: job.kernel, bytes: kernels.get(job.kernel) }, []);
        s.kernels.add(job.kernel);
      }
      s.busy = true;
      s.worker.post(job, [job.args]);
    }
  }

  state.forEach((s) => s.worker.onMessage((m) => {
    s.busy = false;
    const p = pending.get(m.job);
    pending.delete(m.job);
    if (m.error) p.reject(new Error(m.error)); else p.resolve(m);
    dispatch();
  }));

  return {
    size: workers.length,
    addKernel(bytes) {
      const id = nextKernel++;
      kernels.set(id, bytes);
      return id;
    },
    run(kernel, args, width, height, subsamples, x0, y0, dx, dy) {
      const job = nextJob++;
      // (args: a Uint8Array whose buffer is the job's own, to transfer)
      queue.push({ job, kernel, args: args.buffer, width, height, subsamples, x0, y0, dx, dy });
      const result = new Promise((resolve, reject) => pending.set(job, { resolve, reject }));
      dispatch();
      return result;
    },
    terminate() {
      for (const s of state) s.worker.terminate();
    },
  };
}
