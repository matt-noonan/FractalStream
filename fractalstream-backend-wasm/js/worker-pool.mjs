// A pool of kernel workers (kernel-worker.mjs) for the wasm backend: the
// host (Node, or a browser page) makes one and puts it at
// globalThis.fractalstreamWorkers before starting the engine, which then
// renders a viewer's blocks on it, several at once. Without one, the
// engine renders on its own thread.
//
//   const pool = createPool(workers, makeWorker)
//
// where each worker is { post(message, transfer), onMessage(callback),
// terminate() }, so the pool doesn't care how the host makes them; and
// makeWorker() makes another, to replace one that's stopped.
//
// pool.size        how many workers
// pool.addKernel(bytes) -> id    a kernel (its module's bytes) to use
// pool.run(id, args, snapshot, width, height, subsamples, x0, y0, dx, dy)
//                  -> Promise<{ issues, pixels, misses }>   a block (with
//                  a copy of the autocolor snapshot), on the next idle
//                  worker (queued if none is)
// pool.runTool(id, args, arenaSize, tag)
//                  -> Promise<{ status, args, arenaBase, lists, draws }>
//                  a tool's handler, once (see kernel-worker.mjs)
// pool.stop(tag)   stop the tool jobs with this tag: one waiting is
//                  dropped; one running is stopped by stopping its worker
//                  (there's no other way to stop wasm code that's
//                  running), which is replaced by a new one. Either way
//                  its promise is rejected with an Error("stopped").
// pool.terminate()

export function createPool(workers, makeWorker) {
  const kernels = new Map();     // kernel id -> its module's bytes
  let nextKernel = 0;
  let nextJob = 0;
  const queue = [];              // jobs waiting for a worker
  const pending = new Map();     // job -> { resolve, reject }
  const state = workers.map((w) => ({ worker: w, busy: false, kernels: new Set(), job: null }));

  function dispatch() {
    for (const s of state) {
      if (s.busy || queue.length === 0) continue;
      const job = queue.shift();
      if (!s.kernels.has(job.kernel)) {
        s.worker.post({ kernel: job.kernel, bytes: kernels.get(job.kernel) }, []);
        s.kernels.add(job.kernel);
      }
      s.busy = true;
      s.job = job;
      const { transfer, ...message } = job;
      s.worker.post(message, transfer);
    }
  }

  // (a stopped worker's messages, if any come after it's replaced, are
  // ignored)
  function listen(s, w) {
    w.onMessage((m) => {
      if (s.worker !== w) return;
      s.busy = false;
      s.job = null;
      const p = pending.get(m.job);
      pending.delete(m.job);
      if (m.error) p.reject(new Error(m.error)); else p.resolve(m);
      dispatch();
    });
  }
  state.forEach((s) => listen(s, s.worker));

  function reject(job) {
    const p = pending.get(job.job);
    pending.delete(job.job);
    p?.reject(new Error("stopped"));
  }

  return {
    size: workers.length,
    addKernel(bytes) {
      const id = nextKernel++;
      kernels.set(id, bytes);
      return id;
    },
    run(kernel, args, snapshot, width, height, subsamples, x0, y0, dx, dy) {
      const job = nextJob++;
      // (args, snapshot: Uint8Arrays whose buffers are the job's own, to transfer)
      queue.push({ job, kernel, args: args.buffer, snapshot: snapshot.buffer,
                   width, height, subsamples, x0, y0, dx, dy, transfer: [args.buffer, snapshot.buffer] });
      const result = new Promise((resolve, reject) => pending.set(job, { resolve, reject }));
      dispatch();
      return result;
    },
    runTool(kernel, args, arenaSize, tag) {
      const job = nextJob++;
      queue.push({ job, kernel, tool: true, tag, args: args.buffer, arenaSize, transfer: [args.buffer] });
      const result = new Promise((resolve, reject) => pending.set(job, { resolve, reject }));
      dispatch();
      return result;
    },
    stop(tag) {
      for (let i = queue.length - 1; i >= 0; i--) {
        if (queue[i].tool && queue[i].tag === tag) reject(queue.splice(i, 1)[0]);
      }
      for (const s of state) {
        if (!s.job?.tool || s.job.tag !== tag) continue;
        const job = s.job;
        s.worker.terminate();
        s.worker = makeWorker();
        s.kernels = new Set();
        s.busy = false;
        s.job = null;
        listen(s, s.worker);
        reject(job);
      }
      dispatch();
    },
    terminate() {
      for (const s of state) s.worker.terminate();
    },
  };
}
