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
// pool.runTool(id, args, snapshot, arenaSize, tag)
//                  -> Promise<{ status, args, arenaBase, lists, draws }>
//                  a tool's handler, once (see kernel-worker.mjs)
// pool.stop(tag)   stop the tool jobs with this tag: one waiting is
//                  dropped; one running is stopped by stopping its worker
//                  (there's no other way to stop wasm code that's
//                  running), which is replaced by a new one. Either way
//                  its promise is rejected with an Error("stopped").
// pool.addTable(version, bytes)   a table's contents (by its version), to
//                  send to the workers that need it, once each
// pool.hasTable(version)           whether it has them (it drops the
//                  least recently used beyond TABLE_BUDGET bytes, unless a
//                  job waiting or running needs them)
//
// A job's last argument lists its tables, as (version, offset) pairs in
// an Int32Array: the worker puts the elements of its copy of each where
// the descriptor at that offset in the job's arguments says (see
// kernel-worker.mjs).
//
// pool.terminate()

const TABLE_BUDGET = 512 * 1024 * 1024;

export function createPool(workers, makeWorker) {
  const tables = new Map();      // version -> { bytes, used }
  let tableBytes = 0;
  let clock = 0;
  const kernels = new Map();     // kernel id -> its module's bytes
  let nextKernel = 0;
  let nextJob = 0;
  const queue = [];              // jobs waiting for a worker
  const pending = new Map();     // job -> { resolve, reject }
  const state = workers.map((w) => ({ worker: w, busy: false, kernels: new Set(), tables: new Set(), job: null }));

  // (version, offset) pairs, as the job's tables
  const tableList = (pairs) => {
    const list = [];
    for (let i = 0; i + 1 < pairs.length; i += 2) {
      list.push({ version: pairs[i], offset: pairs[i + 1] });
      const t = tables.get(pairs[i]);
      if (t) t.used = ++clock;
    }
    return list;
  };

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
      // (the contents of tables this worker doesn't have yet: copies, since
      // the pool keeps its own for other workers)
      message.tableData = {};
      for (const { version } of job.tables) {
        if (s.tables.has(version)) continue;
        message.tableData[version] = tables.get(version).bytes;
        s.tables.add(version);
      }
      s.worker.post(message, transfer);
    }
  }

  // (a stopped worker's messages, if any come after it's replaced, are
  // ignored)
  function listen(s, w) {
    w.onMessage((m) => {
      if (s.worker !== w) return;
      for (const v of m.evicted ?? []) s.tables.delete(v);
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
    run(kernel, args, snapshot, width, height, subsamples, x0, y0, dx, dy, tablePairs = []) {
      const job = nextJob++;
      // (args, snapshot: Uint8Arrays whose buffers are the job's own, to transfer)
      queue.push({ job, kernel, args: args.buffer, snapshot: snapshot.buffer, tables: tableList(tablePairs),
                   width, height, subsamples, x0, y0, dx, dy, transfer: [args.buffer, snapshot.buffer] });
      const result = new Promise((resolve, reject) => pending.set(job, { resolve, reject }));
      dispatch();
      return result;
    },
    runTool(kernel, args, snapshot, arenaSize, tag, tablePairs = []) {
      const job = nextJob++;
      queue.push({ job, kernel, tool: true, tag, args: args.buffer, snapshot: snapshot.buffer, arenaSize,
                   tables: tableList(tablePairs), transfer: [args.buffer, snapshot.buffer] });
      const result = new Promise((resolve, reject) => pending.set(job, { resolve, reject }));
      dispatch();
      return result;
    },
    hasTable(version) {
      return tables.has(version);
    },
    addTable(version, bytes) {
      if (tables.has(version)) return;
      tables.set(version, { bytes, used: ++clock });
      tableBytes += bytes.length;
      if (tableBytes <= TABLE_BUDGET) return;
      // (the least recently used first, but none a job still needs)
      const needed = new Set([...queue, ...state.map((s) => s.job)].flatMap((j) => j?.tables ?? []).map((t) => t.version));
      needed.add(version);
      for (const [v, t] of [...tables].sort((a, b) => a[1].used - b[1].used)) {
        if (tableBytes <= TABLE_BUDGET) break;
        if (needed.has(v)) continue;
        tables.delete(v);
        tableBytes -= t.bytes.length;
      }
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
        s.tables = new Set();
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
