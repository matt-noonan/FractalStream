// Run a FractalStream wasm reactor module (one that exports its main as
// hs_start; see fractalstream-ui-headless's fsrun) under Node:
//
//   node run-node.mjs module.wasm module.js [arguments...]
//
// where module.js is what GHC's post-link.mjs made from module.wasm (the
// JavaScript FFI's glue). The module can read and write files anywhere,
// relative to the current folder, as a native program would.
//
// The wasm backend renders on a pool of worker threads, one per core, or
// FS_WASM_WORKERS of them (0 for none: then it renders on its own thread).
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { WASI } from "node:wasi";
import { Worker } from "node:worker_threads";
import { createPool } from "../../fractalstream-backend-wasm/js/worker-pool.mjs";

const [wasmPath, jsPath, ...args] = process.argv.slice(2);

const wasi = new WASI({
  version: "preview1",
  // (the RTS exits while initializing without at least a program name)
  args: [path.basename(wasmPath, ".wasm"), ...args],
  env: { ...process.env, PWD: process.cwd() },
  preopens: { "/": "/" },
  returnOnExit: true,
});

const workerCount = process.env.FS_WASM_WORKERS !== undefined
  ? Number(process.env.FS_WASM_WORKERS) : os.availableParallelism();
const workerScript = new URL("../../fractalstream-backend-wasm/js/kernel-worker.mjs", import.meta.url);
const pool = workerCount > 0
  ? createPool(Array.from({ length: workerCount }, () => {
      const w = new Worker(workerScript);
      return {
        post: (m, transfer) => w.postMessage(m, transfer),
        onMessage: (f) => w.on("message", f),
        terminate: () => w.terminate(),
      };
    }))
  : null;
// (for debugging: with FS_WASM_DUMP=folder, each job's parameters and
// arguments go there too, as job-N.json, to replay without the engine)
if (pool && process.env.FS_WASM_DUMP) {
  const run = pool.run.bind(pool);
  let n = 0;
  pool.run = (kernel, args, ...rest) => {
    fs.writeFileSync(`${process.env.FS_WASM_DUMP}/job-${n++}.json`,
      JSON.stringify({ kernel, args: Array.from(args), rest }));
    return run(kernel, args, ...rest);
  };
}
if (pool) globalThis.fractalstreamWorkers = pool;

// The math functions kernels import, for those on the engine's thread
// (the workers load their own).
globalThis.fractalstreamMath = (await WebAssembly.instantiate(fs.readFileSync(
  new URL("../../fractalstream-backend-wasm/js/libm.wasm", import.meta.url)))).instance.exports;

const __exports = {};
const mod = await WebAssembly.compile(fs.readFileSync(wasmPath));
const jsffi = (await import(path.resolve(jsPath))).default(__exports);
const instance = await WebAssembly.instantiate(mod, {
  ghc_wasm_jsffi: jsffi,
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(__exports, instance.exports);
wasi.initialize(instance);

let code = 0;
try {
  await instance.exports.hs_start();
} catch (e) {
  // exitWith throws an ExitCode exception, which rejects the promise
  const m = /ExitFailure (\d+)/.exec(String(e?.message ?? e));
  if (m) code = Number(m[1]);
  else if (!/ExitSuccess/.test(String(e?.message ?? e))) { console.error(e); code = 1; }
}
pool?.terminate();
process.exit(code);
