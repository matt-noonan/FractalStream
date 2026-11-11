// Run a FractalStream wasm reactor module (one that exports its main as
// hs_start; see fractalstream-ui-headless's fsrun) under Node:
//
//   node run-node.mjs module.wasm module.js [arguments...]
//
// where module.js is what GHC's post-link.mjs made from module.wasm (the
// JavaScript FFI's glue). The module can read and write files anywhere,
// relative to the current folder, as a native program would.
import fs from "node:fs";
import path from "node:path";
import { WASI } from "node:wasi";

const [wasmPath, jsPath, ...args] = process.argv.slice(2);

const wasi = new WASI({
  version: "preview1",
  // (the RTS exits while initializing without at least a program name)
  args: [path.basename(wasmPath, ".wasm"), ...args],
  env: { PWD: process.cwd() },
  preopens: { "/": "/" },
  returnOnExit: true,
});

const __exports = {};
const mod = await WebAssembly.compile(fs.readFileSync(wasmPath));
const jsffi = (await import(path.resolve(jsPath))).default(__exports);
const instance = await WebAssembly.instantiate(mod, {
  ghc_wasm_jsffi: jsffi,
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(__exports, instance.exports);
wasi.initialize(instance);

try {
  await instance.exports.hs_start();
} catch (e) {
  // exitWith throws an ExitCode exception, which rejects the promise
  const m = /ExitFailure (\d+)/.exec(String(e?.message ?? e));
  if (m) process.exit(Number(m[1]));
  if (/ExitSuccess/.test(String(e?.message ?? e))) process.exit(0);
  console.error(e);
  process.exit(1);
}
