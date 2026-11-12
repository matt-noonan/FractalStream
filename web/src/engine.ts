// The engine's worker: runs the engine (fractalstream-ui-web, a wasm
// reactor module) off the page's thread, with a pool of kernel workers
// for the wasm backend, and passes messages between it and the page
// (see protocol.ts).
//
// It does for the browser what dev-tools/wasm/run-node.mjs does for Node.

import { WASI, OpenFile, File, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import { createPool } from "./worker-pool.mjs";
import type { Request, Message, Frame } from "./protocol.js";

const scope = self as unknown as DedicatedWorkerGlobalScope;
const here = (name: string) => new URL(name, import.meta.url);

function send(message: Message, transfer: Transferable[] = []) {
  scope.postMessage(message, transfer);
}

async function fetchBytes(name: string): Promise<ArrayBuffer> {
  const response = await fetch(here(name));
  if (!response.ok) throw new Error(`couldn't load ${name}: ${response.status}`);
  return response.arrayBuffer();
}

// The kernel workers: one per core, keeping one core for this thread and
// one for the page.
const workerCount = Math.max(1, (navigator.hardwareConcurrency || 2) - 2);
const pool = createPool(Array.from({ length: workerCount }, () => {
  const w = new Worker(here("kernel-worker.mjs"), { type: "module" });
  return {
    post: (m: unknown, transfer: Transferable[]) => w.postMessage(m, transfer),
    onMessage: (f: (m: any) => void) => w.addEventListener("message", (e) => f(e.data)),
    terminate: () => w.terminate(),
  };
}));

const g = globalThis as any;
g.fractalstreamWorkers = pool;
// What kernels on this thread import (see Backend.Wasm).
g.fractalstreamMath = (await WebAssembly.instantiate(await fetchBytes("libm.wasm"))).instance.exports;
g.fractalstreamAutocolor = await WebAssembly.compile(await fetchBytes("autocolor.wasm"));

// What the engine posts.
g.fractalstreamPost = (json: string) => send(JSON.parse(json));
// (as RGBA, ready for an ImageData: done here, off the page's thread)
g.fractalstreamPostFrame = (viewer: number, rgb: Uint8Array, width: number, height: number,
                            cx: number, cy: number, px: number, py: number) => {
  const rgba = new Uint8ClampedArray(width * height * 4);
  for (let i = 0, j = 0; i < rgb.length; i += 3, j += 4) {
    rgba[j] = rgb[i]; rgba[j + 1] = rgb[i + 1]; rgba[j + 2] = rgb[i + 2]; rgba[j + 3] = 255;
  }
  const frame: Frame = { type: "frame", viewer, width, height, cx, cy, px, py, pixels: rgba.buffer };
  send(frame, [frame.pixels]);
};

g.fractalstreamPostFile = (name: string, bytes: Uint8Array) => {
  send({ type: "file", name, bytes: bytes.buffer as ArrayBuffer }, [bytes.buffer as ArrayBuffer]);
};

// The engine.
const wasi = new WASI(["fractalstream-web"], [], [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((line) => console.log(line)),
  ConsoleStdout.lineBuffered((line) => console.warn(line)),
], { debug: false });
const __exports: Record<string, any> = {};
const glue = await import(here("fractalstream-web.js").href);
const { instance } = await WebAssembly.instantiate(await fetchBytes("fractalstream-web.wasm"), {
  ghc_wasm_jsffi: glue.default(__exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(__exports, instance.exports);
wasi.initialize(instance as unknown as { exports: { memory: WebAssembly.Memory; _initialize: () => unknown } });
const engine = instance.exports as any;

scope.addEventListener("message", async (e: MessageEvent<Request>) => {
  const r = e.data;
  try {
    switch (r.type) {
      case "library": await engine.fs_library(); break;
      case "open": await engine.fs_open(r.section, r.entry); break;
      case "openFile": await engine.fs_open_file(r.name, new Uint8Array(r.bytes)); break;
      case "save": await engine.fs_save(); break;
      case "set": await engine.fs_set(r.name, r.text); break;
      case "view": await engine.fs_view(r.viewer, r.cx, r.cy, r.pixelSize, r.width, r.height); break;
      case "resize": await engine.fs_resize(r.viewer, r.width, r.height); break;
      case "smoothing": await engine.fs_smoothing(r.on); break;
    }
  } catch (err) {
    send({ type: "error", message: String(err) });
  }
});

send({ type: "ready" });
