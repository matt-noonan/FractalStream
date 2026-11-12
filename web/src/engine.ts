// The engine's worker: runs the engine (fractalstream-ui-web, a wasm
// reactor module) off the page's thread, with a pool of kernel workers
// for the wasm backend, and passes messages between it and the page
// (see protocol.ts).
//
// It does for the browser what dev-tools/wasm/run-node.mjs does for Node.

import { WASI, OpenFile, File, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import { createPool } from "./worker-pool.mjs";
import type { Request, Message, Patch } from "./protocol.js";

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
function makeWorker() {
  const w = new Worker(here("kernel-worker.mjs"), { type: "module" });
  return {
    post: (m: unknown, transfer: Transferable[]) => w.postMessage(m, transfer),
    onMessage: (f: (m: any) => void) => w.addEventListener("message", (e) => f(e.data)),
    terminate: () => w.terminate(),
  };
}
const pool = createPool(Array.from({ length: workerCount }, makeWorker), makeWorker);

const g = globalThis as any;
g.fractalstreamWorkers = pool;
// What kernels on this thread import (see Backend.Wasm).
g.fractalstreamMath = (await WebAssembly.instantiate(await fetchBytes("libm.wasm"))).instance.exports;
g.fractalstreamAutocolor = await WebAssembly.compile(await fetchBytes("autocolor.wasm"));

// What the engine posts.
g.fractalstreamPost = (json: string) => send(JSON.parse(json));
// (as RGBA, ready for an ImageData: done here, off the page's thread)
// A patch of a viewer's render (see Patch): the rectangles' pixels, read
// out of the engine's image (RGB, `width` pixels a row) as RGBA, one
// rectangle after another, row by row.
g.fractalstreamPostPatch = (viewer: number, id: number, width: number, height: number,
                            cx: number, cy: number, px: number, py: number,
                            image: Uint8Array, rects: Int32Array, done: boolean) => {
  let size = 0;
  for (let r = 0; r < rects.length; r += 4) size += rects[r + 2] * rects[r + 3] * 4;
  const rgba = new Uint8ClampedArray(size);
  let j = 0;
  for (let r = 0; r < rects.length; r += 4) {
    const [x, y, w, h] = [rects[r], rects[r + 1], rects[r + 2], rects[r + 3]];
    for (let row = y; row < y + h; row++) {
      for (let i = 3 * (row * width + x), end = i + 3 * w; i < end; i += 3, j += 4) {
        rgba[j] = image[i]; rgba[j + 1] = image[i + 1]; rgba[j + 2] = image[i + 2]; rgba[j + 3] = 255;
      }
    }
  }
  const patch: Patch = { type: "patch", viewer, id, width, height, cx, cy, px, py,
                         rects: Array.from(rects), pixels: rgba.buffer, done };
  send(patch, [patch.pixels]);
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
      case "translate": await engine.fs_translate(); break;
      case "pointer": await engine.fs_pointer(r.viewer, r.x, r.y); break;
      case "save": await engine.fs_save(r.views); break;
      case "rotate": await engine.fs_rotate(r.viewer, r.fx, r.fy, r.tx, r.ty); break;
      case "set": await engine.fs_set(r.name, r.text); break;
      case "view": await engine.fs_view(r.viewer, r.cx, r.cy, r.pixelSize, r.width, r.height, ...(r.covered ?? [0, 0, 0, 0])); break;
      case "resize": await engine.fs_resize(r.viewer, r.width, r.height); break;
      case "smoothing": await engine.fs_smoothing(r.on); break;
      case "density": await engine.fs_density(r.value); break;
      case "viewerSmoothing": await engine.fs_viewer_smoothing(r.viewer, r.on); break;
      case "navigate": await engine.fs_navigate(r.viewer, r.what); break;
      case "record": await engine.fs_record(r.viewer); break;
      case "jump": await engine.fs_jump(r.viewer, r.x, r.y, r.pixelSize); break;
      case "where": await engine.fs_where(r.viewer); break;
      case "colorizer": await engine.fs_colorizer(r.viewer, r.name); break;
      case "colorizerArguments": await engine.fs_colorizer_arguments(r.viewer); break;
      case "colorizerAnswers": await engine.fs_colorizer_answers(r.viewer, r.name, JSON.stringify(r.answers)); break;
      case "selectTool": await engine.fs_select_tool(r.viewer, r.name); break;
      case "event": await engine.fs_event(r.viewer, r.kind, r.x1, r.y1, r.x2, r.y2); break;
      case "button": await engine.fs_button(r.viewer, r.label); break;
      case "setTool": await engine.fs_set_tool(r.viewer, r.name, r.text); break;
      case "stop": await engine.fs_stop(r.viewer, r.navigate); break;
      case "analyze": await engine.fs_analyze(r.id, r.text); break;
      case "symbols": await engine.fs_symbols(); break;
      case "rebuild": await engine.fs_rebuild(r.text); break;
    }
  } catch (err) {
    send({ type: "error", message: String(err) });
  }
});

send({ type: "ready" });
