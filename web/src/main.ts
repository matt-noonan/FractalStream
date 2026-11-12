// The page: the library, then a project's configuration panel and
// viewers. The engine runs in a worker (engine.ts); see protocol.ts for
// what they say to each other.

import type { Request, Message, Layout, FieldLayout, Frame, LibrarySection, ViewerInfo } from "./protocol.js";
import { pickFile, droppedFile, chooseDestination, type Opened, type FileHandle } from "./files.js";
import { Desk } from "./windows.js";

const engine = new Worker(new URL("engine.js", import.meta.url), { type: "module" });
const send = (r: Request) => engine.postMessage(r);

const libraryEl = document.getElementById("library")!;
const projectEl = document.getElementById("project")!;
const statusEl = document.getElementById("status")!;
const openButton = document.getElementById("open") as HTMLButtonElement;
const saveButton = document.getElementById("save") as HTMLButtonElement;
const saveAsButton = document.getElementById("save-as") as HTMLButtonElement;
const fileNameEl = document.getElementById("file-name")!;
const libraryButton = document.getElementById("toggle-library") as HTMLButtonElement;
const panelButton = document.getElementById("toggle-panel") as HTMLButtonElement;

// Hiding and showing the library, and the configuration panel.
function toggle(button: HTMLButtonElement, bodyClass: string) {
  const hidden = document.body.classList.toggle(bodyClass);
  button.setAttribute("aria-pressed", String(!hidden));
}
libraryButton.addEventListener("click", () => toggle(libraryButton, "no-library"));
panelButton.addEventListener("click", () => toggle(panelButton, "no-panel"));

function el<K extends keyof HTMLElementTagNameMap>(tag: K, props: Partial<HTMLElementTagNameMap[K]> = {},
                                                 ...children: (Node | string)[]): HTMLElementTagNameMap[K] {
  const e = Object.assign(document.createElement(tag), props);
  e.append(...children);
  return e;
}

function status(text: string, isError = false) {
  statusEl.textContent = text;
  statusEl.classList.toggle("error", isError);
}

// ---------------------------------------------------------------------
// The library

function showLibrary(sections: LibrarySection[]) {
  libraryEl.replaceChildren(...sections.map((s, si) =>
    el("section", {},
      el("h2", { textContent: s.name }),
      el("ul", {}, ...s.entries.map((e, ei) => {
        const item = el("li", { title: e.description },
          e.preview ? el("img", { src: `data:image/png;base64,${e.preview}`, alt: "" }) : "",
          el("span", { textContent: e.title }));
        item.addEventListener("click", () => {
          deselectLibrary();
          item.classList.add("selected");
          status(`Opening ${e.title}…`);
          send({ type: "open", section: si, entry: ei });
        });
        return item;
      })))));
}

function deselectLibrary() {
  for (const li of libraryEl.querySelectorAll("li.selected")) li.classList.remove("selected");
}

// ---------------------------------------------------------------------
// Files

/** The open project's file: its name, whether it's from the library, and
    where to save it back (if the browser allows that). */
let current: { fileName: string; library: boolean; handle: FileHandle | null } | null = null;
/** The handle of the file being opened, until its project comes. */
let opening: FileHandle | null = null;
/** Where the next saved file goes (see "file" messages). */
let pendingSave: ((name: string, bytes: ArrayBuffer) => Promise<string>) | null = null;

async function openFile(opened: Opened | null) {
  if (!opened) return;
  opening = opened.handle;
  deselectLibrary();
  status(`Opening ${opened.file.name}…`);
  const bytes = await opened.file.arrayBuffer();
  engine.postMessage({ type: "openFile", name: opened.file.name, bytes } satisfies Request, [bytes]);
}

async function save(saveAs: boolean) {
  if (!current) return;
  const c = current;
  try {
    const destination = await chooseDestination(c.library ? null : c.handle, saveAs, c.fileName, (handle) => {
      c.handle = handle;
      c.library = false;
      c.fileName = handle.name;
      showFileName();
    });
    if (!destination) return;
    pendingSave = destination;
    send({ type: "save" });
  } catch (e) {
    status(`Couldn't save: ${e}`, true);
  }
}

function showFileName() {
  fileNameEl.textContent = current ? current.fileName + (current.library ? " (from the library)" : "") : "";
  saveButton.disabled = saveAsButton.disabled = !current;
}

openButton.addEventListener("click", async () => openFile(await pickFile()));
saveButton.addEventListener("click", () => save(false));
saveAsButton.addEventListener("click", () => save(true));
document.addEventListener("keydown", async (e) => {
  if (!(e.metaKey || e.ctrlKey)) return;
  const key = e.key.toLowerCase();
  if (key === "o") { e.preventDefault(); openFile(await pickFile()); }
  else if (key === "s") { e.preventDefault(); save(e.shiftKey); }
});
document.addEventListener("dragover", (e) => { e.preventDefault(); });
document.addEventListener("drop", async (e) => { e.preventDefault(); openFile(await droppedFile(e)); });

// ---------------------------------------------------------------------
// A project

const fieldInputs = new Map<string, { input: HTMLInputElement | HTMLSelectElement; error: HTMLElement }>();
const viewers = new Map<number, ViewerView>();

function showProject(title: string, panel: Layout, infos: ViewerInfo[]) {
  fieldInputs.clear();
  viewers.clear();
  const desk = new Desk(el("div"));
  projectEl.replaceChildren(
    el("h1", { textContent: title }),
    el("div", { className: "workspace" },
      desk.element,
      panel ? el("aside", { className: "panel" }, layoutElement(panel)) : ""));
  // Each viewer in a window, side by side: all scaled down alike if they
  // don't fit (each still showing its whole view), to no less than 40%,
  // and then onto more rows if they still don't fit.
  const gap = 16, margin = 12;
  const room = desk.element.clientWidth - 2 * margin;
  // (the gaps between them don't scale)
  const widths = infos.reduce((sum, v) => sum + v.width, 0);
  const scale = Math.min(1, Math.max(0.4, (room - gap * (infos.length - 1)) / widths));
  let x = margin, y = margin, rowHeight = 0;
  for (const info of infos) {
    const w = Math.floor(info.width * scale), h = Math.floor(info.height * scale);
    if (x > margin && x + w > margin + room + 1) { x = margin; y += rowHeight + gap; rowHeight = 0; }
    const v = new ViewerView(info, w, h);
    viewers.set(info.id, v);
    desk.add({ title: info.title, x, y, width: w, height: h, content: v.element,
               onResize: (width, height) => v.resize(width, height) });
    x += w + gap;
    rowHeight = Math.max(rowHeight, h + 30);
  }
  status("");
}

function layoutElement(layout: Layout): Node {
  if (layout === null) return document.createTextNode("");
  switch (layout.kind) {
    case "vertical":
    case "horizontal":
      return el("div", { className: layout.kind }, ...layout.children.map(layoutElement));
    case "panel":
      return el("fieldset", {}, el("legend", { textContent: layout.title }), layoutElement(layout.body));
    case "tabs":
      return tabsElement(layout.tabs);
    case "plain":
      return el("p", { textContent: layout.text });
    default:
      return fieldElement(layout);
  }
}

function tabsElement(tabs: { label: string; body: Layout }[]): Node {
  const bodies = tabs.map((t) => el("div", { className: "tab-body" }, layoutElement(t.body)));
  const buttons = tabs.map((t, i) => {
    const b = el("button", { textContent: t.label, type: "button" });
    b.addEventListener("click", () => select(i));
    return b;
  });
  const select = (i: number) => {
    bodies.forEach((b, k) => (b.hidden = k !== i));
    buttons.forEach((b, k) => b.classList.toggle("selected", k === i));
  };
  select(0);
  return el("div", { className: "tabs" }, el("div", { className: "tab-buttons" }, ...buttons), ...bodies);
}

function fieldElement(f: FieldLayout): Node {
  const error = el("span", { className: "field-error" });
  let input: HTMLInputElement | HTMLSelectElement;
  switch (f.kind) {
    case "check": {
      const box = el("input", { type: "checkbox", checked: f.text === "true" });
      box.addEventListener("change", () => send({ type: "set", name: f.name, text: box.checked ? "true" : "false" }));
      input = box;
      break;
    }
    case "select": {
      const select = el("select", {}, ...(f.options ?? []).map((o) => el("option", { value: o, textContent: o })));
      select.value = f.text;
      select.addEventListener("change", () => send({ type: "set", name: f.name, text: select.value }));
      input = select;
      break;
    }
    default: {
      // (a text box, or for now a color, as its text)
      const box = el("input", { type: "text", value: f.text, spellcheck: false });
      let committed = f.text;
      const commit = () => {
        if (box.value !== committed) {
          committed = box.value;
          send({ type: "set", name: f.name, text: box.value });
        }
      };
      box.addEventListener("keydown", (e) => { if (e.key === "Enter") commit(); });
      box.addEventListener("blur", commit);
      input = box;
    }
  }
  fieldInputs.set(f.name, { input, error });
  return el("label", { className: `field ${f.kind}` }, el("span", { textContent: f.label }), input, error);
}

function fieldResult(name: string, error: string | null) {
  const f = fieldInputs.get(name);
  if (!f) return;
  f.input.classList.toggle("invalid", error !== null);
  f.error.textContent = error ? "⚠" : "";
  f.error.title = error ?? "";
}

// ---------------------------------------------------------------------
// A viewer: a canvas showing its latest frame, in the view the page wants.
//
// The page keeps the view it wants (the target: a center, and a pixel's
// size), which dragging and the wheel change at once, and always draws
// the latest frame where it belongs in that view (each frame says what
// view it shows): a frame for an older view is just drawn a little moved
// or scaled, until a frame for the new one replaces it. So the picture
// never jumps back. The engine is asked for one view at a time: when a
// frame comes, if the target has moved on since, it's asked for the
// target.

interface View { cx: number; cy: number; px: number; py: number }

const sameView = (a: View, b: View) => {
  const close = (x: number, y: number) => Math.abs(x - y) <= 1e-12 * Math.max(Math.abs(x), Math.abs(y), 1e-300);
  return close(a.cx, b.cx) && close(a.cy, b.cy) && close(a.px, b.px) && close(a.py, b.py);
};

/** How many canvas pixels to a CSS pixel: the screen's, up to 2 (each
    doubling is four times the rendering). */
const density = () => Math.min(2, Math.max(1, Math.round(window.devicePixelRatio || 1)));

class ViewerView {
  readonly element: HTMLElement;
  private readonly canvas: HTMLCanvasElement;
  private readonly density = density();
  private readonly context: CanvasRenderingContext2D;
  /** The latest frame, as an image to draw from. */
  private frame: { view: View; image: HTMLCanvasElement } | null = null;
  private target: View | null = null;
  private requested: { view: View; at: number } | null = null;
  private drawPending = false;
  private dragFrom: { x: number; y: number } | null = null;

  constructor(private readonly info: ViewerInfo, width: number, height: number) {
    this.canvas = el("canvas");
    this.context = this.canvas.getContext("2d")!;
    this.element = el("div", { className: "viewer" }, this.canvas);
    this.setSize(width, height);
    this.canvas.addEventListener("pointerdown", (e) => this.startDrag(e));
    this.canvas.addEventListener("pointermove", (e) => this.moveDrag(e));
    this.canvas.addEventListener("pointerup", () => (this.dragFrom = null));
    this.canvas.addEventListener("pointercancel", () => (this.dragFrom = null));
    this.canvas.addEventListener("wheel", (e) => this.zoom(e), { passive: false });
  }

  /** Show it at this size (in CSS pixels): the canvas has `density` times
      as many pixels each way, and the view keeps its center and scale. */
  private setSize(width: number, height: number) {
    this.canvas.width = Math.round(width * this.density);
    this.canvas.height = Math.round(height * this.density);
    this.canvas.style.width = `${width}px`;
    this.canvas.style.height = `${height}px`;
  }

  resize(width: number, height: number) {
    this.setSize(width, height);
    this.redraw();
    this.request();
  }

  show(frame: Frame) {
    const image = el("canvas", { width: frame.width, height: frame.height });
    image.getContext("2d")!.putImageData(
      new ImageData(new Uint8ClampedArray(frame.pixels), frame.width, frame.height), 0, 0);
    const view = { cx: frame.cx, cy: frame.cy, px: frame.px, py: frame.py };
    this.frame = { view, image };
    // (how many frames it's had: for tests, and anyone curious)
    this.element.dataset.frames = String(Number(this.element.dataset.frames ?? 0) + 1);
    if (!this.target) {
      // (the first frame, at the project's size: the same picture across
      // the canvas, whatever its size)
      const scale = frame.width / this.canvas.width;
      this.target = { cx: view.cx, cy: view.cy, px: view.px * scale, py: view.py * scale };
    }
    // (whichever request this answers, the engine is free again)
    this.requested = null;
    this.redraw();
    this.request();
  }

  /** Draw the latest frame in the target view (once per display frame). */
  private redraw() {
    if (this.drawPending) return;
    this.drawPending = true;
    requestAnimationFrame(() => {
      this.drawPending = false;
      const f = this.frame, t = this.target;
      if (!f || !t) return;
      const W = this.canvas.width, H = this.canvas.height;
      const fw = f.image.width, fh = f.image.height;
      // where the frame's corners are, in the target view's pixels
      const left = (f.view.cx - (fw / 2) * f.view.px - t.cx) / t.px + W / 2;
      const top = (t.cy - (f.view.cy + (fh / 2) * f.view.py)) / t.py + H / 2;
      const ctx = this.context;
      ctx.fillStyle = "#808080";
      ctx.fillRect(0, 0, W, H);
      ctx.drawImage(f.image, left, top, (fw * f.view.px) / t.px, (fh * f.view.py) / t.py);
    });
  }

  /** Ask the engine for the target view, at the canvas's size, unless it's
      busy (or has it). */
  private request() {
    const t = this.target, f = this.frame;
    if (!t || !f) return;
    // (if an answer never came, ask again after a while)
    if (this.requested && performance.now() - this.requested.at < 2000) return;
    const width = this.canvas.width, height = this.canvas.height;
    if (sameView(t, f.view) && f.image.width === width && f.image.height === height) return;
    this.requested = { view: { ...t }, at: performance.now() };
    send({ type: "view", viewer: this.info.id, cx: t.cx, cy: t.cy, pixelSize: t.px, width, height });
  }

  /** The pointer's position in the canvas's pixels. */
  private pixel(e: PointerEvent | WheelEvent) {
    const r = this.canvas.getBoundingClientRect();
    return { x: (e.clientX - r.left) * this.canvas.width / r.width,
             y: (e.clientY - r.top) * this.canvas.height / r.height };
  }

  private startDrag(e: PointerEvent) {
    if (!this.target) return;
    this.canvas.setPointerCapture(e.pointerId);
    this.dragFrom = this.pixel(e);
  }

  private moveDrag(e: PointerEvent) {
    const t = this.target;
    if (!this.dragFrom || !t) return;
    const p = this.pixel(e);
    t.cx -= (p.x - this.dragFrom.x) * t.px;
    t.cy += (p.y - this.dragFrom.y) * t.py;
    this.dragFrom = p;
    this.redraw();
    this.request();
  }

  private zoom(e: WheelEvent) {
    e.preventDefault();
    const t = this.target;
    if (!t) return;
    // (a line or a page of scrolling counts as more than a pixel)
    const delta = e.deltaY * (e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? 400 : 1);
    const factor = Math.exp(delta * 0.002);
    const p = this.pixel(e);
    const W = this.canvas.width, H = this.canvas.height;
    // the model point under the pointer stays where it is
    const mx = t.cx + (p.x - W / 2) * t.px;
    const my = t.cy - (p.y - H / 2) * t.py;
    t.px *= factor;
    t.py *= factor;
    t.cx = mx - (p.x - W / 2) * t.px;
    t.cy = my + (p.y - H / 2) * t.py;
    this.redraw();
    this.request();
  }
}

// ---------------------------------------------------------------------

engine.addEventListener("message", (e: MessageEvent<Message>) => {
  const m = e.data;
  switch (m.type) {
    case "ready": status("Loading the library…"); send({ type: "library" }); break;
    case "library": showLibrary(m.sections); status("Pick a project."); break;
    case "project":
      current = { fileName: m.fileName, library: m.library, handle: m.library ? null : opening };
      opening = null;
      showFileName();
      showProject(m.title, m.panel, m.viewers);
      break;
    case "file": {
      const write = pendingSave;
      pendingSave = null;
      if (write) write(m.name, m.bytes).then((name) => status(`Saved ${name}.`),
                                              (e) => status(`Couldn't save: ${e}`, true));
      break;
    }
    case "field": fieldResult(m.name, m.error); break;
    case "error": status(m.message, true); console.error(m.message); break;
    case "frame": viewers.get(m.viewer)?.show(m); break;
  }
});
engine.addEventListener("error", (e) => status(`The engine failed: ${e.message}`, true));
status("Starting the engine…");
