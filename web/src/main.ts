// The page: the library, then a project's configuration panel and
// viewers. The engine runs in a worker (engine.ts); see protocol.ts for
// what they say to each other.

import type { Request, Message, Layout, FieldLayout, Frame, LibrarySection, ViewerInfo } from "./protocol.js";

const engine = new Worker(new URL("engine.js", import.meta.url), { type: "module" });
const send = (r: Request) => engine.postMessage(r);

const libraryEl = document.getElementById("library")!;
const projectEl = document.getElementById("project")!;
const statusEl = document.getElementById("status")!;

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
          for (const li of libraryEl.querySelectorAll("li.selected")) li.classList.remove("selected");
          item.classList.add("selected");
          status(`Opening ${e.title}…`);
          send({ type: "open", section: si, entry: ei });
        });
        return item;
      })))));
}

// ---------------------------------------------------------------------
// A project

const fieldInputs = new Map<string, { input: HTMLInputElement | HTMLSelectElement; error: HTMLElement }>();
const viewers = new Map<number, ViewerView>();

function showProject(title: string, panel: Layout, infos: ViewerInfo[]) {
  fieldInputs.clear();
  viewers.clear();
  const viewerEls = infos.map((info) => {
    const v = new ViewerView(info);
    viewers.set(info.id, v);
    return v.element;
  });
  projectEl.replaceChildren(
    el("h1", { textContent: title }),
    el("div", { className: "workspace" },
      el("div", { className: "viewers" }, ...viewerEls),
      panel ? el("div", { className: "panel" }, layoutElement(panel)) : ""));
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

class ViewerView {
  readonly element: HTMLElement;
  private readonly canvas: HTMLCanvasElement;
  private readonly context: CanvasRenderingContext2D;
  /** The latest frame, as an image to draw from. */
  private frame: { view: View; image: HTMLCanvasElement } | null = null;
  private target: View | null = null;
  private requested: { view: View; at: number } | null = null;
  private drawPending = false;
  private dragFrom: { x: number; y: number } | null = null;

  constructor(private readonly info: ViewerInfo) {
    this.canvas = el("canvas", { width: 1, height: 1 });
    this.context = this.canvas.getContext("2d")!;
    this.element = el("figure", { className: "viewer" }, this.canvas, el("figcaption", { textContent: info.title }));
    this.canvas.addEventListener("pointerdown", (e) => this.startDrag(e));
    this.canvas.addEventListener("pointermove", (e) => this.moveDrag(e));
    this.canvas.addEventListener("pointerup", () => (this.dragFrom = null));
    this.canvas.addEventListener("pointercancel", () => (this.dragFrom = null));
    this.canvas.addEventListener("wheel", (e) => this.zoom(e), { passive: false });
  }

  show(frame: Frame) {
    const image = el("canvas", { width: frame.width, height: frame.height });
    image.getContext("2d")!.putImageData(
      new ImageData(new Uint8ClampedArray(frame.pixels), frame.width, frame.height), 0, 0);
    const view = { cx: frame.cx, cy: frame.cy, px: frame.px, py: frame.py };
    this.frame = { view, image };
    if (!this.target) {
      this.target = { ...view };
      this.canvas.width = frame.width;
      this.canvas.height = frame.height;
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

  /** Ask the engine for the target view, unless it's busy (or has it). */
  private request() {
    const t = this.target, f = this.frame;
    if (!t || !f) return;
    // (if an answer never came, ask again after a while)
    if (this.requested && performance.now() - this.requested.at < 2000) return;
    if (sameView(t, f.view)) return;
    this.requested = { view: { ...t }, at: performance.now() };
    send({ type: "view", viewer: this.info.id, cx: t.cx, cy: t.cy, pixelSize: t.px });
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
    case "project": showProject(m.title, m.panel, m.viewers); break;
    case "field": fieldResult(m.name, m.error); break;
    case "error": status(m.message, true); console.error(m.message); break;
    case "frame": viewers.get(m.viewer)?.show(m); break;
  }
});
engine.addEventListener("error", (e) => status(`The engine failed: ${e.message}`, true));
status("Starting the engine…");
