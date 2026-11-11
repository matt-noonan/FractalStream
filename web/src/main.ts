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
// A viewer: a canvas showing its latest frame. Dragging moves the view,
// and the wheel zooms about the pointer; the image moves at once, and the
// new frame replaces it when the engine has drawn it.

class ViewerView {
  readonly element: HTMLElement;
  private readonly canvas: HTMLCanvasElement;
  private frame: Frame | null = null;
  private drag: { x: number; y: number } | null = null;

  constructor(private readonly info: ViewerInfo) {
    this.canvas = el("canvas", { width: 1, height: 1 });
    this.element = el("figure", { className: "viewer" }, this.canvas, el("figcaption", { textContent: info.title }));
    this.canvas.addEventListener("pointerdown", (e) => this.startDrag(e));
    this.canvas.addEventListener("pointermove", (e) => this.moveDrag(e));
    this.canvas.addEventListener("pointerup", (e) => this.endDrag(e));
    this.canvas.addEventListener("wheel", (e) => this.zoom(e), { passive: false });
  }

  show(frame: Frame) {
    this.frame = frame;
    const { width, height } = frame;
    if (this.canvas.width !== width || this.canvas.height !== height) {
      this.canvas.width = width;
      this.canvas.height = height;
    }
    const rgb = new Uint8Array(frame.pixels);
    const rgba = new Uint8ClampedArray(width * height * 4);
    for (let i = 0, j = 0; i < rgb.length; i += 3, j += 4) {
      rgba[j] = rgb[i]; rgba[j + 1] = rgb[i + 1]; rgba[j + 2] = rgb[i + 2]; rgba[j + 3] = 255;
    }
    this.canvas.getContext("2d")!.putImageData(new ImageData(rgba, width, height), 0, 0);
    this.canvas.style.transform = "";
  }

  /** The pointer's position in the canvas's pixels. */
  private pixel(e: PointerEvent | WheelEvent) {
    const r = this.canvas.getBoundingClientRect();
    return { x: (e.clientX - r.left) * this.canvas.width / r.width,
             y: (e.clientY - r.top) * this.canvas.height / r.height };
  }

  private startDrag(e: PointerEvent) {
    if (!this.frame) return;
    this.canvas.setPointerCapture(e.pointerId);
    this.drag = this.pixel(e);
  }

  private moveDrag(e: PointerEvent) {
    if (!this.drag) return;
    const p = this.pixel(e);
    const r = this.canvas.getBoundingClientRect();
    const scale = r.width / this.canvas.width;
    this.canvas.style.transform = `translate(${(p.x - this.drag.x) * scale}px, ${(p.y - this.drag.y) * scale}px)`;
  }

  private endDrag(e: PointerEvent) {
    if (!this.drag || !this.frame) return;
    const p = this.pixel(e);
    const dx = p.x - this.drag.x, dy = p.y - this.drag.y;
    this.drag = null;
    if (dx === 0 && dy === 0) return;
    const f = this.frame;
    this.request(f.cx - dx * f.px, f.cy + dy * f.py, f.px);
  }

  private zoom(e: WheelEvent) {
    e.preventDefault();
    if (!this.frame) return;
    const f = this.frame;
    const factor = Math.exp(e.deltaY * 0.002);
    const p = this.pixel(e);
    // the model point under the pointer stays where it is
    const mx = f.cx + (p.x - f.width / 2) * f.px;
    const my = f.cy - (p.y - f.height / 2) * f.py;
    const px = f.px * factor, py = f.py * factor;
    this.request(mx - (p.x - f.width / 2) * px, my + (p.y - f.height / 2) * py, px);
    // (until the new frame comes, the old one, scaled about the pointer)
    this.frame = { ...f, cx: mx - (p.x - f.width / 2) * px, cy: my + (p.y - f.height / 2) * py, px, py };
    const r = this.canvas.getBoundingClientRect();
    const scale = r.width / this.canvas.width;
    const s = this.currentScale() / factor;
    this.canvas.style.transformOrigin = `${p.x * scale}px ${p.y * scale}px`;
    this.canvas.style.transform = `scale(${s})`;
  }

  private currentScale(): number {
    const m = /scale\(([^)]+)\)/.exec(this.canvas.style.transform);
    return m ? Number(m[1]) : 1;
  }

  private request(cx: number, cy: number, pixelSize: number) {
    send({ type: "view", viewer: this.info.id, cx, cy, pixelSize });
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
