// The page: the library, then a project's configuration panel and
// viewers. The engine runs in a worker (engine.ts); see protocol.ts for
// what they say to each other.

import type { Request, Message, Layout, FieldLayout, Frame, LibrarySection, ViewerInfo, SavedViews,
              Shape, Handled, EventKind } from "./protocol.js";
import { drawLayers } from "./drawing.js";
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
const paletteEl = document.getElementById("tools")!;

// Hiding and showing the library, and the configuration panel.
function toggle(button: HTMLButtonElement, bodyClass: string) {
  const hidden = document.body.classList.toggle(bodyClass);
  button.setAttribute("aria-pressed", String(!hidden));
}
libraryButton.addEventListener("click", () => toggle(libraryButton, "no-library"));
panelButton.addEventListener("click", () => toggle(panelButton, "no-panel"));

// Smoothing: remembered (in this browser) once chosen; until then, off on
// a high-DPI screen (which is drawn at twice the resolution already), and
// on otherwise.
const smoothingBox = document.getElementById("smoothing") as HTMLInputElement;
const SMOOTHING_KEY = "fractalstream.smoothing";
function savedSmoothing(): boolean | null {
  try {
    const v = localStorage.getItem(SMOOTHING_KEY);
    return v === null ? null : v === "true";
  } catch { return null; }
}
smoothingBox.checked = savedSmoothing() ?? (window.devicePixelRatio || 1) < 1.5;
smoothingBox.addEventListener("change", () => {
  try { localStorage.setItem(SMOOTHING_KEY, String(smoothingBox.checked)); } catch { /* (not remembered) */ }
  send({ type: "smoothing", on: smoothingBox.checked });
});

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
    send({ type: "save", views: JSON.stringify(viewState()) });
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

/** Each field's input, by its key (see FieldLayout). */
const fieldInputs = new Map<string, { input: HTMLInputElement | HTMLSelectElement; error: HTMLElement;
                                      swatch?: HTMLInputElement; setText: (text: string) => void }>();
const viewers = new Map<number, ViewerView>();
/** The viewer the tool palette, its shortcuts and the tool's panel are
    for: the one last clicked. */
let activeViewer: ViewerView | null = null;
/** Where the active viewer's tool's panel goes (in the side panel). */
let toolPanelEl: HTMLElement | null = null;
let asideEl: HTMLElement | null = null;
/** The open project's viewers' windows, for saving the view state. */
let windows: { title: string; view: ViewerView; window: HTMLElement }[] = [];

function viewState(): SavedViews {
  return {
    viewers: Object.fromEntries(windows.map((w) =>
      [w.title, { x: w.window.offsetLeft, y: w.window.offsetTop, ...w.view.state() }])),
  };
}

function showProject(title: string, panel: Layout, infos: ViewerInfo[], views: SavedViews | null) {
  fieldInputs.clear();
  viewers.clear();
  windows = [];
  activeViewer = null;
  const desk = new Desk(el("div"));
  toolPanelEl = el("div", { className: "tool-panel" });
  asideEl = el("aside", { className: "panel" },
               panelGrip(), panel ? layoutElement(panel, projectFields) : "", toolPanelEl);
  projectEl.replaceChildren(
    el("h1", { textContent: title }),
    el("div", { className: "workspace" }, desk.element, asideEl));
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
    // (where it was saved, if it was)
    const saved = views?.viewers?.[info.title];
    const place = saved ? { x: saved.x, y: saved.y, width: saved.width, height: saved.height }
                        : { x, y, width: w, height: h };
    const v = new ViewerView(info, place.width, place.height, saved ?? null);
    viewers.set(info.id, v);
    const win = desk.add({ title: info.title, ...place, content: v.element,
                           onResize: (width, height) => v.resize(width, height) });
    win.addEventListener("pointerdown", () => activate(v), { capture: true });
    v.window = win;
    windows.push({ title: info.title, view: v, window: win });
    x += w + gap;
    rowHeight = Math.max(rowHeight, h + 30);
  }
  activate(windows[0]?.view ?? null);
  status("");
}

/** Make a viewer the active one: the palette, and its tool's panel, are its. */
function activate(v: ViewerView | null) {
  if (activeViewer === v) return;
  activeViewer?.window?.classList.remove("active");
  activeViewer = v;
  v?.window?.classList.toggle("active", viewers.size > 1);
  showPalette();
  showToolPanel();
}

/** The tool palette, in the toolbar: Navigate, and the active viewer's tools. */
function showPalette() {
  const v = activeViewer;
  if (!v) { paletteEl.replaceChildren(); return; }
  const button = (name: string | null, label: string, help: string, shortcut: string) => {
    const b = el("button", { type: "button", textContent: label,
                             title: help + (shortcut ? ` (${shortcut})` : "") });
    b.setAttribute("aria-pressed", String(v.toolName === name));
    b.addEventListener("click", () => v.selectTool(name));
    return b;
  };
  paletteEl.replaceChildren(
    button(null, "Navigate", "Drag to move around; scroll or pinch to zoom", "n"),
    ...v.info.tools.map((t) => button(t.name, t.name, t.help, t.shortcut)));
}

function showToolPanel() {
  if (!toolPanelEl || !asideEl) return;
  toolPanelEl.replaceChildren(activeViewer?.toolPanel ?? "");
  // (no side panel at all, if there's nothing to put in it)
  asideEl.hidden = asideEl.querySelector("fieldset, .field, button, p, .tabs") === null;
}

// A tool's shortcut key (and n for Navigate) picks it, for the active viewer.
document.addEventListener("keydown", (e) => {
  const v = activeViewer;
  if (!v || e.metaKey || e.ctrlKey || e.altKey) return;
  if (e.target instanceof HTMLElement && e.target.closest("input, select, textarea")) return;
  const key = e.key.toLowerCase();
  const tool = v.info.tools.find((t) => t.shortcut && t.shortcut.toLowerCase() === key);
  if (tool) { e.preventDefault(); v.selectTool(tool.name); }
  else if (key === "n") { e.preventDefault(); v.selectTool(null); }
});

// The configuration panel's width: dragged from its left edge, and
// remembered (in this browser).
const PANEL_WIDTH_KEY = "fractalstream.panelWidth";
const MIN_PANEL_WIDTH = 240;
function setPanelWidth(w: number) {
  const width = Math.round(Math.max(MIN_PANEL_WIDTH, Math.min(w, window.innerWidth * 0.7)));
  document.documentElement.style.setProperty("--panel-width", `${width}px`);
  return width;
}
try {
  const saved = Number(localStorage.getItem(PANEL_WIDTH_KEY));
  if (saved > 0) setPanelWidth(saved);
} catch { /* (the default width) */ }

function panelGrip(): HTMLElement {
  const grip = el("div", { className: "panel-grip", title: "Drag to make the panel wider or narrower" });
  grip.addEventListener("pointerdown", (e) => {
    if (e.button !== 0 || !asideEl) return;
    e.preventDefault();
    grip.setPointerCapture(e.pointerId);
    grip.classList.add("dragging");
    const x0 = e.clientX, w0 = asideEl.getBoundingClientRect().width;
    let width = w0;
    const move = (m: PointerEvent) => { width = setPanelWidth(w0 - (m.clientX - x0)); };
    const up = () => {
      grip.classList.remove("dragging");
      grip.removeEventListener("pointermove", move);
      grip.removeEventListener("pointerup", up);
      grip.removeEventListener("pointercancel", up);
      try { localStorage.setItem(PANEL_WIDTH_KEY, String(width)); } catch { /* (not remembered) */ }
    };
    grip.addEventListener("pointermove", move);
    grip.addEventListener("pointerup", up);
    grip.addEventListener("pointercancel", up);
  });
  return grip;
}

/** What a panel's fields and buttons do: set a field, press a button. */
interface PanelActions {
  set(name: string, text: string): void;
  button?(label: string): void;
}
const projectFields: PanelActions = { set: (name, text) => send({ type: "set", name, text }) };

/** A layout's elements. (`title` is the panel it's in's: a panel inside
    it with the same title, as a tool's panel has, doesn't repeat it.) */
function layoutElement(layout: Layout, actions: PanelActions, title?: string): Node {
  if (layout === null) return document.createTextNode("");
  switch (layout.kind) {
    case "vertical":
    case "horizontal":
      return el("div", { className: layout.kind }, ...layout.children.map((c) => layoutElement(c, actions, title)));
    case "panel":
      return el("fieldset", {}, layout.title === title ? "" : el("legend", { textContent: layout.title }),
                layoutElement(layout.body, actions, layout.title));
    case "tabs":
      return tabsElement(layout.tabs, actions);
    case "plain":
      return el("p", { textContent: layout.text });
    case "button": {
      const b = el("button", { type: "button", textContent: layout.label });
      b.addEventListener("click", () => actions.button?.(layout.label));
      return b;
    }
    default:
      return fieldElement(layout, actions);
  }
}

/** Tabs: a chooser of which one to show, and the one chosen. */
function tabsElement(tabs: { label: string; body: Layout }[], actions: PanelActions): Node {
  const bodies = tabs.map((t) => el("div", { className: "tab-body" }, layoutElement(t.body, actions)));
  const chooser = el("select", { className: "tab-chooser", title: "Which settings to show" },
                     ...tabs.map((t, i) => el("option", { value: String(i), textContent: t.label })));
  const select = (i: number) => bodies.forEach((b, k) => (b.hidden = k !== i));
  chooser.addEventListener("change", () => select(Number(chooser.value)));
  select(0);
  return el("div", { className: "tabs" }, chooser, ...bodies);
}

function fieldElement(f: FieldLayout, actions: PanelActions): Node {
  const error = el("span", { className: "field-error" });
  let input: HTMLInputElement | HTMLSelectElement;
  let setText: (text: string) => void;
  switch (f.kind) {
    case "check": {
      const box = el("input", { type: "checkbox", checked: f.text === "true" });
      box.addEventListener("change", () => actions.set(f.name, box.checked ? "true" : "false"));
      input = box;
      setText = (text) => (box.checked = text === "true");
      break;
    }
    case "select": {
      const select = el("select", {}, ...(f.options ?? []).map((o) => el("option", { value: o, textContent: o })));
      select.value = f.text;
      select.addEventListener("change", () => actions.set(f.name, select.value));
      input = select;
      setText = (text) => (select.value = text);
      break;
    }
    default: {
      // (a text box; for a color, with a swatch to pick one with)
      // (the whole value as a tooltip too, for one too long for the box)
      const box = el("input", { type: "text", value: f.text, spellcheck: false, title: f.text });
      box.addEventListener("input", () => (box.title = box.value));
      let committed = f.text;
      const commit = () => {
        if (box.value !== committed) {
          committed = box.value;
          actions.set(f.name, box.value);
        }
      };
      box.addEventListener("keydown", (e) => { if (e.key === "Enter") commit(); });
      box.addEventListener("blur", commit);
      input = box;
      setText = (text) => { committed = text; box.value = text; box.title = text; };
      if (f.kind === "color") {
        const swatch = el("input", { type: "color", value: f.color ?? "#000000", title: "Pick a color" });
        swatch.addEventListener("change", () => { box.value = box.title = rgbText(swatch.value); commit(); });
        fieldInputs.set(f.key, { input, error, swatch, setText });
        return el("label", { className: "field color" }, el("span", { textContent: f.label }), swatch, input, error);
      }
    }
  }
  fieldInputs.set(f.key, { input, error, setText });
  return el("label", { className: `field ${f.kind}` }, el("span", { textContent: f.label }), input, error);
}

/** #rrggbb, as the scripting language writes a color. */
function rgbText(hex: string): string {
  const c = (k: number) => (parseInt(hex.slice(1 + 2 * k, 3 + 2 * k), 16) / 255).toFixed(3);
  return `rgb(${c(0)}, ${c(1)}, ${c(2)})`;
}

/** A tool's handler set a field. */
function fieldText(key: string, text: string, color?: string | null) {
  const f = fieldInputs.get(key);
  if (!f) return;
  f.setText(text);
  if (f.swatch && color) f.swatch.value = color;
  f.input.classList.remove("invalid");
  f.error.textContent = "";
}

function fieldResult(key: string, error: string | null, color?: string | null) {
  const f = fieldInputs.get(key);
  if (!f) return;
  if (f.swatch && color) f.swatch.value = color;
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
  /** On the sphere: the screen point (in the sphere's radii) where the
      globe was last turned from, and where the pointer is now. */
  private turn: { from: [number, number]; to: [number, number] } | null = null;
  private readonly errorEl: HTMLElement;
  /** Shown while its tool's handler (or overlay) runs for a while: what's
      running, for how long, and buttons to stop it. */
  private readonly runEl: HTMLElement;
  private runTimer: number | undefined;
  /** Its window, on the desk. */
  window: HTMLElement | null = null;

  /** Its tool (null: navigating), the pointer events that tool handles,
      and its panel. */
  toolName: string | null = null;
  private handles = new Set<Handled>();
  toolPanel: HTMLElement | null = null;
  /** What its tools have drawn, in screen-plane coordinates. */
  private layers: Shape[][] = [];
  /** The pointer, from when the button went down: where (in canvas
      pixels), and whether it's moved far enough to be a drag. */
  private press: { start: { x: number; y: number }; dragging: boolean } | null = null;
  /** Pointer events for the tool, waiting to be sent: one at a time, and
      only the latest of a run of drags (so a slow handler doesn't fall
      behind the pointer). */
  private events: Extract<Request, { type: "event" }>[] = [];
  private eventSent = 0;

  constructor(readonly info: ViewerInfo, width: number, height: number,
              private readonly saved: { cx: number; cy: number; "pixel-size": number } | null) {
    this.canvas = el("canvas");
    this.context = this.canvas.getContext("2d")!;
    this.errorEl = el("div", { className: "viewer-error", hidden: true });
    this.runEl = el("div", { className: "viewer-running", hidden: true, role: "status" });
    this.element = el("div", { className: "viewer" }, this.canvas, this.errorEl, this.runEl);
    this.setSize(width, height);
    this.canvas.addEventListener("pointerdown", (e) => this.startDrag(e));
    this.canvas.addEventListener("pointermove", (e) => this.moveDrag(e));
    this.canvas.addEventListener("pointerup", (e) => this.endDrag(e));
    this.canvas.addEventListener("pointercancel", () => { this.dragFrom = null; this.press = null; });
    this.canvas.addEventListener("dblclick", (e) => {
      if (this.handles.has("doubleClick")) this.toolEvent(1, this.pixel(e));
    });
    this.canvas.addEventListener("wheel", (e) => this.wheel(e), { passive: false });
    // (Safari's pinches: gesture events, with the scale since the pinch began)
    let lastScale = 1;
    this.canvas.addEventListener("gesturestart", (e) => { e.preventDefault(); lastScale = 1; });
    this.canvas.addEventListener("gesturechange", (e) => {
      e.preventDefault();
      const g = e as unknown as { scale: number; clientX: number; clientY: number };
      if (!(g.scale > 0)) return;
      this.zoomBy(lastScale / g.scale, this.pixel(g));
      lastScale = g.scale;
    });
    this.canvas.addEventListener("gestureend", (e) => e.preventDefault());
  }

  /** Show it at this size (in CSS pixels): the canvas has `density` times
      as many pixels each way, and the view keeps its center and scale. */
  private setSize(width: number, height: number) {
    this.canvas.width = Math.round(width * this.density);
    this.canvas.height = Math.round(height * this.density);
    this.canvas.style.width = `${width}px`;
    this.canvas.style.height = `${height}px`;
  }

  /** What it shows, and its size, for saving (see SavedViews). */
  state() {
    const t = this.target;
    return { width: Math.round(this.canvas.width / this.density), height: Math.round(this.canvas.height / this.density),
             cx: t?.cx ?? 0, cy: t?.cy ?? 0, "pixel-size": (t?.px ?? 0) * this.density };
  }

  /** Ask the engine to select a tool (null: navigate). */
  selectTool(name: string | null) {
    send({ type: "selectTool", viewer: this.info.id, name: name ?? "" });
  }

  /** The engine selected a tool. */
  toolSelected(name: string | null, panel: Layout, handles: Handled[]) {
    this.toolName = name;
    this.handles = new Set(handles);
    this.element.classList.toggle("tool", name !== null);
    this.events = [];
    for (const key of [...fieldInputs.keys()]) if (key.startsWith(`tool:${this.info.id}:`)) fieldInputs.delete(key);
    const id = this.info.id;
    this.toolPanel = panel === null ? null : el("div", {}, layoutElement(panel, {
      set: (fieldName, text) => send({ type: "setTool", viewer: id, name: fieldName, text }),
      button: (label) => send({ type: "button", viewer: id, label }),
    }));
    if (activeViewer === this) { showPalette(); showToolPanel(); }
  }

  get drawing() { return this.layers; }

  showDrawing(layers: Shape[][]) {
    this.layers = layers;
    this.redraw();
  }

  /** The engine handled the last pointer event. */
  eventDone() {
    this.eventSent = 0;
    this.sendEvent();
  }

  /** A screen-plane point: a model point, on the plane; on the sphere, in
      its radii from the globe's center. */
  private toPlane(p: { x: number; y: number }): [number, number] {
    const t = this.target!;
    return [t.cx + (p.x - this.canvas.width / 2) * t.px, t.cy - (p.y - this.canvas.height / 2) * t.py];
  }

  private toolEvent(kind: EventKind, a: { x: number; y: number }, b = a) {
    if (!this.target) return;
    const [x1, y1] = this.toPlane(a), [x2, y2] = this.toPlane(b);
    const last = this.events[this.events.length - 1];
    const event = { type: "event" as const, viewer: this.info.id, kind, x1, y1, x2, y2 };
    if (kind === 2 && last?.kind === 2) this.events[this.events.length - 1] = event;
    else this.events.push(event);
    this.sendEvent();
  }

  private sendEvent() {
    // (if an answer never came, go on after a while)
    if (this.eventSent && performance.now() - this.eventSent < 5000) return;
    const next = this.events.shift();
    if (!next) return;
    this.eventSent = performance.now();
    send(next);
  }

  /** A handler has been running for a while: say so, with a spinner and
      the time since it started, and buttons to stop it. */
  showRunning(what: "tool" | "overlay", name: string | null) {
    const frames = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏";
    const started = performance.now() - 250;
    const spinner = el("span", { className: "spinner" });
    const message = el("span", { className: "message",
      textContent: what === "tool" ? `Running the “${name}” tool…` : "Running the overlay…" });
    const stop = el("button", { type: "button", textContent: "Stop",
      title: what === "tool" ? "Stop it (its changes and drawing are dropped)"
                             : "Stop it, and turn the overlay off until the project's opened again" });
    const buttons: HTMLButtonElement[] = [stop];
    const stopping = (navigate: boolean) => {
      message.textContent = "Stopping…";
      buttons.forEach((b) => (b.disabled = true));
      send({ type: "stop", viewer: this.info.id, navigate });
    };
    stop.addEventListener("click", () => stopping(false));
    if (what === "tool") {
      const nav = el("button", { type: "button", textContent: "Stop and switch to Navigate" });
      nav.addEventListener("click", () => stopping(true));
      buttons.push(nav);
    }
    let tick = 0;
    const update = () => {
      spinner.textContent = `${frames[tick++ % frames.length]} ${Math.floor((performance.now() - started) / 1000)} s`;
    };
    update();
    window.clearInterval(this.runTimer);
    this.runTimer = window.setInterval(update, 100);
    this.runEl.replaceChildren(spinner, message, ...buttons);
    this.runEl.hidden = false;
  }

  runDone() {
    window.clearInterval(this.runTimer);
    this.runTimer = undefined;
    this.runEl.hidden = true;
    this.runEl.replaceChildren();
  }

  showError(message: string) {
    this.errorEl.textContent = message;
    this.errorEl.hidden = false;
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
    this.errorEl.hidden = true;
    if (!this.target) {
      if (this.saved && !this.info.sphere) {
        // (as it was saved)
        const ps = this.saved["pixel-size"] / this.density;
        this.target = { cx: this.saved.cx, cy: this.saved.cy, px: ps, py: ps * (view.py / view.px) };
      } else {
        // (the first frame, at the project's size: the same picture across
        // the canvas, whatever its size)
        const scale = frame.width / this.canvas.width;
        this.target = { cx: view.cx, cy: view.cy, px: view.px * scale, py: view.py * scale };
      }
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
      if (this.layers.length) {
        drawLayers(ctx, this.layers, ([x, y]) => [(x - t.cx) / t.px + W / 2, (t.cy - y) / t.py + H / 2],
                   [t.px, t.py], this.density);
      }
    });
  }

  /** Ask the engine for the target view, at the canvas's size, unless it's
      busy (or has it). */
  private request() {
    const t = this.target, f = this.frame;
    if (!t || !f) return;
    // (if an answer never came, ask again after a while)
    if (this.requested && performance.now() - this.requested.at < 2000) return;
    // (turning the globe first, then any change of zoom or size)
    if (this.turn && (this.turn.from[0] !== this.turn.to[0] || this.turn.from[1] !== this.turn.to[1])) {
      const { from, to } = this.turn;
      this.turn.from = to;
      this.requested = { view: { ...t }, at: performance.now() };
      send({ type: "rotate", viewer: this.info.id, fx: from[0], fy: from[1], tx: to[0], ty: to[1] });
      return;
    }
    const width = this.canvas.width, height = this.canvas.height;
    if (sameView(t, f.view) && f.image.width === width && f.image.height === height) return;
    this.requested = { view: { ...t }, at: performance.now() };
    send({ type: "view", viewer: this.info.id, cx: t.cx, cy: t.cy, pixelSize: t.px, width, height });
  }

  /** The pointer's position in the canvas's pixels. */
  private pixel(e: { clientX: number; clientY: number }) {
    const r = this.canvas.getBoundingClientRect();
    return { x: (e.clientX - r.left) * this.canvas.width / r.width,
             y: (e.clientY - r.top) * this.canvas.height / r.height };
  }

  /** Whether the tool handles dragging (else dragging navigates). */
  private get toolDrags() { return this.handles.has("drag") || this.handles.has("dragDone"); }

  private startDrag(e: PointerEvent) {
    if (!this.target || e.button !== 0) return;
    this.canvas.setPointerCapture(e.pointerId);
    this.press = { start: this.pixel(e), dragging: false };
    this.dragFrom = this.press.start;
    if (this.info.sphere && !this.toolDrags) {
      const s = this.onScreen(this.dragFrom);
      this.turn = { from: s, to: s };
    }
  }

  private endDrag(e: PointerEvent) {
    const press = this.press;
    this.press = null;
    this.dragFrom = null;
    if (!press || !this.target) return;
    const p = this.pixel(e);
    if (!press.dragging) { if (this.handles.has("click")) this.toolEvent(0, p); }
    else if (this.handles.has("dragDone")) this.toolEvent(3, press.start, p);
  }

  /** A point of the canvas on the screen plane: in the sphere's radii from
      its center (the view's), y up. */
  private onScreen(p: { x: number; y: number }): [number, number] {
    const t = this.target!;
    return [(p.x - this.canvas.width / 2) * t.px, (this.canvas.height / 2 - p.y) * t.py];
  }

  private moveDrag(e: PointerEvent) {
    const t = this.target;
    if (!this.dragFrom || !t || !this.press) return;
    const p = this.pixel(e);
    // (it's a drag once it's moved a few pixels: until then, a click)
    if (!this.press.dragging) {
      const { x, y } = this.press.start;
      if (Math.hypot(p.x - x, p.y - y) < 3 * this.density) return;
      this.press.dragging = true;
    }
    if (this.toolDrags) {
      if (this.handles.has("drag")) this.toolEvent(2, this.press.start, p);
      return;
    }
    if (this.info.sphere && this.turn) {
      // (the globe turns: the old frame can't show that, so it stays until
      // the new one comes)
      this.turn.to = this.onScreen(p);
      this.request();
      return;
    }
    t.cx -= (p.x - this.dragFrom.x) * t.px;
    t.cy += (p.y - this.dragFrom.y) * t.py;
    this.dragFrom = p;
    this.redraw();
    this.request();
  }

  /** The wheel zooms; so does pinching a trackpad, which most browsers
      send as the wheel with Control held (and smaller steps). */
  private wheel(e: WheelEvent) {
    e.preventDefault();
    // (a line or a page of scrolling counts as more than a pixel)
    const delta = e.deltaY * (e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? 400 : 1);
    this.zoomBy(Math.exp(delta * (e.ctrlKey ? 0.01 : 0.002)), this.pixel(e));
  }

  /** Zoom by a factor (of the pixel size: above 1 zooms out) about a point
      (on the sphere, about the globe's center). */
  private zoomBy(factor: number, p0: { x: number; y: number }) {
    const t = this.target;
    if (!t) return;
    const W = this.canvas.width, H = this.canvas.height;
    const p = this.info.sphere ? { x: W / 2, y: H / 2 } : p0;
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
    case "ready":
      send({ type: "smoothing", on: smoothingBox.checked });
      status("Loading the library…");
      send({ type: "library" });
      break;
    case "library": showLibrary(m.sections); status("Pick a project."); break;
    case "project":
      current = { fileName: m.fileName, library: m.library, handle: m.library ? null : opening };
      opening = null;
      showFileName();
      showProject(m.title, m.panel, m.viewers, m.views);
      break;
    case "file": {
      const write = pendingSave;
      pendingSave = null;
      if (write) write(m.name, m.bytes).then((name) => status(`Saved ${name}.`),
                                              (e) => status(`Couldn't save: ${e}`, true));
      break;
    }
    case "field": fieldResult(m.key, m.error, m.color); break;
    case "fieldText": fieldText(m.key, m.text, m.color); break;
    case "tool": viewers.get(m.viewer)?.toolSelected(m.name, m.panel, m.handles); break;
    case "toolError": status(m.message, true); console.error(m.message); break;
    case "drawing": viewers.get(m.viewer)?.showDrawing(m.layers); break;
    case "eventDone": viewers.get(m.viewer)?.eventDone(); break;
    case "running": viewers.get(m.viewer)?.showRunning(m.what, m.name); break;
    case "runDone": viewers.get(m.viewer)?.runDone(); break;
    case "viewerError": viewers.get(m.viewer)?.showError(m.message); break;
    case "error": status(m.message, true); console.error(m.message); break;
    case "frame": viewers.get(m.viewer)?.show(m); break;
  }
});
engine.addEventListener("error", (e) => status(`The engine failed: ${e.message}`, true));
// (drawings' text is in the core's font: redraw once it's loaded)
document.fonts.load(`13px "DejaVu Sans"`).then(() => viewers.forEach((v) => v.showDrawing(v.drawing)));
status("Starting the engine…");
