// The page: the library, then a project's configuration panel and
// viewers. The engine runs in a worker (engine.ts); see protocol.ts for
// what they say to each other.

import type { Request, Message, Layout, FieldLayout, Patch, LibrarySection, ViewerInfo, SavedViews,
              Shape, Handled, EventKind, Navigation } from "./protocol.js";
import { drawLayers } from "./drawing.js";
import { ScriptEditor } from "./editor.js";
import { pickFile, droppedFile, chooseDestination, type Opened, type FileHandle } from "./files.js";
import { Desk, type MenuItem } from "./windows.js";

const engine = new Worker(new URL("engine.js", import.meta.url), { type: "module" });
const send = (r: Request) => engine.postMessage(r);

const libraryEl = document.getElementById("library")!;
const projectEl = document.getElementById("project")!;
const statusEl = document.getElementById("status")!;
const noticesEl = document.getElementById("notices")!;
const openButton = document.getElementById("open") as HTMLButtonElement;
const saveButton = document.getElementById("save") as HTMLButtonElement;
const saveAsButton = document.getElementById("save-as") as HTMLButtonElement;
const fileNameEl = document.getElementById("file-name")!;
const libraryButton = document.getElementById("toggle-library") as HTMLButtonElement;
const panelButton = document.getElementById("toggle-panel") as HTMLButtonElement;
const paletteEl = document.getElementById("tools")!;
const scriptButton = document.getElementById("toggle-script") as HTMLButtonElement;

// Hiding and showing the library, and the configuration panel.
function toggle(button: HTMLButtonElement, bodyClass: string) {
  const hidden = document.body.classList.toggle(bodyClass);
  button.setAttribute("aria-pressed", String(!hidden));
}
libraryButton.addEventListener("click", () => toggle(libraryButton, "no-library"));
panelButton.addEventListener("click", () => toggle(panelButton, "no-panel"));

// Smoothing, each viewer's to begin with ("Use subpixel smoothing", in its
// menu, changes it; not saved, as on the desktop): off on a high-DPI
// screen (which is drawn at twice the resolution already), on otherwise.
const smoothingAtFirst = (window.devicePixelRatio || 1) < 1.5;

function el<K extends keyof HTMLElementTagNameMap>(tag: K, props: Partial<HTMLElementTagNameMap[K]> = {},
                                                 ...children: (Node | string)[]): HTMLElementTagNameMap[K] {
  const e = Object.assign(document.createElement(tag), props);
  e.append(...children);
  return e;
}

/** What's happening, in the status line; an error is a notice instead
    (and the status line's cleared). */
function status(text: string, isError = false) {
  if (isError) {
    statusEl.textContent = "";
    notice(text, "error");
    return;
  }
  statusEl.textContent = text;
}

/** A notice under the toolbar, until it's dismissed (or one of its
    buttons is pressed): an error, or something to know or decide. The
    newest is on top; only the latest few are kept. */
function notice(text: string, kind: "error" | "info",
                actions: { label: string; run: () => void }[] = []): HTMLElement {
  const dismiss = el("button", { type: "button", className: "notice-dismiss", textContent: "×", title: "Dismiss" });
  dismiss.setAttribute("aria-label", "Dismiss");
  const n = el("div", { className: `notice ${kind}` }, el("div", { className: "notice-text", textContent: text }));
  n.setAttribute("role", kind === "error" ? "alert" : "status");
  if (actions.length) {
    n.append(el("div", { className: "notice-actions" }, ...actions.map((a) => {
      const b = el("button", { type: "button", textContent: a.label });
      b.addEventListener("click", () => { n.remove(); a.run(); });
      return b;
    })));
  }
  n.append(dismiss);
  dismiss.addEventListener("click", () => n.remove());
  noticesEl.prepend(n);
  while (noticesEl.children.length > 4) noticesEl.lastElementChild!.remove();
  return n;
}

function clearNotices() { noticesEl.replaceChildren(); }

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
      [w.title, { x: w.window.offsetLeft, y: w.window.offsetTop, ...w.view.state(),
                  ...(Desk.isMinimized(w.window) ? { minimized: true } : {}) }])),
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
  workspaceEl = el("div", { className: "workspace" }, desk.element, asideEl);
  projectEl.replaceChildren(el("h1", { textContent: title }), workspaceEl);
  projectTitle = title;
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
    const win = desk.add({ title: info.title, ...place, content: v.element, minimized: saved?.minimized,
                           onResize: (width, height) => v.resize(width, height),
                           menu: () => v.menu() });
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
  // (not while typing: in a text box, or the script editor)
  if (e.target instanceof HTMLElement && (e.target.closest("input, select, textarea") || e.target.isContentEditable)) return;
  // (moving about: as on the desktop)
  const navigation: Record<string, () => void> = { "<": () => v.navigate(0), ">": () => v.navigate(1),
                                                   "^": () => v.navigate(2), "@": () => v.askJump() };
  if (navigation[e.key]) { e.preventDefault(); navigation[e.key](); return; }
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

// ---------------------------------------------------------------------
// The script editor: over the whole workspace (the viewers and the
// configuration), from the toolbar's Script button (see editor.ts). A
// rebuild that works closes it (one that doesn't leaves it open, saying
// why); it keeps what's typed in it while it's closed, until another
// project is opened.

let workspaceEl: HTMLElement | null = null;
let projectTitle = "";
let projectScript = "";
let scriptEditor: ScriptEditor | null = null;
let editorOverlay: HTMLElement | null = null;
let editorOpen = false;
let symbolTables: [[string, string][], [string, string][]] | null = null;

function showEditor() {
  if (!workspaceEl) return;
  if (!scriptEditor) {
    scriptEditor = new ScriptEditor(projectScript, {
      analyze: (id, text) => send({ type: "analyze", id, text }),
      rebuild: (text) => send({ type: "rebuild", text }),
    });
    if (symbolTables) scriptEditor.setSymbols(...symbolTables);
    const close = el("button", { type: "button", className: "editor-close", textContent: "Close",
                                 title: "Close the script (what's typed in it is kept)" });
    close.addEventListener("click", () => hideEditor());
    editorOverlay = el("div", { className: "editor-overlay", role: "dialog" },
      el("div", { className: "editor-header" }, el("span", { className: "editor-title" }), close),
      scriptEditor.element);
  }
  const titleEl = editorOverlay!.querySelector(".editor-title")!;
  titleEl.textContent = `${projectTitle}: script`;
  workspaceEl.append(editorOverlay!);
  editorOpen = true;
  scriptButton.setAttribute("aria-pressed", "true");
  scriptEditor.focus();
}

function hideEditor() {
  editorOverlay?.remove();
  editorOpen = false;
  scriptButton.setAttribute("aria-pressed", "false");
}

scriptButton.addEventListener("click", () => (editorOpen ? hideEditor() : showEditor()));

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

/** A picture of a view, and where (in the model) it shows all of what's
    there, if anywhere. */
interface Picture { view: View; image: HTMLCanvasElement; covers: { x0: number; x1: number; y0: number; y1: number } | null }

/** A render going on: its number (see Patch), view, image (the patches
    so far, until the latest ones are flushed onto it, from `data`), and
    where the patches not yet flushed are. */
interface Pending {
  id: number;
  view: View;
  image: HTMLCanvasElement;
  context: CanvasRenderingContext2D;
  data: ImageData;
  dirty: [number, number, number, number] | null;
}

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
  /** The picture: the last render that was done. */
  private frame: Picture | null = null;
  /** The render going on, as it's drawn (see Patch). */
  private pending: Pending | null = null;
  /** The latest render the engine's posted anything of. */
  private latest = 0;
  private target: View | null = null;
  /** The view (and size) the engine was last asked for. */
  private sent: { view: View; width: number; height: number } | null = null;
  private requestPending = false;
  /** Waiting for the engine's answer to the last request (since when, and
      the latest render before it): a render begun since then, done, or
      slow enough to be showing its progress (see `sendRequest`). */
  private waiting: { since: number; after: number } | null = null;
  /** Whether to make the view a step in the history, once it's asked for
      (see `recordSoon`). */
  private recordWanted = false;
  private recordTimer: number | undefined;
  /** Waiting for the engine to read an aspect ratio (see `askAspect`). */
  private aspectAnswer: ((error: string | null) => void) | null = null;
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
  /** "Show pointer position": the readout (hidden unless it's on), and
      the pointer's latest screen-plane point, waiting to be asked about
      (one question at a time, as for tool events: the engine knows where
      a point of the globe is). */
  private readonly pointerEl: HTMLElement;
  private pointerAsked = 0;
  private pointerNext: [number, number] | null = null;

  constructor(readonly info: ViewerInfo, width: number, height: number,
              private readonly saved: { cx: number; cy: number; "pixel-size": number } | null) {
    this.canvas = el("canvas");
    this.context = this.canvas.getContext("2d")!;
    this.errorEl = el("div", { className: "viewer-error", hidden: true });
    this.runEl = el("div", { className: "viewer-running", hidden: true, role: "status" });
    this.pointerEl = el("div", { className: "viewer-pointer", hidden: true, textContent: "…" });
    this.element = el("div", { className: "viewer" }, this.canvas, this.errorEl, this.runEl, this.pointerEl);
    this.setSize(width, height);
    // (focusable, and focused when it's clicked: so the keyboard's for the
    // tool shortcuts again, after typing in the script editor)
    this.canvas.tabIndex = -1;
    this.canvas.addEventListener("pointerdown", (e) => { this.canvas.focus({ preventScroll: true }); this.startDrag(e); });
    this.canvas.addEventListener("pointermove", (e) => { this.moveDrag(e); this.trackPointer(e); });
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

  get pointerShown() { return !this.pointerEl.hidden; }

  setPointerShown(on: boolean) {
    this.pointerEl.hidden = !on;
  }

  /** Where the pointer is, for the readout (if it's shown). */
  private trackPointer(e: PointerEvent) {
    if (this.pointerEl.hidden || !this.target) return;
    const p = this.pixel(e);
    this.pointerNext = this.info.sphere ? this.onScreen(p) : this.toPlane(p);
    this.askPointer();
  }

  private askPointer() {
    // (if an answer never came, go on after a while)
    if (this.pointerAsked && performance.now() - this.pointerAsked < 2000) return;
    const next = this.pointerNext;
    if (!next) return;
    this.pointerNext = null;
    this.pointerAsked = performance.now();
    send({ type: "pointer", viewer: this.info.id, x: next[0], y: next[1] });
  }

  /** The engine's answer: what the readout shows. */
  showPointer(text: string) {
    this.pointerEl.textContent = text;
    this.pointerAsked = 0;
    this.askPointer();
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

  /** Part of a render (see Patch): its rectangles into the render's
      image, which is drawn over what was there (see `redraw`). */
  patch(m: Patch) {
    this.latest = Math.max(this.latest, m.id);
    // (the answer to the last request: done, or slow, so it can be
    // dropped for the next one)
    if (this.waiting && m.id > this.waiting.after) { this.waiting = null; this.request(); }
    if (m.id < (this.pending?.id ?? 0)) return;
    if (!this.pending || this.pending.id !== m.id) this.startRender(m);
    const p = this.pending!;
    const src = new Uint8ClampedArray(m.pixels);
    const dst = p.data.data;
    let k = 0;
    for (let r = 0; r < m.rects.length; r += 4) {
      const [x, y, w, h] = m.rects.slice(r, r + 4);
      for (let row = y; row < y + h; row++, k += 4 * w) dst.set(src.subarray(k, k + 4 * w), 4 * (row * m.width + x));
      const d = p.dirty;
      p.dirty = d ? [Math.min(d[0], x), Math.min(d[1], y), Math.max(d[2], x + w), Math.max(d[3], y + h)]
                  : [x, y, x + w, y + h];
    }
    if (m.done) this.finishRender();
    this.redraw();
  }

  /** A new render's first patch. A render it replaces, dropped before it
      was done, is let go: what it drew (in its own view, on its own grid
      of pieces) isn't kept, and the picture is still the last render
      that was done. */
  private startRender(m: Patch) {
    const view = { cx: m.cx, cy: m.cy, px: m.px, py: m.py };
    const image = el("canvas", { width: m.width, height: m.height });
    this.pending = { id: m.id, view, image, context: image.getContext("2d")!,
                     data: new ImageData(m.width, m.height), dirty: null };
    this.errorEl.hidden = true;
    if (!this.target) {
      if (this.saved) {
        // (as it was saved: on the sphere, the engine's turned the globe
        // as it was too)
        const ps = this.saved["pixel-size"] / this.density;
        this.target = { cx: this.saved.cx, cy: this.saved.cy, px: ps, py: ps * (view.py / view.px) };
      } else {
        // (the first frame, at the project's size: the same picture across
        // the canvas, whatever its size)
        const scale = m.width / this.canvas.width;
        this.target = { cx: view.cx, cy: view.cy, px: view.px * scale, py: view.py * scale };
      }
      // (where it starts: the first step in its history)
      this.recordWanted = true;
    }
    // (the engine's rendering this, without being asked: the first
    // render, or one of new parameters)
    this.sent ??= { view: { ...view }, width: m.width, height: m.height };
    this.request();
  }

  /** The render's done: it's the picture now, all of it drawn. */
  private finishRender() {
    const p = this.pending!;
    this.flush(p);
    const W = p.image.width, H = p.image.height, v = p.view;
    this.frame = { view: v, image: p.image,
                   covers: { x0: v.cx - (W / 2) * v.px, x1: v.cx + (W / 2) * v.px,
                             y0: v.cy - (H / 2) * v.py, y1: v.cy + (H / 2) * v.py } };
    this.pending = null;
    // (how many renders it's finished: for tests, and anyone curious)
    this.element.dataset.frames = String(Number(this.element.dataset.frames ?? 0) + 1);
    this.request();
  }

  /** A render's patches so far, onto its image. */
  private flush(p: Pending) {
    if (!p.dirty) return;
    const [x0, y0, x1, y1] = p.dirty;
    p.context.putImageData(p.data, 0, 0, x0, y0, x1 - x0, y1 - y0);
    p.dirty = null;
  }

  /** Draw a picture (of a view) onto a canvas (of a view, and a size). */
  private drawPicture(ctx: CanvasRenderingContext2D, v: View, image: HTMLCanvasElement, t: View, W: number, H: number) {
    const fw = image.width, fh = image.height;
    // where the picture's corners are, in the target view's pixels
    const left = (v.cx - (fw / 2) * v.px - t.cx) / t.px + W / 2;
    const top = (t.cy - (v.cy + (fh / 2) * v.py)) / t.py + H / 2;
    ctx.drawImage(image, left, top, (fw * v.px) / t.px, (fh * v.py) / t.py);
  }

  /** Draw the picture in the target view, with the render going on over it
      (once per display frame). */
  private redraw() {
    if (this.drawPending) return;
    this.drawPending = true;
    requestAnimationFrame(() => {
      this.drawPending = false;
      const t = this.target;
      if (!t) return;
      const W = this.canvas.width, H = this.canvas.height;
      const ctx = this.context;
      ctx.fillStyle = "#808080";
      ctx.fillRect(0, 0, W, H);
      if (this.frame) this.drawPicture(ctx, this.frame.view, this.frame.image, t, W, H);
      if (this.pending) {
        this.flush(this.pending);
        this.drawPicture(ctx, this.pending.view, this.pending.image, t, W, H);
      }
      if (this.layers.length) {
        drawLayers(ctx, this.layers, ([x, y]) => [(x - t.cx) / t.px + W / 2, (t.cy - y) / t.py + H / 2],
                   [t.px, t.py], this.density);
      }
    });
  }

  /** Ask the engine for the target view, at the canvas's size, if it's
      changed: once it's answered the last request (see `sendRequest`),
      and once per display frame, at most. */
  private request() {
    if (this.requestPending) return;
    this.requestPending = true;
    requestAnimationFrame(() => {
      this.requestPending = false;
      this.sendRequest();
    });
  }

  /** One request at a time, as the engine answers them: a render that's
      quick (done within the engine's hold, a tenth of a second) comes
      whole, and the next request waits for it, so a drag shows whole
      frames; a slow one shows its progress after the hold, and then the
      next request can go (and drop it). (If an answer never comes, the
      next goes after a while.) */
  private sendRequest() {
    const t = this.target;
    if (!t || !this.sent) return;
    if (this.waiting && performance.now() - this.waiting.since < 2000) return;
    const width = this.canvas.width, height = this.canvas.height;
    const turned = this.turn && (this.turn.from[0] !== this.turn.to[0] || this.turn.from[1] !== this.turn.to[1]);
    const moved = !sameView(t, this.sent.view) || this.sent.width !== width || this.sent.height !== height;
    if (!turned && !moved) {
      // (a gesture's end, once its view's been asked for and answered)
      if (this.recordWanted) { this.recordWanted = false; send({ type: "record", viewer: this.info.id }); }
      return;
    }
    this.waiting = { since: performance.now(), after: this.latest };
    // (turning the globe first, then any change of zoom or size)
    if (turned) {
      const { from, to } = this.turn!;
      this.turn!.from = to;
      send({ type: "rotate", viewer: this.info.id, fx: from[0], fy: from[1], tx: to[0], ty: to[1] });
    }
    if (moved) {
      this.sent = { view: { ...t }, width, height };
      send({ type: "view", viewer: this.info.id, cx: t.cx, cy: t.cy, pixelSize: t.px, width, height,
             covered: this.covered(t, width, height) });
    }
  }

  /** Where (in pixels of a view, of a size) the picture shows all of what
      it covers: none of it on the sphere, whose globe may have turned. */
  private covered(t: View, W: number, H: number): [number, number, number, number] | null {
    const c = this.frame?.covers;
    if (!c || this.info.sphere) return null;
    const left = Math.max(0, Math.ceil((c.x0 - t.cx) / t.px + W / 2 - 1e-6));
    const right = Math.min(W, Math.floor((c.x1 - t.cx) / t.px + W / 2 + 1e-6));
    const top = Math.max(0, Math.ceil((t.cy - c.y1) / t.py + H / 2 - 1e-6));
    const bottom = Math.min(H, Math.floor((t.cy - c.y0) / t.py + H / 2 + 1e-6));
    return right > left && bottom > top ? [left, top, right, bottom] : null;
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
    // (a pan, or a turn of the globe, done: a step in the history)
    else if (!this.toolDrags) this.recordSoon(0);
  }

  /** Make the view, once it's been asked for, a step in the history
      (after `delay` ms with no more zooming, for a wheel or a pinch). */
  private recordSoon(delay: number) {
    clearTimeout(this.recordTimer);
    this.recordTimer = window.setTimeout(() => { this.recordWanted = true; this.request(); }, delay);
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
    this.recordSoon(300);
  }

  /** Its menu, as the desktop's: moving about, the sphere or the plane,
      its colorizer, and its options. */
  menu(): MenuItem[] {
    const c = this.info.colorizer;
    const items: MenuItem[] = [
      { kind: "action", label: "Last view", shortcut: "<", run: () => this.navigate(0) },
      { kind: "action", label: "Next view", shortcut: ">", run: () => this.navigate(1) },
      { kind: "action", label: "Jump to view…", shortcut: "@", run: () => this.askJump() },
      { kind: "action", label: "Original view", shortcut: "^", run: () => this.navigate(2) },
    ];
    // (a complex viewer's pixels are square; it can show the sphere instead)
    if (this.info.complex) {
      items.push({ kind: "toggle", label: "Show on the sphere", on: this.info.sphere, toggle: () => this.navigate(3) });
    } else {
      items.push({ kind: "action", label: "Aspect ratio…", run: () => this.askAspect() });
    }
    if (c) {
      items.push({ kind: "separator" }, { kind: "submenu", label: "Colorizer", items: () => [
        ...c.items.map((i): MenuItem => ({
          kind: "toggle", label: i.label, on: i.inUse, help: i.description ?? undefined,
          toggle: () => { if (!i.inUse) send({ type: "colorizer", viewer: this.info.id, name: i.name }); } })),
        { kind: "separator" },
        { kind: "action", label: "Arguments…", enabled: c.arguments,
          run: () => send({ type: "colorizerArguments", viewer: this.info.id }) },
      ] });
    }
    items.push(
      { kind: "separator" },
      { kind: "toggle", label: "Show pointer position", on: this.pointerShown,
        toggle: () => this.setPointerShown(!this.pointerShown) },
      { kind: "toggle", label: "Use subpixel smoothing", on: this.info.smooth,
        toggle: () => {
          this.info.smooth = !this.info.smooth;
          send({ type: "viewerSmoothing", viewer: this.info.id, on: this.info.smooth });
        } });
    return items;
  }

  /** The last view, the next, the original one, or the sphere (or the plane). */
  navigate(what: Navigation) {
    send({ type: "navigate", viewer: this.info.id, what });
  }

  /** The engine moved the view (going back, say): it's the target now,
      and what the engine's rendering. On the sphere, or turning to or
      from it, the last picture can't be moved to fit, so it goes. */
  moved(m: { cx: number; cy: number; px: number; py: number; sphere: boolean }) {
    if (m.sphere || this.info.sphere) this.frame = null;
    this.info.sphere = m.sphere;
    const view = { cx: m.cx, cy: m.cy, px: m.px, py: m.py };
    this.target = { ...view };
    this.sent = { view: { ...view }, width: this.canvas.width, height: this.canvas.height };
    this.turn = null;
    this.redraw();
  }

  /** "Jump to view…": where it is now, first (see `jumpFrom`). */
  askJump() {
    send({ type: "where", viewer: this.info.id });
  }

  /** Ask where to go: a center (on the sphere, the point to face) and a
      pixel's size, starting from where it is. */
  jumpFrom(x: number, y: number, pixelSize: number) {
    const sphere = this.info.sphere;
    askForm(this.element, "Jump to view", [
      { label: sphere ? "Face x =" : "x =", value: String(x) },
      { label: sphere ? "Face y =" : "y =", value: String(y) },
      { label: "pixel size =", value: String(pixelSize) },
    ], (values) => {
      const [nx, ny, ps] = values.map((v) => Number(v.trim()));
      if (![nx, ny, ps].every(Number.isFinite) || !(ps > 0)) return "Each should be a number (and the pixel size more than 0).";
      send({ type: "jump", viewer: this.info.id, x: nx, y: ny, pixelSize: ps });
      return null;
    });
  }

  /** A real viewer's aspect ratio (a pixel's height over its width), as
      the desktop asks for it: the engine reads it (an expression, like
      1/2), and says if it isn't one. */
  askAspect() {
    const t = this.target;
    const now = t ? String(Number((t.py / t.px).toPrecision(6))) : "1";
    askForm(this.element, "Aspect ratio", [{ label: "pixel height / pixel width =", value: now }],
            (values) => new Promise((resolve) => {
              this.aspectAnswer = resolve;
              send({ type: "aspect", viewer: this.info.id, text: values[0] });
            }),
            [{ label: "Square pixels", values: ["1"] }]);
  }

  /** The engine's answer to an aspect ratio: why it isn't one, if it isn't. */
  aspectAnswered(error: string | null) {
    this.aspectAnswer?.(error);
    this.aspectAnswer = null;
  }

  /** Ask for a colorizer's arguments, as the desktop does. */
  askArguments(name: string, fields: { param: string; note: string; text: string }[]) {
    askForm(this.element, `${name}: arguments`,
            fields.map((f) => ({ label: `${f.param} =`, note: f.note, value: f.text })),
            (answers) => {
              send({ type: "colorizerAnswers", viewer: this.info.id, name, answers });
              return null;
            });
  }
}

/** A small form over a viewer: each field a label, a note, and a text box;
    OK (or Enter) gives `ok` the answers, which says (or promises to say)
    what's wrong with them, if anything (and then the form stays); Cancel
    (or Escape) closes it. Any other buttons fill in the fields, and OK. */
function askForm(host: HTMLElement, title: string, fields: { label: string; note?: string; value: string }[],
                 ok: (values: string[]) => string | null | Promise<string | null>,
                 others: { label: string; values: string[] }[] = []) {
  host.querySelector(".viewer-form")?.remove();
  const inputs = fields.map((f) => el("input", { type: "text", value: f.value, spellcheck: false }));
  const error = el("div", { className: "form-error" });
  const okButton = el("button", { type: "submit", textContent: "OK" });
  const cancel = el("button", { type: "button", textContent: "Cancel" });
  const extra = others.map((o) => {
    const b = el("button", { type: "button", textContent: o.label });
    b.addEventListener("click", () => {
      o.values.forEach((v, i) => { if (inputs[i]) inputs[i].value = v; });
      form.requestSubmit();
    });
    return b;
  });
  const form = el("form", { className: "viewer-form" },
    el("div", { className: "form-title", textContent: title }),
    ...fields.map((f, i) => el("label", { className: "form-field" },
      el("span", { textContent: f.label }), inputs[i], f.note ? el("small", { textContent: f.note }) : "")),
    error, el("div", { className: "form-buttons" }, ...extra, el("span", { className: "form-gap" }), cancel, okButton));
  form.setAttribute("role", "dialog");
  const close = () => form.remove();
  form.addEventListener("submit", async (e) => {
    e.preventDefault();
    const problem = await ok(inputs.map((i) => i.value));
    if (problem) error.textContent = problem; else close();
  });
  cancel.addEventListener("click", close);
  form.addEventListener("keydown", (e) => { if (e.key === "Escape") { e.preventDefault(); close(); } });
  // (not the viewer's: no navigating, nor tools, from inside the form)
  form.addEventListener("pointerdown", (e) => e.stopPropagation());
  host.append(form);
  inputs[0]?.focus();
  inputs[0]?.select();
}

// ---------------------------------------------------------------------

engine.addEventListener("message", (e: MessageEvent<Message>) => {
  const m = e.data;
  switch (m.type) {
    case "ready":
      send({ type: "smoothing", on: smoothingAtFirst });
      send({ type: "density", value: density() });
      send({ type: "symbols" });
      status("Loading the library…");
      send({ type: "library" });
      break;
    case "library": showLibrary(m.sections); status("Pick a project."); break;
    case "project":
      if (m.rebuilt) {
        // (the same project, built again from its edited script: its
        // windows stay as they were, and so does the editor)
        // (and, for one whose script hadn't built, so had no windows yet,
        // where its file had them)
        const views = { viewers: { ...m.views?.viewers, ...viewState().viewers } };
        showProject(m.title, m.panel, m.viewers, views);
        scriptEditor?.projectScript(m.script, true);
        // (it worked: the editor's done its job)
        hideEditor();
        status("Rebuilt.");
        break;
      }
      // (a new project has no file of its own yet: not even the one it
      // was translated from)
      current = { fileName: m.fileName, library: m.library, handle: m.library || m.new ? null : opening };
      opening = null;
      clearNotices();
      showFileName();
      // (a new project: a new script, and no editor until it's asked for)
      projectScript = m.script;
      scriptEditor = null;
      editorOpen = false;
      showProject(m.title, m.panel, m.viewers, m.views);
      scriptButton.disabled = false;
      scriptButton.setAttribute("aria-pressed", "false");
      if (m.notes.length) notice(`Translation notes:\n${m.notes.map((n) => `• ${n}`).join("\n")}`, "info");
      if (m.edit) {
        showEditor();
        if (m.viewers.length === 0) status("Its script doesn't build yet: fix it, then Rebuild.");
      }
      break;
    case "ancient":
      status("");
      opening = null;
      notice(`“${m.name}” looks like a file from an old version of FractalStream (1.0). ` +
             `Translate it into a new project? (The old file is left as it is.)`, "info",
             [{ label: "Translate", run: () => { status(`Translating ${m.name}…`); send({ type: "translate" }); } }]);
      break;
    case "pointer": viewers.get(m.viewer)?.showPointer(m.text); break;
    case "view": viewers.get(m.viewer)?.moved(m); break;
    case "where": viewers.get(m.viewer)?.jumpFrom(m.x, m.y, m.pixelSize); break;
    case "aspect": viewers.get(m.viewer)?.aspectAnswered(m.error); break;
    case "colorizerArguments": viewers.get(m.viewer)?.askArguments(m.name, m.fields); break;
    case "analysis": scriptEditor?.analysis(m.id, m); break;
    case "symbols": symbolTables = [m.names, m.accents]; scriptEditor?.setSymbols(m.names, m.accents); break;
    case "rebuildError": scriptEditor?.rebuildFailed(m.error); break;
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
    case "patch": viewers.get(m.viewer)?.patch(m); break;
  }
});
engine.addEventListener("error", (e) => status(`The engine failed: ${e.message}`, true));
// (drawings' text is in the core's font: redraw once it's loaded)
document.fonts.load(`13px "DejaVu Sans"`).then(() => viewers.forEach((v) => v.showDrawing(v.drawing)));
status("Starting the engine…");
