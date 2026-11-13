// The page: the library (welcome.ts) while no project's open; then a
// project's viewers, its title (a menu, floating in the corner) and its
// configuration (a drawer, pulled out from the right), until it's closed.
// The engine runs in a worker (engine.ts); see protocol.ts for what they
// say to each other.

import type { Request, Message, Layout, FieldLayout, Patch, ViewerInfo, SavedViews, SavedViewer,
              Shape, Handled, EventKind, Navigation, AutocolorGroup, AutocolorEdit } from "./protocol.js";
import { drawLayers } from "./drawing.js";
import { ScriptEditor } from "./editor.js";
import { pickFile, droppedFile, chooseDestination, type Opened, type FileHandle } from "./files.js";
import { Desk, menuButton, type MenuItem } from "./windows.js";
import type { Movies } from "./capture.js";
import { Welcome } from "./welcome.js";
import { onPreferences, preferences, smoothingNow, type Preferences } from "./preferences.js";
import { showPreferences } from "./preferences-dialog.js";
import { makeLink, readLink, hasLink, fetchProject } from "./links.js";
import { sideBySide, fitted, tiled, toFractions, fromFractions, narrowScreen, type Place, type Room } from "./layout.js";

const engine = new Worker(new URL("engine.js", import.meta.url), { type: "module" });
const send = (r: Request) => engine.postMessage(r);

const projectEl = document.getElementById("project")!;
const noticesEl = document.getElementById("notices")!;

// The preferences the engine follows (once it's started): whether new
// viewers are smoothed (each viewer's ☰ menu's "Use subpixel smoothing"
// changes its own; not saved, as on the desktop), and the sphere's
// shading.
let engineReady = false;
let engineFollows: Preferences | null = null;
function sendPreferences(p: Preferences) {
  if (!engineReady) return;
  const before = engineFollows;
  engineFollows = p;
  if (!before || smoothingNow(before) !== smoothingNow(p)) send({ type: "smoothing", on: smoothingNow(p) });
  if (!before || before.sphereShading !== p.sphereShading) send({ type: "sphereShading", value: p.sphereShading / 100 });
}

function el<K extends keyof HTMLElementTagNameMap>(tag: K, props: Partial<HTMLElementTagNameMap[K]> = {},
                                                 ...children: (Node | string)[]): HTMLElementTagNameMap[K] {
  const e = Object.assign(document.createElement(tag), props);
  e.append(...children);
  return e;
}

/** Say something: an error, as a notice (until it's dismissed); else,
    something that's happened, as a notice that goes after a few seconds. */
function say(text: string, isError = false) {
  if (isError) { notice(text, "error"); return; }
  const n = notice(text, "info");
  n.classList.add("passing");
  setTimeout(() => n.remove(), 4000);
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
// The library, and the two screens: the library's while no project's
// open, and the project's.

const welcome = new Welcome({
  open: (section, entry) => send({ type: "open", section, entry }),
  openFile: async () => openFile(await pickFile()),
  preferences: showPreferences,
});
document.getElementById("welcome")!.append(welcome.element);
const projectScreen = document.getElementById("project-screen")!;

function showScreen(project: boolean) {
  document.body.classList.toggle("in-project", project);
  projectScreen.hidden = !project;
  welcome.element.parentElement!.hidden = project;
  if (!project) welcome.focus();
}

// The browser's Back button closes the project (asking first, as Close
// does), back to the library: opening one adds a step to the page's
// history, and closing it some other way takes that step back.
let leavingHistory = false;
// (a step left from before the page was reloaded isn't the open project's)
if (history.state?.project) history.replaceState(null, "");
function enterHistory() { if (!history.state?.project) history.pushState({ project: true }, ""); }
function leaveHistory() {
  if (!history.state?.project) return;
  leavingHistory = true;
  history.back();
}
window.addEventListener("popstate", () => {
  if (leavingHistory) { leavingHistory = false; return; }
  // (a link, typed or pasted into the address bar: see openLink)
  if (hasLink(location.hash)) return;
  if (!current) return;
  // (stay, until the question's answered)
  history.pushState({ project: true }, "");
  void closeProject();
});

// ---------------------------------------------------------------------
// Links (links.ts): opened when the page is, once the library's loaded,
// or when one's put in the address bar; then taken out of it (so that
// reloading goes back to the library). Copy link (in the title's menu)
// makes one, of the project as it is now.

let libraryLoaded = false;

async function openLink() {
  if (!libraryLoaded || !hasLink(location.hash)) return;
  const hash = location.hash;
  history.replaceState(history.state, "", location.pathname + location.search);
  let link;
  try {
    link = await readLink(hash);
  } catch {
    say("That link couldn't be read: it may have been cut short, or changed.", true);
    return;
  }
  if (!link) return;
  switch (link.kind) {
    case "library":
      if (current && !(await offerToSave("opening the link"))) return;
      welcome.setOpening(true);
      send({ type: "openLink", id: link.id, state: link.state });
      break;
    case "project":
      await openFile({ file: new File([link.bytes as BlobPart], link.name), handle: null });
      break;
    case "url":
      try {
        const { name, bytes } = await fetchProject(link.url);
        await openFile({ file: new File([bytes], name), handle: null });
      } catch (e) {
        say(`Couldn't open ${link.url}: ${e instanceof Error ? e.message : e}. ` +
            "(Its server has to let other web pages read it, as GitHub's raw files and Gists do.)", true);
      }
      break;
  }
}
window.addEventListener("hashchange", () => void openLink());

/** Copy link: a link to the project as it is now, to the clipboard (or,
    if the page can't put it there, in a dialog, to copy). */
function copyLink() {
  send({ type: "share", views: JSON.stringify(viewState()), draft: scriptEditor?.draft() ?? null });
}

async function linkMade(m: Extract<Message, { type: "share" }>) {
  const link = await makeLink(m);
  try {
    await navigator.clipboard.writeText(link);
    say(m.library ? "Link copied: it opens this project, as it is now."
                  : `Link copied: it opens this project, as it is now (its whole script is in it: ${Math.round(link.length / 1024)} KB).`);
  } catch {
    showLink(link);
  }
}

function showLink(link: string) {
  const text = el("textarea", { className: "link-text", value: link, readOnly: true, rows: 4 });
  const done = el("button", { type: "button", textContent: "Done" });
  const dialog = el("div", { className: "page-dialog", role: "dialog" },
    el("div", { className: "page-dialog-box" },
      el("div", { className: "form-title", textContent: "A link to this project" }),
      el("p", { textContent: "Copy it to share the project as it is now:" }),
      text,
      el("div", { className: "form-buttons" }, el("span", { className: "form-gap" }), done)));
  done.addEventListener("click", () => dialog.remove());
  dialog.addEventListener("keydown", (e) => { if (e.key === "Escape") dialog.remove(); });
  document.body.append(dialog);
  text.focus();
  text.select();
}

// ---------------------------------------------------------------------
// Files

/** The open project's file: its name, whether it's from the library,
    where to save it back (if the browser allows that), and whether to
    save its tables' data in it, for plain saves (see `save`). */
let current: { fileName: string; library: boolean; handle: FileHandle | null; saveTables: boolean | null } | null = null;
/** The handle of the file being opened, until its project comes. */
let opening: FileHandle | null = null;
/** Where the next saved file goes (see "file" messages), and what to tell
    once it's written (or not). */
let pendingSave: { write: (name: string, bytes: ArrayBuffer) => Promise<string>; done: (ok: boolean) => void } | null = null;
/** Whether the open project has changes to save, as the engine counts
    them (see "unsaved"); or what's typed in its script editor isn't its
    script. */
let engineUnsaved = false;
function unsaved(): boolean {
  return current !== null && (engineUnsaved || (scriptEditor?.draft() ?? null) !== null);
}
// (leaving the page, as closing the project, asks first)
window.addEventListener("beforeunload", (e) => { if (unsaved()) e.preventDefault(); });

async function openFile(opened: Opened | null) {
  if (!opened) return;
  // (another project, in place of the open one: as closing it, asked first)
  if (current && !(await offerToSave("opening another project"))) return;
  opening = opened.handle;
  welcome.setOpening(true);
  const bytes = await opened.file.arrayBuffer();
  engine.postMessage({ type: "openFile", name: opened.file.name, bytes } satisfies Request, [bytes]);
}

/** Save the project, as the desktop app does: whether to save what its
    tables hold (if they hold anything) is asked every time the file is
    chosen (before choosing it, so that choosing it is the last step), and
    the first time it isn't; the answer's kept for plain saves from then
    on (to begin with, whether its file had them). Whether it was saved:
    not if it was cancelled, or failed. */
async function save(saveAs: boolean): Promise<boolean> {
  if (!current) return false;
  const c = current;
  try {
    const choosing = saveAs || c.library || !c.handle;
    const summary = await tableSummary();
    let tables: 0 | 1 | 2;
    let answer: boolean | null = null;
    if (!summary.built) tables = 2;
    else if (summary.tables.length === 0) tables = 0;
    else if (c.saveTables !== null && !choosing) tables = c.saveTables ? 1 : 0;
    else {
      answer = await askSaveTables(summary.tables, c.saveTables ?? true);
      if (answer === null) return false;
      tables = answer ? 1 : 0;
    }
    const destination = await chooseDestination(c.library ? null : c.handle, saveAs, c.fileName, (handle) => {
      c.handle = handle;
      c.library = false;
      c.fileName = handle.name;
      showFileName();
    });
    if (!destination) return false;
    if (answer !== null) c.saveTables = answer;
    const draft = scriptEditor?.draft() ?? null;
    const saved = await new Promise<boolean>((done) => {
      pendingSave = { write: destination, done };
      send({ type: "save", views: JSON.stringify(viewState()), tables, draft });
    });
    // (what was typed is the project's script now, as saved: as the
    // desktop's is, though it's not rebuilt yet)
    if (saved && draft !== null) {
      projectScript = draft;
      scriptEditor?.projectScript(draft, false);
      showFileName();
    }
    return saved;
  } catch (e) {
    say(`Couldn't save: ${e}`, true);
    return false;
  }
}

/** Before closing the project (or opening another, or leaving the page),
    as the desktop app does: if it has changes to save, ask whether to
    save them first. Whether to go ahead: not if that's cancelled, or so
    is saving. */
async function offerToSave(what: string): Promise<boolean> {
  if (!current || !unsaved()) return true;
  const answer = await ask(`Save the changes to “${projectTitle}” before ${what}?`,
                           "If you don't, they're lost.", ["Cancel", "Don't save", "Save"], 2);
  if (answer === 2) return save(false);
  return answer === 1;
}

/** Close the project, back to the library (asking about any changes to
    save first). Whether it was closed. */
async function closeProject(): Promise<boolean> {
  if (!current) return true;
  if (!(await offerToSave("closing it"))) return false;
  send({ type: "close" });
  forgetProject();
  leaveHistory();
  showScreen(false);
  return true;
}

/** The page forgets the open project (the engine's closed it, or opened
    another in its place). */
function forgetProject() {
  hideEditor();
  scriptEditor = null;
  editorOverlay = null;
  projectScript = "";
  hideAutocolor();
  autocolorOverlay = null;
  for (const v of viewers.values()) v.runDone();
  viewers.clear();
  fieldInputs.clear();
  windows = [];
  activeViewer = null;
  toolPanelEl = asideEl = workspaceEl = null;
  projectEl.replaceChildren();
  current = null;
  engineUnsaved = false;
  showFileName();
  closeDetails();
  clearNotices();
}

/** A question over the whole page, with buttons (the last is the
    default): the button chosen, or 0 for Escape (as the first). */
function ask(title: string, text: string, labels: string[], focus: number): Promise<number> {
  return new Promise((resolve) => {
    const buttons = labels.map((label, i) => {
      const b = el("button", { type: "button", textContent: label });
      b.addEventListener("click", () => { dialog.remove(); resolve(i); });
      return b;
    });
    const dialog = el("div", { className: "page-dialog", role: "dialog" },
      el("div", { className: "page-dialog-box" },
        el("div", { className: "form-title", textContent: title }),
        el("p", { textContent: text }),
        el("div", { className: "form-buttons" }, buttons[0], el("span", { className: "form-gap" }), ...buttons.slice(1))));
    dialog.addEventListener("keydown", (e) => { if (e.key === "Escape") { dialog.remove(); resolve(0); } });
    document.body.append(dialog);
    buttons[focus].focus();
  });
}

/** Edit project details (from the title's menu), as the desktop's
    Project info: its title, description and preview, as the library shows
    them. (The preview's made with a viewer's "Use this as preview".) */
let detailsDialog: { element: HTMLElement; preview: HTMLElement } | null = null;

function previewElement(preview: string | null): HTMLElement {
  return preview
    ? el("img", { className: "details-preview", src: `data:image/png;base64,${preview}`, alt: "The project's preview" })
    : el("p", { className: "details-no-preview",
                textContent: "No preview yet. To make one, choose “Use this as preview” from a viewer's ☰ menu." });
}

function showDetails(m: Extract<Message, { type: "details" }>) {
  if (m.fromViewer) {
    // (made with "Use this as preview": the dialog shows it, if it's open)
    if (detailsDialog) detailsDialog.preview.replaceChildren(previewElement(m.preview));
    say("That's the project's preview now, as the library will show it.");
    return;
  }
  closeDetails();
  const title = el("input", { type: "text", value: m.title, placeholder: m.fileName.replace(/\.fs$/, "") });
  const description = el("textarea", { className: "details-description", value: m.description, rows: 6 });
  const preview = el("div", {}, previewElement(m.preview));
  const close = () => closeDetails();
  const ok = () => {
    send({ type: "setDetails", title: title.value, description: description.value });
    // (as the engine calls it from now on)
    projectTitle = title.value.trim() || m.fileName.replace(/\.fs$/, "");
    showFileName();
    close();
  };
  const cancelButton = el("button", { type: "button", textContent: "Cancel" });
  const okButton = el("button", { type: "button", textContent: "OK" });
  cancelButton.addEventListener("click", close);
  okButton.addEventListener("click", ok);
  title.addEventListener("keydown", (e) => { if (e.key === "Enter") ok(); });
  const element = el("div", { className: "page-dialog", role: "dialog" },
    el("div", { className: "page-dialog-box details-dialog" },
      el("div", { className: "form-title", textContent: "Project details" }),
      el("label", { className: "form-field" }, el("span", { textContent: "Title:" }), title),
      el("label", { className: "details-field" }, el("span", { textContent: "Description:" }), description),
      preview,
      el("p", { className: "details-file", textContent: `File: ${m.fileName}` }),
      el("div", { className: "form-buttons" }, cancelButton, el("span", { className: "form-gap" }), okButton)));
  element.addEventListener("keydown", (e) => { if (e.key === "Escape") { e.preventDefault(); close(); } });
  document.body.append(element);
  detailsDialog = { element, preview };
  title.focus();
}

function closeDetails() {
  detailsDialog?.element.remove();
  detailsDialog = null;
}

/** The tables that hold anything (see "tableSummary"). */
type TableSummary = Extract<Message, { type: "tableSummary" }>;
let tableSummaryWaiting: ((s: TableSummary) => void) | null = null;
function tableSummary(): Promise<TableSummary> {
  return new Promise((resolve) => {
    tableSummaryWaiting = resolve;
    send({ type: "tableSummary" });
  });
}

/** "812 bytes", "14 KB", "2.1 MB", as the desktop says them. */
function describeBytes(n: number): string {
  const scaled = (unit: number, name: string) => {
    const x = n / unit;
    return `${x < 10 ? x.toFixed(1) : x.toFixed(0)} ${name}`;
  };
  return n < 1e3 ? `${n} bytes` : n < 1e6 ? scaled(1e3, "KB") : n < 1e9 ? scaled(1e6, "MB") : scaled(1e9, "GB");
}

/** Ask whether to save what the tables hold, as the desktop does: true,
    false, or null to cancel the save. (`initially`: the default.) */
function askSaveTables(tables: TableSummary["tables"], initially: boolean): Promise<boolean | null> {
  return new Promise((resolve) => {
    const total = tables.reduce((n, t) => n + t.bytes, 0);
    const button = (label: string, value: boolean | null) => {
      const b = el("button", { type: "button", textContent: label });
      b.addEventListener("click", () => { dialog.remove(); resolve(value); });
      return b;
    };
    const withButton = button("Save with tables", true);
    const withoutButton = button("Save without tables", false);
    const dialog = el("div", { className: "page-dialog", role: "dialog" },
      el("div", { className: "page-dialog-box" },
        el("div", { className: "form-title", textContent: "Save table data?" }),
        el("p", { textContent: "This project's tables hold data:" }),
        el("ul", {}, ...tables.map((t) => el("li", { textContent: `${t.name}:  ${t.size.join(" × ")},  ${describeBytes(t.bytes)}` }))),
        el("p", { textContent: `About ${describeBytes(total)} in all, before compression. Without them, the tables are empty when the project is opened again.` }),
        el("div", { className: "form-buttons" }, button("Cancel", null), el("span", { className: "form-gap" }), withoutButton, withButton)));
    dialog.addEventListener("keydown", (e) => { if (e.key === "Escape") { dialog.remove(); resolve(null); } });
    document.body.append(dialog);
    (initially ? withButton : withoutButton).focus();
  });
}

/** The page's own tool, for making a movie along a path (see ViewerView). */
const PATH_TOOL = "Animate along path";

// ---------------------------------------------------------------------
// The project's title, floating in the workspace's corner: a menu of what
// to do with the project, and a dot when it has changes to save.

const titleText = el("span", { className: "project-title-text" });
const titleDot = el("span", { className: "project-title-dot", title: "It has changes to save", hidden: true });
const titleMenu = menuButton(() => [
  { kind: "action", label: "Save project", shortcut: "⌘S", run: () => void save(false) },
  { kind: "action", label: "Save project as…", shortcut: "⇧⌘S", run: () => void save(true) },
  { kind: "action", label: "Close project", run: () => void closeProject() },
  { kind: "action", label: "Copy link", help: "A link that opens this project as it is now", run: copyLink },
  { kind: "separator" },
  { kind: "action", label: "Edit project details…", run: () => send({ type: "details" }) },
  { kind: "toggle", label: "Edit project script", on: editorOpen, toggle: () => (editorOpen ? hideEditor() : showEditor()) },
  { kind: "separator" },
  { kind: "action", label: "Fit windows to the page", run: () => relayout(true) },
  { kind: "separator" },
  { kind: "action", label: "Preferences…", run: showPreferences },
], { content: [titleText, titleDot, el("span", { className: "project-title-arrow", textContent: "▾" })],
     className: "project-title-button", title: "Project", align: "left" });
// (in the workspace, over its windows and under the drawer: see showProject)
const titleEl = el("div", { className: "project-title" }, titleMenu);
// (windows are kept out from under it: as it's renamed, and as the page is resized)
new ResizeObserver(() => desk?.keepAllClear()).observe(titleEl);
window.addEventListener("resize", () => desk?.keepAllClear());

/** The title, and whether there are changes to save; its file's name, as
    its tooltip. */
function showFileName() {
  titleText.textContent = projectTitle;
  titleDot.hidden = !unsaved();
  titleMenu.querySelector("button")!.title = current
    ? `${current.fileName}${current.library ? " (from the library)" : ""}${unsaved() ? ": changes not saved" : ""}`
    : "";
}

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
/** The viewer the tool shortcuts and the tool's panel are for: the one
    last clicked. */
let activeViewer: ViewerView | null = null;
/** Where the active viewer's tool's panel goes (in the side panel). */
let toolPanelEl: HTMLElement | null = null;
let asideEl: HTMLElement | null = null;
/** The desk the viewers' windows are on. */
let desk: Desk | null = null;
/** The open project's viewers' windows, for saving the view state; and
    each one's place on a wide screen, as fractions of the desk, and its
    shape (kept while they're tiled on a narrow screen: what's saved). */
let windows: { title: string; view: ViewerView; window: HTMLElement; layout: number[] | null; shape: number }[] = [];
/** Whether the windows are tiled (on a narrow screen), not placed. */
let tiledNow = false;
/** The desk's size when the windows were last laid out: where they are
    is as fractions of this (the page may have been resized since). */
let laidOutOn = { width: 1, height: 1 };

function deskSize() {
  return { width: desk?.element.clientWidth || window.innerWidth, height: desk?.element.clientHeight || window.innerHeight };
}

/** Where windows go: below the title, inside a margin. */
function deskRoom(): Room {
  const margin = 12, d = deskSize();
  const top = titleEl.offsetTop + titleEl.offsetHeight + margin;
  return { left: margin, top, width: d.width - 2 * margin, height: d.height - top - margin };
}

/** A window's place on the desk now. */
function placeOf(w: { view: ViewerView; window: HTMLElement }): Place {
  const { width, height } = w.view.cssSize();
  return { x: w.window.offsetLeft, y: w.window.offsetTop, width, height };
}

function viewState(): SavedViews {
  const d = laidOutOn;
  return {
    viewers: Object.fromEntries(windows.map((w) => {
      const p = placeOf(w);
      // (tiled: where it goes on a wide screen, if it's been anywhere)
      const where = tiledNow ? (w.layout ? { place: w.layout, shape: w.shape } : {})
                             : { place: toFractions(p, d), shape: Math.round((p.width / p.height) * 10000) / 10000 };
      return [w.title, { ...where, ...w.view.state(), ...(Desk.isMinimized(w.window) ? { minimized: true } : {}) }];
    })),
  };
}

/** Fit the windows to the page, as it is now: tiled, on a narrow screen;
    else as they are, unless they're off it (then shrunk, all alike, to
    fit), or, if `rearrange` (or coming back from being tiled), as they
    were placed. Each viewer shows what it did, at its new size. */
function relayout(rearrange = false) {
  if (!desk || windows.length === 0) return;
  const room = deskRoom(), d = deskSize();
  const minimized = windows.map((w) => Desk.isMinimized(w.window));
  // (going to tiles: where they were, as fractions of the desk they were
  // laid out on, is what's kept)
  if (narrowScreen.matches && !tiledNow) {
    for (const w of windows) {
      const p = placeOf(w);
      w.layout = toFractions(p, laidOutOn);
      w.shape = p.width / p.height;
    }
  }
  let places: Place[];
  if (narrowScreen.matches) places = tiled(minimized, room);
  else if (rearrange || (tiledNow && windows.some((w) => !w.layout))) {
    // (side by side, each with the shape it has now, as big as they fit,
    // up to the project's sizes)
    places = sideBySide(windows.map((w) => {
      const p = placeOf(w);
      return { width: w.view.info.width, height: w.view.info.width * (p.height / p.width) };
    }), room);
  } else if (tiledNow) {
    places = fitted(windows.map((w) => fromFractions(w.layout!, w.shape, d)), room, minimized);
  } else {
    places = fitted(windows.map(placeOf), room, minimized);
  }
  tiledNow = narrowScreen.matches;
  laidOutOn = d;
  windows.forEach((w, i) => {
    const before = placeOf(w), p = places[i];
    if (before.x === p.x && before.y === p.y && before.width === p.width && before.height === p.height) return;
    desk!.place(w.window, p.x, p.y, p.width, p.height);
    if (before.width !== p.width || before.height !== p.height) w.view.fitTo(p.width, p.height);
  });
}

// (when the page changes size: a window resized, a phone turned)
let relayoutTimer: number | undefined;
window.addEventListener("resize", () => {
  clearTimeout(relayoutTimer);
  relayoutTimer = window.setTimeout(() => relayout(), 150);
});

function showProject(title: string, panel: Layout, infos: ViewerInfo[], views: SavedViews | null) {
  fieldInputs.clear();
  viewers.clear();
  windows = [];
  activeViewer = null;
  desk = new Desk(el("div"));
  toolPanelEl = el("div", { className: "tool-panel" });
  asideEl = el("aside", { className: "panel" },
               panelGrip(), el("div", { className: "panel-body" }, panel ? layoutElement(panel, projectFields) : "", toolPanelEl),
               drawerTab());
  workspaceEl = el("div", { className: "workspace" }, desk.element, titleEl, asideEl);
  projectEl.replaceChildren(workspaceEl);
  projectTitle = title;
  showFileName();
  // (the drawer's closed, to begin with)
  setDrawer(false);
  // (no window under the title)
  const dk = desk;
  dk.keepClearOf(() => {
    const t = titleEl.getBoundingClientRect(), r = dk.element.getBoundingClientRect();
    const gap = 8;
    return { left: t.left - r.left + dk.element.scrollLeft - gap, top: t.top - r.top + dk.element.scrollTop - gap,
             right: t.right - r.left + dk.element.scrollLeft + gap, bottom: t.bottom - r.top + dk.element.scrollTop + gap };
  });
  // Each viewer in a window: where it was saved (as fractions of the
  // desk; or, in an older file, where it was in CSS pixels), or else side
  // by side below the title, as big as they fit, up to the project's
  // sizes; and all shrunk alike if they don't fit. On a narrow screen,
  // they're tiled down the page instead (but it's where they'd go on a
  // wide one that's saved). Each shows the region it was saved showing.
  const room = deskRoom(), d = deskSize();
  const saved = infos.map((info) => views?.viewers?.[info.title]);
  const minimized = saved.map((s) => s?.minimized ?? false);
  const defaults = sideBySide(infos.map((i) => ({ width: i.width, height: i.height })), room);
  const wide = fitted(infos.map((_, k): Place => {
    const s = saved[k];
    if (s?.place && s.place.length === 4 && (s.shape ?? 0) > 0) return fromFractions(s.place, s.shape!, d);
    if (s && s.width && s.height && s.x !== undefined && s.y !== undefined) return { x: s.x, y: s.y, width: s.width, height: s.height };
    return defaults[k];
  }), room, minimized);
  tiledNow = narrowScreen.matches;
  laidOutOn = d;
  const places = tiledNow ? tiled(minimized, room) : wide;
  infos.forEach((info, k) => {
    const s = saved[k];
    const place = places[k];
    const v = new ViewerView(info, place.width, place.height, s ? savedView(s) : null);
    viewers.set(info.id, v);
    const win = dk.add({ title: info.title, ...place, content: v.element, minimized: s?.minimized,
                           onResize: (width, height) => v.resize(width, height),
                           menu: () => v.menu(), titleExtra: v.toolChooser ?? undefined });
    win.addEventListener("pointerdown", () => activate(v), { capture: true });
    v.window = win;
    // (where it goes on a wide screen: as saved; else, if it's been placed
    // on one, there; tiled to begin with, nowhere yet)
    windows.push({ title: info.title, view: v, window: win,
                   layout: s?.place?.length === 4 ? s.place : tiledNow ? null : toFractions(wide[k], d),
                   shape: s?.place?.length === 4 && (s.shape ?? 0) > 0 ? s.shape! : wide[k].width / wide[k].height });
  });
  activate(windows[0]?.view ?? null);
}

/** A saved view: its center, and the region it shows (in an older file,
    from its size and a pixel's size). */
function savedView(s: SavedViewer): SavedView | null {
  if (s.region?.length === 2) return { cx: s.cx, cy: s.cy, region: [s.region[0], s.region[1]] };
  const ps = s["pixel-size"];
  if (ps && s.width && s.height) return { cx: s.cx, cy: s.cy, region: [s.width * ps, s.height * ps] };
  return null;
}

/** Make a viewer the active one: the tool shortcuts, and its tool's panel, are its. */
function activate(v: ViewerView | null) {
  if (activeViewer === v) return;
  activeViewer?.window?.classList.remove("active");
  activeViewer = v;
  v?.window?.classList.toggle("active", viewers.size > 1);
  showToolPanel();
}

function showToolPanel() {
  if (!toolPanelEl || !asideEl) return;
  toolPanelEl.replaceChildren(activeViewer?.toolPanel ?? "");
  // (no drawer at all, if there's nothing to put in it)
  asideEl.hidden = asideEl.querySelector(".panel-body :is(fieldset, .field, button, p, .tabs)") === null;
}

// The configuration (and the active viewer's tool's panel), in a drawer
// over the right of the workspace: pulled out, or pushed back, by its tab,
// with a click or a drag (or Escape, from inside it, closes it).

function setDrawer(open: boolean) {
  if (!asideEl) return;
  asideEl.classList.toggle("open", open);
  asideEl.style.transform = "";
  const tab = asideEl.querySelector(".drawer-tab");
  tab?.setAttribute("aria-expanded", String(open));
  tab?.setAttribute("title", open ? "Hide the configuration" : "Show the configuration");
}

function drawerTab(): HTMLElement {
  const tab = el("button", { type: "button", className: "drawer-tab" },
                 el("span", { className: "drawer-tab-label", textContent: "Configuration" }));
  tab.setAttribute("aria-label", "Configuration");
  tab.addEventListener("pointerdown", (e) => {
    if (e.button !== 0 || !asideEl) return;
    e.preventDefault();
    tab.setPointerCapture(e.pointerId);
    const aside = asideEl;
    const width = aside.getBoundingClientRect().width;
    const wasOpen = aside.classList.contains("open");
    const x0 = e.clientX;
    let moved = false, shown = wasOpen ? width : 0;
    aside.classList.add("dragging");
    const move = (m: PointerEvent) => {
      const dx = m.clientX - x0;
      if (Math.abs(dx) > 4) moved = true;
      if (!moved) return;
      // (how much of it is out)
      shown = Math.max(0, Math.min(width, (wasOpen ? width : 0) - dx));
      aside.style.transform = `translateX(${width - shown}px)`;
    };
    const up = () => {
      aside.classList.remove("dragging");
      tab.removeEventListener("pointermove", move);
      tab.removeEventListener("pointerup", up);
      tab.removeEventListener("pointercancel", up);
      // (a click toggles it; a drag leaves it open if it's more than a third out)
      setDrawer(moved ? shown > width / 3 : !wasOpen);
    };
    tab.addEventListener("pointermove", move);
    tab.addEventListener("pointerup", up);
    tab.addEventListener("pointercancel", up);
  });
  // (the keyboard: Space or Return, as a button)
  tab.addEventListener("keydown", (e) => {
    if (e.key === "Enter" || e.key === " ") { e.preventDefault(); setDrawer(!asideEl?.classList.contains("open")); }
  });
  return tab;
}

document.addEventListener("keydown", (e) => {
  if (e.key === "Escape" && asideEl?.classList.contains("open") && asideEl.contains(document.activeElement)) {
    setDrawer(false);
  }
});

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

// The configuration drawer's width: dragged from its left edge, and
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
// configuration), from the title's Edit project script (see editor.ts). A
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
/** Capturing pictures and movies (see capture.ts): loaded the first time
    it's used, since its video muxer (Mediabunny) is most of its size. */
const capture = () => import("./capture.js");
let moviesMade: Promise<Movies> | null = null;
const movies = () => (moviesMade ??= capture().then((c) => new c.Movies(send, say, () =>
  [...viewers.values()].map((v) => ({ id: v.info.id, title: v.info.title, animates: v.info.animates })))));

/** The viewer (by title) whose colorizer was just chosen, if the next
    rebuild is for that (see the "project" message). */
let colorizerChange: string | null = null;

function showEditor() {
  if (!workspaceEl) return;
  if (!scriptEditor) {
    scriptEditor = new ScriptEditor(projectScript, {
      analyze: (id, text) => send({ type: "analyze", id, text }),
      rebuild: (text) => send({ type: "rebuild", text }),
    }, preferences());
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
  scriptEditor.focus();
}

function hideEditor() {
  editorOverlay?.remove();
  editorOpen = false;
}

// ---------------------------------------------------------------------
// The autocolor editor: the project's autocolor groups, over the
// workspace (as the script editor is), as the desktop's Autocolor window
// shows them: pick a group; lock it, blend its colors for points not in
// it, clear it; its tolerance and the color for points not in it; and its
// points, each moved, recolored or deleted, and new ones added. The
// engine posts the groups as they change (renders add points) while it's
// open; the list isn't rebuilt while one of its fields is being typed in.

let autocolorOverlay: HTMLElement | null = null;
let autocolorGroups: AutocolorGroup[] = [];
let autocolorGroup: number | null = null;
/** The groups' last rendering, to rebuild only what's changed. */
let autocolorShown = "";

function showAutocolor() {
  if (!workspaceEl) return;
  if (!autocolorOverlay) {
    const close = el("button", { type: "button", className: "editor-close", textContent: "Close" });
    close.addEventListener("click", hideAutocolor);
    autocolorOverlay = el("div", { className: "editor-overlay autocolor-overlay", role: "dialog" },
      el("div", { className: "editor-header" }, el("span", { className: "editor-title", textContent: "Autocolor" }), close),
      el("div", { className: "autocolor-body" }));
    autocolorOverlay.addEventListener("keydown", (e) => { if (e.key === "Escape") hideAutocolor(); });
    // (a field typed in: once it's left, the list can be rebuilt)
    autocolorOverlay.addEventListener("focusout", () => setTimeout(renderAutocolor, 0));
  }
  autocolorShown = "";
  autocolorOverlay.querySelector(".autocolor-body")!.replaceChildren(
    el("p", { className: "autocolor-status", textContent: "…" }));
  workspaceEl.append(autocolorOverlay);
  send({ type: "autocolorWatch", on: true });
}

function hideAutocolor() {
  if (!autocolorOverlay?.isConnected) return;
  autocolorOverlay.remove();
  send({ type: "autocolorWatch", on: false });
}

function autocolorEdit(edit: AutocolorEdit) {
  autocolorSay("");
  send({ type: "autocolorEdit", edit });
}

function autocolorSay(text: string) {
  const s = autocolorOverlay?.querySelector(".autocolor-status");
  if (s) s.textContent = text;
}

function renderAutocolor() {
  const body = autocolorOverlay?.querySelector(".autocolor-body");
  if (!body || !autocolorOverlay!.isConnected) return;
  // (not while a field's being typed in: it'd be wiped)
  const active = document.activeElement;
  if (active instanceof HTMLInputElement && active.type === "text" && autocolorOverlay!.contains(active)) return;
  const groups = autocolorGroups;
  if (autocolorGroup === null || !groups.some((g) => g.id === autocolorGroup)) autocolorGroup = groups[0]?.id ?? null;
  const key = JSON.stringify([autocolorGroup, groups]);
  if (key === autocolorShown) return;
  autocolorShown = key;
  const status = el("p", { className: "autocolor-status" });
  const g = groups.find((x) => x.id === autocolorGroup);
  if (!g) {
    status.textContent = "No viewer has used autocolor yet: a group appears here once one does.";
    body.replaceChildren(status);
    return;
  }
  const id = g.id;
  const chooser = el("select", {}, ...groups.map((x) => el("option", { value: String(x.id), textContent: `Group ${x.id}`, selected: x.id === id })));
  chooser.addEventListener("change", () => { autocolorGroup = Number(chooser.value); renderAutocolor(); });
  const check = (label: string, on: boolean, title: string, op: "locked" | "blend") => {
    const box = el("input", { type: "checkbox", checked: on });
    box.addEventListener("change", () => autocolorEdit({ op, group: id, on: box.checked }));
    return el("label", { className: "autocolor-check", title }, box, ` ${label}`);
  };
  const clear = el("button", { type: "button", textContent: "Clear all" });
  clear.addEventListener("click", () => autocolorEdit({ op: "clear", group: id }));
  const tolerance = el("input", { type: "text", value: g.tolerance, spellcheck: false,
                                  title: "How near a point has to be to an entry to get its color" });
  const commitTolerance = () => { if (tolerance.value !== g.tolerance) autocolorEdit({ op: "tolerance", group: id, text: tolerance.value }); };
  tolerance.addEventListener("keydown", (e) => { if (e.key === "Enter") commitTolerance(); });
  tolerance.addEventListener("blur", commitTolerance);
  const miss = el("input", { type: "color", value: g.miss, title: "The color for points not in the group" });
  miss.addEventListener("change", () => autocolorEdit({ op: "miss", group: id, color: miss.value }));
  const rows = g.entries.map((p) => {
    const pt = el("input", { type: "text", value: p.point, spellcheck: false });
    const commit = () => { if (pt.value !== p.point) autocolorEdit({ op: "move", group: id, x: p.x, y: p.y, text: pt.value }); };
    pt.addEventListener("keydown", (e) => { if (e.key === "Enter") commit(); });
    pt.addEventListener("blur", commit);
    const color = el("input", { type: "color", value: p.color });
    color.addEventListener("change", () => autocolorEdit({ op: "color", group: id, x: p.x, y: p.y, color: color.value }));
    const del = el("button", { type: "button", textContent: "Delete" });
    del.addEventListener("click", () => autocolorEdit({ op: "delete", group: id, x: p.x, y: p.y }));
    return el("div", { className: "autocolor-entry" }, pt, color, del);
  });
  const newPoint = el("input", { type: "text", spellcheck: false, placeholder: "a point, like 0.5 + 0.25𝑖" });
  const add = el("button", { type: "button", textContent: "Add" });
  const addIt = () => { if (newPoint.value.trim()) { autocolorEdit({ op: "add", group: id, text: newPoint.value }); newPoint.value = ""; } };
  add.addEventListener("click", addIt);
  newPoint.addEventListener("keydown", (e) => { if (e.key === "Enter") addIt(); });
  body.replaceChildren(
    el("div", { className: "autocolor-row" }, chooser,
       check("Lock", g.locked, "Add no new points to this group", "locked"), clear),
    el("div", { className: "autocolor-grid" },
       el("span", { textContent: "Tolerance:" }), tolerance,
       el("span", { textContent: "Color for points not in the group:" }), el("span", {}, miss)),
    el("div", { className: "autocolor-row" },
       check("Blend when no matching point", g.blend,
             "When the group is locked (or full), give points that aren't near any of its points a blend of its colors, rather than the color for points not in the group",
             "blend")),
    el("div", { className: "autocolor-list" },
       ...(rows.length ? rows : [el("p", { className: "autocolor-empty", textContent: "No points yet." })])),
    el("div", { className: "autocolor-row" }, el("span", { textContent: "New point:" }), newPoint, add),
    status);
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

/** A viewer's view, as it was saved: its center, and the region it showed. */
interface SavedView { cx: number; cy: number; region: [number, number] }

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
  /** A Shift-drag's box to zoom into (see `zoomToBox`), as it's drawn. */
  private boxing = false;
  private readonly boxEl: HTMLElement;
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
  /** Its tool chooser, in its title bar (if it has tools): the tool it
      has, as a menu of its tools, and Navigate. */
  readonly toolChooser: HTMLElement | null;
  private readonly toolLabel: HTMLElement;
  /** A tool's been chosen from its chooser (or with its shortcut): its
      panel, if it has one, opens the drawer. */
  private toolChosen = false;
  /** The "Animate along path" tool (the page's own, as on the desktop: the
      engine's navigating meanwhile), where the viewer's coordinate is a
      configuration variable (`info.animates`): a drag draws a path, in
      screen-plane coordinates, and letting go asks how to make a movie
      along it. The path stays until the next, or another tool. */
  private pathTool = false;
  private path: [number, number][] | null = null;
  /** A press with the tool: its drag starts a new path. */
  private pathStarts = false;

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
              private readonly saved: SavedView | null) {
    this.canvas = el("canvas");
    this.context = this.canvas.getContext("2d")!;
    this.errorEl = el("div", { className: "viewer-error", hidden: true });
    this.runEl = el("div", { className: "viewer-running", hidden: true, role: "status" });
    this.pointerEl = el("div", { className: "viewer-pointer", hidden: true, textContent: "…" });
    this.boxEl = el("div", { className: "zoom-box", hidden: true });
    this.element = el("div", { className: "viewer" }, this.canvas, this.boxEl, this.errorEl, this.runEl, this.pointerEl);
    this.toolLabel = el("span", { className: "tool-chooser-label", textContent: "Navigate" });
    this.toolChooser = info.tools.length === 0 && !info.animates ? null : menuButton(() => [
      { kind: "toggle", label: "Navigate", on: this.toolName === null && !this.pathTool, shortcut: "n",
        help: "Drag to move around; scroll or pinch to zoom", toggle: () => this.selectTool(null) },
      ...info.tools.map((t): MenuItem => ({ kind: "toggle", label: t.name, on: this.toolName === t.name,
                                             shortcut: t.shortcut || undefined, help: t.help || undefined,
                                             toggle: () => this.selectTool(t.name) })),
      ...(info.animates ? [{ kind: "toggle", label: PATH_TOOL, on: this.pathTool,
                             help: `Drag a path for ${info.animates} to move along, in a movie`,
                             toggle: () => this.selectPathTool() } as MenuItem] : []),
    ], { content: [this.toolLabel, el("span", { className: "tool-chooser-arrow", textContent: "▾" })],
         className: "tool-chooser", title: "Tool" });
    this.toolChooser?.classList.add("tool-chooser-wrap");
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

  /** Its size, in CSS pixels. */
  cssSize() {
    return { width: Math.round(this.canvas.width / this.density), height: Math.round(this.canvas.height / this.density) };
  }

  /** The region it shows, in the model's units (width and height): what
      it's asked to show, or, before it's shown anything, what it was
      saved showing. */
  region(): [number, number] | null {
    const t = this.target;
    if (t) return [this.canvas.width * t.px, this.canvas.height * t.py];
    return this.saved?.region ?? null;
  }

  /** What it shows, for saving (see SavedViews). */
  state() {
    const t = this.target, r = this.region();
    return { cx: t?.cx ?? this.saved?.cx ?? 0, cy: t?.cy ?? this.saved?.cy ?? 0, ...(r ? { region: r } : {}) };
  }

  /** Show it at this size (in CSS pixels), as the windows are fitted to
      the page: showing what it did (all of it, centered), unlike a
      resize, which shows more or less of the view at the same scale. */
  fitTo(width: number, height: number) {
    const t = this.target, r = this.region();
    this.setSize(width, height);
    if (t && r) {
      const ratio = t.py / t.px;
      const px = Math.max(r[0] / this.canvas.width, r[1] / (this.canvas.height * ratio));
      this.target = { ...t, px, py: px * ratio };
    }
    // (with no frame yet, the first's made to show the saved region, or
    // the project's whole view, at the size it has then)
    this.redraw();
    this.request();
  }

  /** Ask the engine to select a tool (null: navigate), as the user's chosen. */
  selectTool(name: string | null) {
    this.toolChosen = true;
    this.setPathTool(false);
    send({ type: "selectTool", viewer: this.info.id, name: name ?? "" });
  }

  /** Choose "Animate along path" (the engine's tool, if any, put down). */
  private selectPathTool() {
    if (this.toolName !== null) send({ type: "selectTool", viewer: this.info.id, name: "" });
    this.setPathTool(true);
  }

  private setPathTool(on: boolean) {
    if (!on && this.path) { this.path = null; this.redraw(); }
    this.pathTool = on;
    this.element.classList.toggle("tool", on || this.toolName !== null);
    this.toolLabel.textContent = on ? PATH_TOOL : this.toolName ?? "Navigate";
  }

  /** The path being drawn, or drawn last, if any (screen-plane points). */
  get moviePath() { return this.path; }

  /** The engine selected a tool. */
  toolSelected(name: string | null, panel: Layout, handles: Handled[]) {
    this.toolName = name;
    if (name !== null) this.setPathTool(false);
    this.handles = new Set(handles);
    this.element.classList.toggle("tool", name !== null);
    this.events = [];
    for (const key of [...fieldInputs.keys()]) if (key.startsWith(`tool:${this.info.id}:`)) fieldInputs.delete(key);
    const id = this.info.id;
    this.toolPanel = panel === null ? null : el("div", {}, layoutElement(panel, {
      set: (fieldName, text) => send({ type: "setTool", viewer: id, name: fieldName, text }),
      button: (label) => send({ type: "button", viewer: id, label }),
    }));
    this.toolLabel.textContent = this.pathTool ? PATH_TOOL : name ?? "Navigate";
    this.element.classList.toggle("tool", this.pathTool || name !== null);
    this.toolChooser?.querySelector("button")?.setAttribute("title", `Tool: ${this.toolLabel.textContent}`);
    if (activeViewer === this) {
      showToolPanel();
      // (a tool chosen with a panel: the drawer's opened, to show it)
      if (this.toolChosen && this.toolPanel) setDrawer(true);
    }
    this.toolChosen = false;
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
        // (as it was saved: the region it showed, all of it, at its size
        // now; on the sphere, the engine's turned the globe as it was too)
        const ratio = view.py / view.px, [rw, rh] = this.saved.region;
        const ps = Math.max(rw / this.canvas.width, rh / (this.canvas.height * ratio));
        this.target = { cx: this.saved.cx, cy: this.saved.cy, px: ps, py: ps * ratio };
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
      const toPx = ([x, y]: [number, number]): [number, number] => [(x - t.cx) / t.px + W / 2, (t.cy - y) / t.py + H / 2];
      if (this.layers.length) drawLayers(ctx, this.layers, toPx, [t.px, t.py], this.density);
      if (this.path && this.path.length > 1) {
        // (as the desktop draws it: white, with a dark edge, to show on anything)
        ctx.save();
        ctx.lineCap = "round";
        ctx.lineJoin = "round";
        ctx.beginPath();
        this.path.map(toPx).forEach(([x, y], i) => (i ? ctx.lineTo(x, y) : ctx.moveTo(x, y)));
        ctx.strokeStyle = "rgba(0, 0, 0, 0.75)";
        ctx.lineWidth = 4 * this.density;
        ctx.stroke();
        ctx.strokeStyle = "white";
        ctx.lineWidth = 2 * this.density;
        ctx.stroke();
        ctx.restore();
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
  private get toolDrags() { return this.pathTool || this.handles.has("drag") || this.handles.has("dragDone"); }

  private startDrag(e: PointerEvent) {
    if (!this.target || e.button !== 0) return;
    this.canvas.setPointerCapture(e.pointerId);
    this.press = { start: this.pixel(e), dragging: false };
    this.dragFrom = this.press.start;
    this.pathStarts = this.pathTool;
    // (Shift and drag: a box to zoom into, on the plane, as on the desktop)
    this.boxing = e.shiftKey && !this.info.sphere && !this.toolDrags;
    if (this.boxing) return;
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
    if (this.boxing) {
      this.boxing = false;
      this.boxEl.hidden = true;
      if (press.dragging) this.zoomToBox(press.start, p);
      return;
    }
    if (this.pathTool) {
      // (a movie along it, if it's a path: Movies asks how)
      if (press.dragging && this.path && this.path.length > 1) {
        const path = this.path, id = this.info.id;
        void movies().then((m) => m.askPath(id, path));
      }
      return;
    }
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
    if (this.boxing) {
      // (the box, in CSS pixels over the canvas)
      const a = this.press.start, d = this.density;
      Object.assign(this.boxEl.style, {
        left: `${Math.min(a.x, p.x) / d}px`, top: `${Math.min(a.y, p.y) / d}px`,
        width: `${Math.abs(p.x - a.x) / d}px`, height: `${Math.abs(p.y - a.y) / d}px` });
      this.boxEl.hidden = false;
      return;
    }
    if (this.pathTool) {
      // (a point every couple of pixels, as the desktop has it)
      const z = this.toPlane(p);
      if (this.pathStarts) { this.path = [this.toPlane(this.press.start)]; this.pathStarts = false; }
      if (!this.path) return;
      const [x, y] = this.path[this.path.length - 1];
      if (Math.hypot((z[0] - x) / t.px, (z[1] - y) / t.py) >= 2 * this.density) {
        this.path.push(z);
        this.redraw();
      }
      return;
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

  /** Zoom into a box (from one corner to the other, in canvas pixels), as
      the desktop does: its center the view's, scaled so that its area is
      the canvas's (unless it's too small to mean anything). A step in the
      history. */
  private zoomToBox(a: { x: number; y: number }, b: { x: number; y: number }) {
    const t = this.target!;
    const W = this.canvas.width, H = this.canvas.height;
    const scale = Math.sqrt((Math.abs(b.x - a.x) * Math.abs(b.y - a.y)) / (W * H));
    if (!(scale >= 0.001)) return;
    const [mx, my] = this.toPlane({ x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 });
    t.cx = mx;
    t.cy = my;
    t.px *= scale;
    t.py *= scale;
    this.redraw();
    this.request();
    this.recordSoon(0);
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
          toggle: () => {
            if (i.inUse) return;
            colorizerChange = this.info.title;
            send({ type: "colorizer", viewer: this.info.id, name: i.name });
          } })),
        { kind: "separator" },
        { kind: "action", label: "Arguments…", enabled: c.arguments,
          run: () => send({ type: "colorizerArguments", viewer: this.info.id }) },
        // (the project's, not this viewer's: kept here, as on the desktop,
        // so that the menu stays short)
        { kind: "action", label: "Autocolor settings…", run: () => showAutocolor() },
      ] });
    }
    items.push(
      { kind: "separator" },
      // (what it shows: a picture, or a movie)
      { kind: "submenu", label: "Capture", items: () => [
        { kind: "action", label: "Image…", run: () => void capture().then((c) => c.askImage(this.info.id, send)) },
        { kind: "action", label: "Movie…", run: () => void movies().then((m) => m.ask(this.info.id, this.info.title)) },
      ] },
      { kind: "action", label: "Use this as preview", help: "Make what this viewer shows the project's preview, in the library",
        run: () => send({ type: "useAsPreview", viewer: this.info.id }) },
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
    if (m.sphere || this.info.sphere) {
      this.frame = null;
      // (a path drawn on the globe, or on the plane before it, can't follow)
      this.path = null;
    }
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
              colorizerChange = this.info.title;
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

onPreferences((p) => {
  sendPreferences(p);
  scriptEditor?.setPreferences(p);
});

// ---------------------------------------------------------------------

engine.addEventListener("message", (e: MessageEvent<Message>) => {
  const m = e.data;
  switch (m.type) {
    case "ready":
      engineReady = true;
      sendPreferences(preferences());
      send({ type: "density", value: density() });
      send({ type: "symbols" });
      send({ type: "library" });
      break;
    case "library": welcome.show(m.sections); libraryLoaded = true; void openLink(); break;
    case "share": void linkMade(m); break;
    case "project":
      if (m.rebuilt) {
        // (the same project, built again from its edited script: its
        // windows stay as they were, and so does the editor)
        // (and, for one whose script hadn't built, so had no windows yet,
        // where its file had them)
        const views = { viewers: { ...m.views?.viewers, ...viewState().viewers } };
        // (an open autocolor editor stays open: the groups are the same)
        const autocolorOpen = autocolorOverlay?.isConnected ?? false;
        // (and so does the drawer, open or not)
        const drawerOpen = asideEl?.classList.contains("open") ?? false;
        showProject(m.title, m.panel, m.viewers, views);
        setDrawer(drawerOpen);
        if (autocolorOpen && autocolorOverlay && workspaceEl) workspaceEl.append(autocolorOverlay);
        scriptEditor?.projectScript(m.script, true);
        // (it worked: the editor's done its job)
        hideEditor();
        // (for a colorizer, which one the viewer uses now)
        const changed = colorizerChange && m.viewers.find((v) => v.title === colorizerChange);
        say(changed ? `${changed.title}: colored by ${changed.colorizer?.inUse ?? "its script"}.` : "Rebuilt.");
        colorizerChange = null;
        break;
      }
      // (a new project has no file of its own yet: not even the one it
      // was translated from)
      // (a new project: a new script, and no editor until it's asked for;
      // nor the autocolor editor)
      forgetProject();
      current = { fileName: m.fileName, library: m.library, handle: m.library || m.new ? null : opening,
                  saveTables: m.savedTables ? true : null };
      opening = null;
      welcome.setOpening(false);
      showFileName();
      projectScript = m.script;
      showScreen(true);
      enterHistory();
      showProject(m.title, m.panel, m.viewers, m.views);
      if (m.notes.length) notice(`Translation notes:\n${m.notes.map((n) => `• ${n}`).join("\n")}`, "info");
      // (its editor says why, if its script doesn't build)
      if (m.edit) showEditor();
      break;
    case "ancient":
      opening = null;
      welcome.setOpening(false);
      notice(`“${m.name}” looks like a file from an old version of FractalStream (1.0). ` +
             `Translate it into a new project? (The old file is left as it is.)`, "info",
             [{ label: "Translate", run: () => { welcome.setOpening(true); send({ type: "translate" }); } }]);
      break;
    case "pointer": viewers.get(m.viewer)?.showPointer(m.text); break;
    case "view": viewers.get(m.viewer)?.moved(m); break;
    case "where": viewers.get(m.viewer)?.jumpFrom(m.x, m.y, m.pixelSize); break;
    case "aspect": viewers.get(m.viewer)?.aspectAnswered(m.error); break;
    case "tableSummary": tableSummaryWaiting?.(m); tableSummaryWaiting = null; break;
    case "download": void capture().then((c) => c.saveDownload(m)); break;
    case "movieTemplate": void movies().then((v) => v.template(m)); break;
    case "movieCheck": void movies().then((v) => v.checked(m)); break;
    case "movieFrame": void movies().then((v) => v.frame(m)); break;
    case "movieDone": void movies().then((v) => v.done(m)); break;
    case "autocolor": autocolorGroups = m.groups; renderAutocolor(); break;
    case "autocolorError": autocolorSay(m.message); break;
    case "colorizerArguments": viewers.get(m.viewer)?.askArguments(m.name, m.fields); break;
    case "analysis": scriptEditor?.analysis(m.id, m); break;
    case "symbols": symbolTables = [m.names, m.accents]; scriptEditor?.setSymbols(m.names, m.accents); break;
    case "rebuildError": scriptEditor?.rebuildFailed(m.error); break;
    case "file": {
      const write = pendingSave;
      pendingSave = null;
      if (write) write.write(m.name, m.bytes).then((name) => { say(`Saved ${name}.`); write.done(true); },
                                                    (e) => {
                                                      say(`Couldn't save: ${e}`, true);
                                                      // (it still has changes to save)
                                                      send({ type: "unsaved", on: true });
                                                      write.done(false);
                                                    });
      break;
    }
    case "unsaved": engineUnsaved = m.on; showFileName(); break;
    case "field": fieldResult(m.key, m.error, m.color); break;
    case "fieldText": fieldText(m.key, m.text, m.color); break;
    case "tool": viewers.get(m.viewer)?.toolSelected(m.name, m.panel, m.handles); break;
    case "toolError": say(m.message, true); console.error(m.message); break;
    case "drawing": viewers.get(m.viewer)?.showDrawing(m.layers); break;
    case "eventDone": viewers.get(m.viewer)?.eventDone(); break;
    case "running": viewers.get(m.viewer)?.showRunning(m.what, m.name); break;
    case "runDone": viewers.get(m.viewer)?.runDone(); break;
    case "viewerError": viewers.get(m.viewer)?.showError(m.message); break;
    case "error": colorizerChange = null; welcome.setOpening(false); say(m.message, true); console.error(m.message); break;
    case "details": showDetails(m); break;
    case "patch": viewers.get(m.viewer)?.patch(m); break;
  }
});
engine.addEventListener("error", (e) => { welcome.setOpening(false); say(`The engine failed: ${e.message}`, true); });
// (drawings' text is in the core's font: redraw once it's loaded)
document.fonts.load(`13px "DejaVu Sans"`).then(() => viewers.forEach((v) => v.showDrawing(v.drawing)));
