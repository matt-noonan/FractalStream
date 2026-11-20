// The script editor: a project's whole script, in CodeMirror, as the
// desktop app's editor (UI.ProjectScriptEditor) has it.
//
// - It's colored, and its error marked, by the engine (Language.Editor.
//   Highlight, and checking the script), a moment after each change.
// - Once it checks out, it's written with mathematical symbols (unless
//   the preferences say not to)
//   (Language.Editor.Normalize: z^2 as z², := as ≔), except the line the
//   cursor's on, until the cursor leaves it (so that what's being typed
//   isn't rewritten under it), and only once it's been edited (so that
//   just opening a script doesn't change it).
// - Symbols can be typed by their names (symbols.ts): \theta and a space
//   is θ and a space; Tab completes a name, Return finishes one, Escape
//   leaves it as typed; pasted names become their symbols.
// - Return indents the new line as the one it breaks, and a level
//   further after a ":" (as the desktop's does).
// - Rebuild (or Ctrl/Cmd-Enter) builds the project again from it (and if
//   that works, the page closes the editor); Revert goes back to the
//   project's script.
// - Its preferences (preferences.ts): line numbers, writing with
//   symbols, emacs keys, and which blocks fold (stanzas and tool handlers
//   always do, as on the desktop: by their indentation, UI.CodeEditor's
//   foldLevelsForText); its font and colors are the page's stylesheet's.

import { Compartment, EditorSelection, EditorState, Prec, type StateCommand, StateEffect, StateField, RangeSet, RangeSetBuilder,
         Transaction, type Extension } from "@codemirror/state";
import { EditorView, Decoration, type DecorationSet, gutter, GutterMarker, keymap, lineNumbers, showTooltip, type Tooltip,
         ViewPlugin, type ViewUpdate, WidgetType } from "@codemirror/view";
import { defaultKeymap, emacsStyleKeymap, history, historyKeymap, indentWithTab, isolateHistory } from "@codemirror/commands";
import { codeFolding, foldedRanges, foldGutter, foldKeymap, foldService, unfoldAll, unfoldEffect } from "@codemirror/language";
import { isMac, type Preferences } from "./preferences.js";
import type { ScriptError } from "./protocol.js";
import { Symbols, typedName } from "./symbols.js";

export interface EditorHost {
  analyze(id: number, text: string): void;
  rebuild(text: string): void;
}

/** An analysis of the text, from the engine. */
interface Analysis {
  spans: [number, number, string][];
  error: ScriptError | null;
  normalizations: [number, number, string][];
}

// --- decorations: the engine's colors, and the error

const setColors = StateEffect.define<DecorationSet>();
const colors = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(deco, tr) {
    deco = deco.map(tr.changes);
    for (const e of tr.effects) if (e.is(setColors)) deco = e.value;
    return deco;
  },
  provide: (f) => EditorView.decorations.from(f),
});

// --- the error shown: underlined, its line marked in the gutter, and its
// message in a note under its line (if the note's shown). Its place moves
// with the text as it's edited, until the next analysis.

interface ShownError {
  /** Where it is, if it says (the end not included). */
  from: number | null;
  to: number | null;
  message: string;
  /** Found in rebuilding, rather than as it was typed. */
  rebuild: boolean;
  note: boolean;
}

const setShownError = StateEffect.define<ShownError | null>();
const setNote = StateEffect.define<boolean>();
const shownError = StateField.define<ShownError | null>({
  create: () => null,
  update(e, tr) {
    if (e && tr.docChanged && e.from !== null && e.to !== null)
      e = { ...e, from: tr.changes.mapPos(e.from), to: tr.changes.mapPos(e.to, 1) };
    for (const x of tr.effects) {
      if (x.is(setShownError)) e = x.value;
      else if (x.is(setNote) && e) e = { ...e, note: x.value };
    }
    return e;
  },
});

/** Where an error is in a text as it is: at least a character (an error
    at the very end, at the last one), or nowhere. */
function errorRange(state: EditorState, e: ShownError | null): { from: number; to: number } | null {
  if (!e || e.from === null || e.to === null || state.doc.length === 0) return null;
  const from = Math.min(e.from, state.doc.length - 1);
  return { from, to: Math.min(Math.max(e.to, from + 1), state.doc.length) };
}

/** An error's note: its message in a box, with an arrow up at the error
    (put in place by placeNote, once it's been drawn). */
class NoteWidget extends WidgetType {
  constructor(readonly message: string) { super(); }
  eq(other: NoteWidget) { return other.message === this.message; }
  toDOM() {
    return el("div", { className: "cm-error-note-row" },
              el("div", { className: "cm-error-note" },
                 el("div", { className: "cm-error-note-arrow" }), el("div", { textContent: this.message })));
  }
  // (its text can be selected, and copied)
  ignoreEvent() { return true; }
}

const errorDecorations = EditorView.decorations.compute([shownError], (state) => {
  const e = state.field(shownError);
  const r = errorRange(state, e);
  if (!e || !r) return Decoration.none;
  const marks = [Decoration.mark({ class: "cm-script-error" }).range(r.from, r.to)];
  if (e.note) marks.push(Decoration.widget({ widget: new NoteWidget(e.message), block: true, side: 1 })
                                   .range(state.doc.lineAt(r.from).to));
  return Decoration.set(marks, true);
});

/** The note's box under the error (as far as it fits), and its arrow at it. */
const placeNote = ViewPlugin.fromClass(class {
  constructor(view: EditorView) { this.place(view); }
  update(u: ViewUpdate) {
    if (u.docChanged || u.geometryChanged || u.viewportChanged || u.startState.field(shownError) !== u.state.field(shownError))
      this.place(u.view);
  }
  place(view: EditorView) {
    view.requestMeasure({
      read: (v) => {
        const r = errorRange(v.state, v.state.field(shownError));
        const box = v.contentDOM.querySelector<HTMLElement>(".cm-error-note");
        if (!r || !box || !box.parentElement) return null;
        const at = v.coordsAtPos(r.from, 1);
        const row = box.parentElement.getBoundingClientRect();
        return { box, x: at ? at.left - row.left : 0, rowWidth: row.width, boxWidth: box.offsetWidth };
      },
      write: (m) => {
        if (!m) return;
        const left = Math.max(0, Math.min(m.x - 16, m.rowWidth - m.boxWidth));
        m.box.style.marginLeft = `${left}px`;
        const arrow = m.box.querySelector<HTMLElement>(".cm-error-note-arrow");
        if (arrow) arrow.style.left = `${Math.max(4, m.x - left)}px`;
      },
    });
  }
});

const errorDot = new class extends GutterMarker {
  toDOM() { return el("div", { className: "cm-error-dot" }); }
};

// --- symbol entry: the escape being typed (from its backslash), if any

const setEscape = StateEffect.define<number | null>();
const escapeStart = StateField.define<number | null>({
  create: () => null,
  update(start, tr) {
    if (start !== null) start = tr.changes.mapPos(start, 1);
    for (const e of tr.effects) if (e.is(setEscape)) start = e.value;
    // (gone, if the cursor's left it)
    if (start !== null) {
      const head = tr.state.selection.main.head;
      if (head <= start || !tr.state.selection.main.empty || tr.state.doc.sliceString(start, start + 1) !== "\\") start = null;
    }
    return start;
  },
});

// --- folding: a line folds if the lines after it are indented deeper
// (blank lines aside), up to the last of them; a stanza's header (at the
// left margin, as its body is) folds its whole stanza. Stanzas and tool
// handlers (`on click:`, ...) always fold; other blocks only if asked.

/** A line's depth: a stanza's header's is its indentation; every other
    line's is deeper than any header's. */
function depth(text: string): number {
  const spaces = text.length - text.trimStart().length;
  return text.startsWith("@") ? spaces : 1000 + spaces;
}

function folds(codeBlocks: boolean): Extension {
  return foldService.of((state, lineStart) => {
    const line = state.doc.lineAt(lineStart);
    const text = line.text;
    if (!text.trim() || !(codeBlocks || text.startsWith("@") || text.startsWith("on "))) return null;
    const d = depth(text);
    let end = -1;
    for (let n = line.number + 1; n <= state.doc.lines; n++) {
      const l = state.doc.line(n);
      if (!l.text.trim()) continue;
      if (depth(l.text) <= d) break;
      end = l.to;
    }
    // (not the blank lines after it: folded stanzas keep the space between them)
    return end < 0 ? null : { from: line.to, to: end };
  });
}

// --- Return: a new line, indented as the line it breaks is, and one level
// further if that opens a block (ends with ":", not counting a comment),
// as the desktop's (Language.Editor.newLineIndent, UI.EditorKeys.newLine)

/** How many spaces one level of indentation is (Language.Editor.indentUnit). */
const INDENT_UNIT = 2;

/** A line without its comment: from a "#" that isn't in quotes on. */
function withoutComment(line: string): string {
  let inQuotes = false;
  for (let i = 0; i < line.length; i++) {
    const c = line[i];
    if (c === "#" && !inQuotes) return line.slice(0, i);
    if (c === "\\" && inQuotes) i++;
    else if (c === '"') inQuotes = !inQuotes;
  }
  return line;
}

/** How far to indent the line a break starts, given what's before the
    break on its line. */
export function newLineIndent(before: string): number {
  const indent = before.length - before.replace(/^ +/, "").length;
  return withoutComment(before).trimEnd().endsWith(":") ? indent + INDENT_UNIT : indent;
}

/** Break the line at each cursor (replacing what's selected): the spaces
    around the break go, as they'd only be left trailing, or add to the
    indentation. In a line's indentation, the new line's is as deep. */
const newLine: StateCommand = ({ state, dispatch }) => {
  dispatch(state.update(state.changeByRange((range) => {
    const line = state.doc.lineAt(range.from);
    const before = line.text.slice(0, range.from - line.from);
    const after = state.doc.sliceString(range.to, state.doc.lineAt(range.to).to);
    const allSpaces = /^ *$/.test(before);
    const indent = allSpaces ? before.length : newLineIndent(before);
    const from = allSpaces ? range.from : range.from - (before.length - before.trimEnd().length);
    const to = range.to + (after.length - after.trimStart().length);
    const insert = "\n" + " ".repeat(indent);
    return { changes: { from, to, insert }, range: EditorSelection.cursor(from + insert.length) };
  }), { scrollIntoView: true, userEvent: "input" }));
  return true;
};

export class ScriptEditor {
  readonly element: HTMLElement;
  private readonly view: EditorView;
  private readonly status: HTMLElement;
  private symbols: Symbols | null = null;
  private script: string;
  private requested = 0;
  /** The text the last analysis was asked for. */
  private sentText = "";
  private analyzed: { text: string; analysis: Analysis } | null = null;
  private timer: number | undefined;
  private edited = false;
  private caretLine = 0;
  /** The error the last analysis found, if any (in the text it was of). */
  private liveError: ScriptError | null = null;
  /** The analysis's error that's shown (see admitLiveError), and when the
      text was last changed (performance.now()). */
  private liveShown: ScriptError | null = null;
  private lastEdit = 0;
  private waitTimer: number | undefined;
  /** The error rebuilding found, if any, with the text it was found in:
      shown while that's the text (and there's no other). */
  private rebuildError: { text: string; error: ScriptError } | null = null;
  /** Whether an error's note is shown, if that's been chosen (by clicking
      its mark, or going to it), until there's no error. */
  private noteChosen: boolean | null = null;
  private readonly numbering = new Compartment();
  private readonly folding = new Compartment();
  private readonly emacs = new Compartment();

  constructor(script: string, private readonly host: EditorHost, private prefs: Preferences) {
    this.script = script;
    this.status = el("div", { className: "editor-status" });
    this.status.addEventListener("click", () => { if (this.status.classList.contains("clickable")) this.goToError(); });
    const rebuild = el("button", { type: "button", textContent: "Run script",
                                   title: "Build the project again from this script (Ctrl/Cmd-Enter)" });
    const revert = el("button", { type: "button", textContent: "Revert", title: "Go back to the project's script" });
    rebuild.addEventListener("click", () => this.rebuild());
    revert.addEventListener("click", () => this.revert());
    const holder = el("div", { className: "editor-text" });
    this.element = el("div", { className: "script-editor" }, holder, this.status,
                      el("div", { className: "editor-buttons" }, rebuild, revert));
    this.view = new EditorView({ parent: holder, state: this.newState(script) });
    this.reanalyze();
  }

  /** The symbols' names, from the engine. */
  setSymbols(names: [string, string][], accents: [string, string][]) {
    this.symbols = new Symbols(names, accents);
  }

  /** The project was built (again) from a script: it's the script now. */
  projectScript(script: string, rebuilt: boolean) {
    this.script = script;
    if (rebuilt) this.say("Rebuilt.", false);
  }

  /** Rebuilding found an error (one the analysis as it's typed didn't):
      it's shown, and gone to, until the script's changed. */
  rebuildFailed(error: ScriptError) {
    this.rebuildError = { text: this.view.state.doc.toString(), error };
    this.showError();
    this.goToError();
  }

  /** The error there is now, if any: the analysis's, or rebuilding's (for
      the text as it is). */
  private currentError(): { error: ScriptError; rebuild: boolean } | null {
    if (this.liveShown) return { error: this.liveShown, rebuild: false };
    const r = this.rebuildError;
    if (r && r.text === this.view.state.doc.toString()) return { error: r.error, rebuild: true };
    return null;
  }

  /** The analysis's error, shown as the script's typed: not at once (one
      is often only there because the code's still being written), but once
      the typing stops for a second; and if it's on the cursor's line, once
      the cursor's left the line, or the typing's stopped for three. Once
      shown, it stays while it's the same error, and goes as soon as it
      isn't. */
  private admitLiveError() {
    window.clearTimeout(this.waitTimer);
    const live = this.liveError;
    if (!live) { this.liveShown = null; return; }
    if (this.liveShown && this.liveShown.message === live.message) { this.liveShown = live; return; }
    const state = this.view.state;
    const onCursorLine = this.view.hasFocus && live.from !== null
      && state.doc.lineAt(Math.min(live.from, state.doc.length)).number === state.doc.lineAt(state.selection.main.head).number;
    const wait = (onCursorLine ? 3000 : 1000) - (performance.now() - this.lastEdit);
    if (wait <= 0) { this.liveShown = live; return; }
    this.liveShown = null;
    this.waitTimer = window.setTimeout(() => this.showError(), wait + 20);
  }

  /** Whether there's an error waiting to be shown. */
  private waiting(): boolean {
    return this.liveError !== null && this.liveShown?.message !== this.liveError.message;
  }

  /** The text's changed: an error waits for the typing to stop. */
  private typed() { this.lastEdit = performance.now(); }

  /** The cursor's moved (or the focus): an error waiting for it to leave
      its line may be shown now. */
  private moved() { if (this.waiting()) this.showError(); }

  /** Show the error there is now, if any: in the text, and summed up below it. */
  private showError() {
    this.admitLiveError();
    const current = this.currentError();
    if (!current) {
      this.noteChosen = null;
      if (this.view.state.field(shownError)) this.view.dispatch({ effects: setShownError.of(null) });
      if (this.status.classList.contains("error")) this.say("", false);
      return;
    }
    const { error, rebuild } = current;
    const message = error.message.trim();
    const e: ShownError = { from: error.from, to: error.to, message, rebuild, note: false };
    e.note = this.wantNote(e, this.view.state);
    this.view.dispatch({ effects: setShownError.of(e) });
    const r = errorRange(this.view.state, e);
    const line = r ? `Line ${this.view.state.doc.lineAt(r.from).number}: ` : "";
    this.say(`⚠ ${rebuild ? "Not rebuilt. " : ""}${line}${message.split(/\s+/).join(" ")}`, true,
             r ? `${message}\n\n(Click to go to it.)` : message, r !== null);
  }

  /** Whether an error's note is to be shown: if that's been chosen; or
      else once the cursor's off its line, or the editor isn't being typed in. */
  private wantNote(e: ShownError, state: EditorState): boolean {
    if (this.noteChosen !== null) return this.noteChosen;
    const r = errorRange(state, e);
    if (!r) return false;
    return !this.view.hasFocus || state.doc.lineAt(r.from).number !== state.doc.lineAt(state.selection.main.head).number;
  }

  /** Show or hide the error's note, as it's to be now. */
  private refreshNote() {
    const e = this.view.state.field(shownError);
    if (!e) return;
    const want = this.wantNote(e, this.view.state);
    if (want !== e.note) this.view.dispatch({ effects: setNote.of(want) });
  }

  /** The error's mark was clicked: show its note, or hide it, from now on. */
  private toggleNote(lineFrom: number): boolean {
    const state = this.view.state;
    const r = errorRange(state, state.field(shownError));
    if (!r || state.doc.lineAt(r.from).from !== lineFrom) return false;
    this.noteChosen = !state.field(shownError)!.note;
    this.refreshNote();
    return true;
  }

  /** Go to the error, if there is one, showing its note (F8): one waiting
      to be shown is shown at once. */
  goToError(): boolean {
    if (this.waiting()) {
      this.liveShown = this.liveError;
      this.showError();
    }
    const state = this.view.state;
    const r = errorRange(state, state.field(shownError));
    if (!r) return false;
    // (opening any fold it's in)
    const unfold: StateEffect<unknown>[] = [];
    foldedRanges(state).between(r.from, r.from, (from, to) => { unfold.push(unfoldEffect.of({ from, to })); });
    this.noteChosen = true;
    this.view.dispatch({ selection: { anchor: r.from }, effects: [...unfold, setNote.of(true),
                         EditorView.scrollIntoView(r.from, { y: "center" })] });
    this.view.focus();
    return true;
  }

  focus() { this.view.focus(); }

  /** The preferences changed. (If which blocks fold has, every fold's
      opened first: one that can't fold any more would stay folded, with
      nothing to open it by.) */
  setPreferences(p: Preferences) {
    const before = this.prefs;
    this.prefs = p;
    if (before.foldCodeBlocks !== p.foldCodeBlocks) unfoldAll(this.view);
    this.view.dispatch({ effects: [
      this.numbering.reconfigure(this.numberingFor(p)),
      this.folding.reconfigure(this.foldingFor(p)),
      this.emacs.reconfigure(this.emacsFor(p)),
    ] });
    if (p.mathSymbols && !before.mathSymbols) this.normalize(this.view.hasFocus);
  }

  private numberingFor(p: Preferences): Extension { return p.lineNumbers ? lineNumbers() : []; }
  private foldingFor(p: Preferences): Extension { return folds(p.foldCodeBlocks); }
  // (a Mac's editor has them already: the Control key isn't for shortcuts there)
  private emacsFor(p: Preferences): Extension { return p.emacsKeys && !isMac ? Prec.high(keymap.of(emacsStyleKeymap)) : []; }

  /** What's been typed, if it isn't the project's script (not rebuilt
      yet): saved as the script, as the desktop app saves its editor's. */
  draft(): string | null {
    const text = this.view.state.doc.toString();
    return text === this.script ? null : text;
  }

  /** An analysis from the engine: used only if it's of the text as it is
      now. (One of text that's been typed in since is dropped: its offsets
      would be wrong, and rewriting the script with them would garble it.
      Another is on its way, for the text as it is.) */
  analysis(id: number, analysis: Analysis) {
    const text = this.view.state.doc.toString();
    if (id !== this.requested || text !== this.sentText) return;
    this.analyzed = { text, analysis };
    const b = new RangeSetBuilder<Decoration>();
    const marks: [number, number, string][] = analysis.spans.map(([f, t, c]) => [f, t, `tok-${c}`]);
    marks.sort((a, z) => a[0] - z[0] || a[1] - z[1]);
    for (const [f, t, c] of marks) if (t > f) b.add(f, t, Decoration.mark({ class: c }));
    this.view.dispatch({ effects: setColors.of(b.finish()) });
    this.liveError = analysis.error;
    this.showError();
    if (!analysis.error) this.normalize(this.view.hasFocus);
  }

  /** Say something under the editor: the whole of it as its tooltip, if
      it's too long to show; clicking it goes to the error, if it's clickable. */
  private say(text: string, error: boolean, title = "", clickable = false) {
    this.status.textContent = text;
    this.status.title = title;
    this.status.classList.toggle("error", error);
    this.status.classList.toggle("clickable", clickable);
  }

  private rebuild() {
    this.finishEscape();
    this.say("Rebuilding…", false);
    this.host.rebuild(this.view.state.doc.toString());
  }

  private revert() {
    this.view.setState(this.newState(this.script));
    this.edited = false;
    this.rebuildError = null;
    this.say("", false);
    this.reanalyze();
  }

  private reanalyze() {
    window.clearTimeout(this.timer);
    this.timer = window.setTimeout(() => {
      this.sentText = this.view.state.doc.toString();
      this.host.analyze(++this.requested, this.sentText);
    }, 150);
  }

  /** Write the script with mathematical symbols, if it checked out as it
      is (and it's been edited): except the cursor's line, if skipCaret. */
  private normalize(skipCaret: boolean) {
    const state = this.view.state;
    const a = this.analyzed;
    if (!this.prefs.mathSymbols || !this.edited || !a || a.analysis.error || a.text !== state.doc.toString()) return;
    if (state.field(escapeStart) !== null) return;
    const caret = state.doc.lineAt(state.selection.main.head);
    const changes = a.analysis.normalizations
      .filter(([f, t]) => !(skipCaret && f >= caret.from && t <= caret.to + 1))
      .map(([from, to, insert]) => ({ from, to, insert }));
    if (changes.length) this.view.dispatch({ changes, userEvent: "input.normalize" });
  }

  // --- symbol entry

  /** After a typed change: the escape being typed, turned into its symbol
      once it's finished. */
  private onTyped(view: EditorView) {
    if (!this.symbols) return;
    const state = view.state;
    const start = state.field(escapeStart);
    if (start === null) return;
    const head = state.selection.main.head;
    const typed = state.doc.sliceString(start, head);
    const e = this.symbols.escapeAt(false, typed);
    if (e.kind === "complete") {
      // (a second backslash finishes a name, and starts another)
      const next = typed.slice(e.length).startsWith("\\") ? start + e.text.length : null;
      view.dispatch({ changes: { from: start, to: start + e.length, insert: e.text },
                      effects: setEscape.of(next), annotations: isolateHistory.of("full") });
    } else if (e.kind === "literal" || e.kind === "notAName") {
      view.dispatch({ effects: setEscape.of(null) });
    }
  }

  /** Finish the escape being typed, as at the end of the text (Return). */
  private finishEscape(): boolean {
    const state = this.view.state;
    const start = state.field(escapeStart);
    if (start === null || !this.symbols) return false;
    const head = state.selection.main.head;
    const e = this.symbols.escapeAt(true, state.doc.sliceString(start, head));
    this.view.dispatch({
      ...(e.kind === "complete" ? { changes: { from: start, to: start + e.length, insert: e.text } } : {}),
      effects: setEscape.of(null), annotations: isolateHistory.of("full") });
    return true;
  }

  /** Tab: complete the name being typed. */
  private completeEscape(): boolean {
    const state = this.view.state;
    const start = state.field(escapeStart);
    if (start === null || !this.symbols) return false;
    const head = state.selection.main.head;
    const typed = state.doc.sliceString(start, head);
    const name = typedName(typed);
    if (name === null) return true;
    const c = this.symbols.complete(name);
    if (!c) return true;
    if ("finished" in c) {
      // (an accent's name stays an escape, waiting for what it goes on)
      const accent = c.finished.startsWith("\\");
      this.view.dispatch({ changes: { from: start, to: head, insert: c.finished },
                           selection: { anchor: start + c.finished.length },
                           effects: setEscape.of(accent ? start : null), annotations: isolateHistory.of("full") });
    } else {
      this.view.dispatch({ changes: { from: head - name.length, to: head, insert: c.longer },
                           selection: { anchor: head - name.length + c.longer.length } });
    }
    return true;
  }

  private newState(doc: string): EditorState {
    const editor = this;
    const escapeKeys = keymap.of([
      { key: "Tab", run: () => editor.completeEscape() },
      { key: "Enter", run: () => { editor.finishEscape(); return false; } },
      { key: "Escape", run: (v) => {
          if (v.state.field(escapeStart) === null) return false;
          v.dispatch({ effects: setEscape.of(null) });
          return true;
        } },
      { key: "Mod-Enter", run: () => { editor.rebuild(); return true; } },
      { key: "F8", run: () => editor.goToError() },
      // (after finishing an escape, if there was one)
      { key: "Enter", run: newLine },
    ]);
    // (the escape being typed, boxed, with what it would become below it)
    const escapeBox = EditorView.decorations.compute([escapeStart, "selection"], (state) => {
      const start = state.field(escapeStart);
      const head = state.selection.main.head;
      return start === null || head <= start ? Decoration.none
        : Decoration.set([Decoration.mark({ class: "cm-escape" }).range(start, head)]);
    });
    const preview = showTooltip.compute([escapeStart, "selection", "doc"], (state): Tooltip | null => {
      const start = state.field(escapeStart);
      if (start === null || !editor.symbols) return null;
      const p = editor.symbols.preview(state.doc.sliceString(start, state.selection.main.head));
      if (!p) return null;
      return {
        pos: start, above: false, strictSide: true,
        create: () => {
          const dom = el("div", { className: "cm-symbol-preview" });
          if (p.exact) {
            const [f, t] = p.exact;
            dom.append(p.line.slice(0, f), el("strong", { textContent: p.line.slice(f, t) }), p.line.slice(t));
          } else dom.textContent = p.line;
          return { dom };
        },
      };
    });
    const watch = ViewPlugin.fromClass(class {
      update(u: ViewUpdate) {
        if (u.docChanged) {
          editor.typed();
          if (u.transactions.some((t) => t.isUserEvent("input") || t.isUserEvent("delete"))) editor.edited = true;
          editor.reanalyze();
          if (u.transactions.some((t) => t.isUserEvent("input.type"))) queueMicrotask(() => editor.onTyped(u.view));
        }
        // (the cursor's left a line: it can be written with symbols now)
        const line = u.state.doc.lineAt(u.state.selection.main.head).number;
        if (line !== editor.caretLine) {
          editor.caretLine = line;
          if (!u.docChanged) queueMicrotask(() => editor.normalize(true));
        }
        if (u.focusChanged && !u.view.hasFocus) queueMicrotask(() => editor.normalize(false));
        // (the error's note: shown once the cursor's off its line, ...)
        if (u.selectionSet || u.focusChanged) queueMicrotask(() => { editor.moved(); editor.refreshNote(); });
      }
    });
    // (a backslash typed starts an escape; pasted names become symbols)
    const startEscape = EditorState.transactionExtender.of((tr: Transaction) => {
      if (!tr.isUserEvent("input.type") || !tr.docChanged) return null;
      let at: number | null = null;
      tr.changes.iterChanges((_fa, _ta, fromB, _tb, inserted) => {
        if (inserted.toString() === "\\") at = fromB;
      });
      if (at === null || tr.startState.field(escapeStart) !== null) return null;
      return { effects: setEscape.of(at) };
    });
    const pasteSymbols = EditorState.transactionFilter.of((tr: Transaction) => {
      if (!tr.isUserEvent("input.paste") || !editor.symbols) return tr;
      const changes: { from: number; to: number; insert: string }[] = [];
      tr.changes.iterChanges((fromA, toA, _fb, _tb, inserted) =>
        changes.push({ from: fromA, to: toA, insert: editor.symbols!.replaceNames(inserted.toString()) }));
      return [{ changes, userEvent: "input.paste", scrollIntoView: true }];
    });
    const extensions: Extension[] = [
      history(), escapeKeys, this.emacs.of(this.emacsFor(this.prefs)),
      keymap.of([...defaultKeymap, ...historyKeymap, ...foldKeymap, indentWithTab]),
      colors, escapeStart, escapeBox, preview, watch, startEscape, pasteSymbols,
      shownError, errorDecorations, placeNote,
      EditorView.lineWrapping, this.numbering.of(this.numberingFor(this.prefs)),
      // (the error's line marked, between the line numbers and the folds)
      gutter({
        class: "cm-error-gutter",
        markers: (v) => {
          const r = errorRange(v.state, v.state.field(shownError));
          return r ? RangeSet.of([errorDot.range(v.state.doc.lineAt(r.from).from)]) : RangeSet.empty;
        },
        initialSpacer: () => errorDot,
        domEventHandlers: { mousedown: (_v, line) => editor.toggleNote(line.from) },
      }),
      // (its markers redrawn when which lines fold changes)
      codeFolding(), foldGutter({ foldingChanged: (u) => u.startState.facet(foldService) !== u.state.facet(foldService) }),
      this.folding.of(this.foldingFor(this.prefs)),
    ];
    return EditorState.create({ doc, extensions });
  }
}

function el<K extends keyof HTMLElementTagNameMap>(tag: K, props: Partial<HTMLElementTagNameMap[K]> = {},
                                                 ...children: (Node | string)[]): HTMLElementTagNameMap[K] {
  const e = Object.assign(document.createElement(tag), props);
  e.append(...children);
  return e;
}
