// The script editor: a project's whole script, in CodeMirror, as the
// desktop app's editor (UI.ProjectScriptEditor) has it.
//
// - It's colored, and its error marked, by the engine (Language.Editor.
//   Highlight, and checking the script), a moment after each change.
// - Once it checks out, it's written with mathematical symbols
//   (Language.Editor.Normalize: z^2 as z², := as ≔), except the line the
//   cursor's on, until the cursor leaves it (so that what's being typed
//   isn't rewritten under it), and only once it's been edited (so that
//   just opening a script doesn't change it).
// - Symbols can be typed by their names (symbols.ts): \theta and a space
//   is θ and a space; Tab completes a name, Return finishes one, Escape
//   leaves it as typed; pasted names become their symbols.
// - Rebuild (or Ctrl/Cmd-Enter) builds the project again from it (and if
//   that works, the page closes the editor); Revert goes back to the
//   project's script.

import { EditorState, StateEffect, StateField, RangeSetBuilder, Transaction, type Extension } from "@codemirror/state";
import { EditorView, Decoration, type DecorationSet, keymap, showTooltip, type Tooltip, ViewPlugin,
         type ViewUpdate } from "@codemirror/view";
import { defaultKeymap, history, historyKeymap, indentWithTab, isolateHistory } from "@codemirror/commands";
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

  constructor(script: string, private readonly host: EditorHost) {
    this.script = script;
    this.status = el("div", { className: "editor-status" });
    const rebuild = el("button", { type: "button", textContent: "Rebuild",
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

  rebuildFailed(error: ScriptError) {
    this.say(`⚠ It didn't build, so the project's as it was: ${error.message}`, true);
  }

  focus() { this.view.focus(); }

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
    const e = analysis.error;
    if (e && e.from !== null && e.to !== null) {
      const to = Math.max(e.to, e.from + 1);
      marks.push([Math.min(e.from, text.length), Math.min(to, text.length), "cm-script-error"]);
    }
    marks.sort((a, z) => a[0] - z[0] || a[1] - z[1]);
    for (const [f, t, c] of marks) if (t > f) b.add(f, t, Decoration.mark({ class: c }));
    this.view.dispatch({ effects: setColors.of(b.finish()) });
    if (e) this.say(`⚠ ${e.message}`, true);
    else { if (this.status.classList.contains("error")) this.say("", false); this.normalize(this.view.hasFocus); }
  }

  private say(text: string, error: boolean) {
    this.status.textContent = text;
    this.status.classList.toggle("error", error);
  }

  private rebuild() {
    this.finishEscape();
    this.say("Rebuilding…", false);
    this.host.rebuild(this.view.state.doc.toString());
  }

  private revert() {
    this.view.setState(this.newState(this.script));
    this.edited = false;
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
    if (!this.edited || !a || a.analysis.error || a.text !== state.doc.toString()) return;
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
      history(), escapeKeys, keymap.of([...defaultKeymap, ...historyKeymap, indentWithTab]),
      colors, escapeStart, escapeBox, preview, watch, startEscape, pasteSymbols,
      EditorView.lineWrapping,
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
