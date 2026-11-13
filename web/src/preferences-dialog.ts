// The Preferences dialog, as the desktop's Preferences window
// (UI.Preferences) has it, less what a page can't do (choosing the
// projects' folder): the script editor's font and keys, the sphere's
// shading and viewers' smoothing, and the editor's colors. Each change is
// made, and saved, as soon as it's made.

import { classes, classStyle, editorBackground, isMac, preferences, setPreferences, smoothingNow,
         type ClassStyle, type Preferences, type Smoothing } from "./preferences.js";

function el<K extends keyof HTMLElementTagNameMap>(tag: K, props: Partial<HTMLElementTagNameMap[K]> = {},
                                                 ...children: (Node | string)[]): HTMLElementTagNameMap[K] {
  const e = Object.assign(document.createElement(tag), props);
  e.append(...children);
  return e;
}

const FONTS = ["ui-monospace", "SF Mono", "Menlo", "Monaco", "Consolas", "Cascadia Code", "DejaVu Sans Mono",
               "JetBrains Mono", "Fira Code", "Source Code Pro", "Courier New"];

let open: HTMLElement | null = null;

export function showPreferences() {
  if (open) return;
  const tabs: [string, () => HTMLElement][] = [["Editor", editorTab], ["Viewers", viewersTab], ["Colors", colorsTab]];
  const body = el("div", { className: "prefs-body" });
  const tabButtons = tabs.map(([label], i) => {
    const b = el("button", { type: "button", className: "prefs-tab", textContent: label });
    b.setAttribute("role", "tab");
    b.addEventListener("click", () => choose(i));
    return b;
  });
  const choose = (i: number) => {
    tabButtons.forEach((b, k) => b.setAttribute("aria-selected", String(k === i)));
    body.replaceChildren(tabs[i][1]());
    try { sessionStorage.setItem("fractalstream.preferencesTab", String(i)); } catch { /* (not remembered) */ }
  };
  const done = el("button", { type: "button", textContent: "Done" });
  const close = () => { dialog.remove(); open = null; };
  done.addEventListener("click", close);
  const tabRow = el("div", { className: "prefs-tabs" }, ...tabButtons);
  tabRow.setAttribute("role", "tablist");
  const dialog = el("div", { className: "page-dialog", role: "dialog" },
    el("div", { className: "page-dialog-box prefs-dialog" },
      el("div", { className: "form-title", textContent: "Preferences" }),
      tabRow, body,
      el("p", { className: "prefs-note", textContent: "These are this browser's, for every project." }),
      el("div", { className: "form-buttons" }, el("span", { className: "form-gap" }), done)));
  dialog.setAttribute("aria-label", "Preferences");
  dialog.addEventListener("keydown", (e) => {
    if (e.key === "Escape") { e.preventDefault(); close(); }
  });
  // (a click outside the box closes it)
  dialog.addEventListener("pointerdown", (e) => { if (e.target === dialog) close(); });
  let first = 0;
  try { first = Math.min(tabs.length - 1, Math.max(0, Number(sessionStorage.getItem("fractalstream.preferencesTab")) || 0)); }
  catch { /* (the first) */ }
  choose(first);
  document.body.append(dialog);
  open = dialog;
  tabButtons[first].focus();
}

// --- Editor

function check(label: string, get: (p: Preferences) => boolean, set: (on: boolean) => Partial<Preferences>): HTMLElement {
  const box = el("input", { type: "checkbox", checked: get(preferences()) });
  box.addEventListener("change", () => setPreferences(set(box.checked)));
  return el("label", { className: "prefs-check" }, box, el("span", { textContent: label }));
}

function editorTab(): HTMLElement {
  const p = preferences();
  const list = el("datalist", { id: "prefs-fonts" }, ...FONTS.map((f) => el("option", { value: f })));
  const font = el("input", { type: "text", value: p.editorFont ?? "", placeholder: "The usual fixed-width font" });
  font.setAttribute("list", "prefs-fonts");
  font.setAttribute("aria-label", "Font for scripts");
  font.addEventListener("change", () => setPreferences({ editorFont: font.value.trim() || null }));
  const size = el("input", { type: "number", min: "4", max: "200", step: "1", className: "prefs-size",
                             value: p.editorFontSize === null ? "" : String(p.editorFontSize), placeholder: "10" });
  size.setAttribute("aria-label", "Font size, in points");
  size.addEventListener("change", () => {
    const n = Number(size.value);
    const ok = size.value.trim() !== "" && Number.isInteger(n) && n >= 4 && n <= 200;
    setPreferences({ editorFontSize: ok ? n : null });
    if (!ok) size.value = "";
  });
  const usual = el("button", { type: "button", textContent: "Use the default" });
  usual.addEventListener("click", () => {
    font.value = ""; size.value = "";
    setPreferences({ editorFont: null, editorFontSize: null });
  });
  return el("div", { className: "prefs-pane" },
    el("div", { className: "prefs-label", textContent: "Font for scripts:" }),
    el("div", { className: "prefs-row" }, font, size, el("span", { textContent: "pt" }), usual, list),
    check("Write scripts with mathematical symbols (⟵, ℝ, z², …) as they're edited",
          (q) => q.mathSymbols, (on) => ({ mathSymbols: on })),
    check("Show line numbers", (q) => q.lineNumbers, (on) => ({ lineNumbers: on })),
    check("Fold blocks of code, not just stanzas and tool handlers",
          (q) => q.foldCodeBlocks, (on) => ({ foldCodeBlocks: on })),
    ...(isMac ? [] : [check("Emacs-style movement keys (Control-A, Control-E, …)",
                            (q) => q.emacsKeys, (on) => ({ emacsKeys: on }))]));
}

// --- Viewers

function viewersTab(): HTMLElement {
  const p = preferences();
  const slider = el("input", { type: "range", min: "0", max: "100", step: "1", value: String(p.sphereShading) });
  slider.setAttribute("aria-label", "Shading of the Riemann sphere");
  const shown = el("span", { className: "prefs-percent", textContent: `${p.sphereShading}%` });
  slider.addEventListener("input", () => {
    shown.textContent = `${slider.value}%`;
    setPreferences({ sphereShading: Number(slider.value) });
  });
  const choice = el("select", {});
  choice.setAttribute("aria-label", "Smoothing for new viewers");
  const highDpi = !smoothingNow({ ...p, smoothing: "auto" });
  for (const [value, label] of [
    ["auto", `Automatic: ${highDpi ? "off on this screen, which is high-DPI" : "on, on this screen"}`],
    ["on", "On"], ["off", "Off"]] as [Smoothing, string][]) {
    choice.append(el("option", { value, textContent: label, selected: p.smoothing === value }));
  }
  choice.addEventListener("change", () => setPreferences({ smoothing: choice.value as Smoothing }));
  return el("div", { className: "prefs-pane" },
    el("div", { className: "prefs-label", textContent: "Shading of the Riemann sphere:" }),
    el("div", { className: "prefs-row" }, slider, shown),
    el("p", { className: "prefs-hint", textContent: "Shades the globe as if it were lit from the upper left. At 0% it isn't shaded at all." }),
    el("div", { className: "prefs-label", textContent: "Subpixel smoothing, for new viewers:" }),
    choice,
    el("p", { className: "prefs-hint", textContent: "Each viewer's ☰ menu turns its own smoothing on or off." }));
}

// --- Colors

function colorsTab(): HTMLElement {
  let light = !window.matchMedia("(prefers-color-scheme: dark)").matches;
  const which = el("select", {},
    el("option", { value: "light", textContent: "a light page", selected: light }),
    el("option", { value: "dark", textContent: "a dark page", selected: !light }));
  which.setAttribute("aria-label", "Colors for");
  const resetAll = el("button", { type: "button", textContent: "Reset all" });
  const rows = el("div", { className: "prefs-colors" });
  const key = (): "editorColorsLight" | "editorColorsDark" => (light ? "editorColorsLight" : "editorColorsDark");
  const changes = () => preferences()[key()];
  const change = (name: string, f: (c: ClassStyle) => ClassStyle | null) => {
    const all = { ...changes() };
    const c = f({ ...(all[name] ?? {}) });
    if (c && Object.keys(c).length) all[name] = c; else delete all[name];
    setPreferences({ [key()]: all });
  };
  const render = () => {
    rows.replaceChildren(...classes.map(([name, label]) => row(name, label)));
    rows.style.background = editorBackground(light);
    resetAll.disabled = Object.keys(changes()).length === 0;
  };
  const row = (name: string, label: string): HTMLElement => {
    const sample = el("span", { className: "prefs-sample", textContent: label });
    const color = el("input", { type: "color", title: "Color" });
    const background = el("input", { type: "color", title: "Background" });
    color.setAttribute("aria-label", `Color for ${label}`);
    background.setAttribute("aria-label", `Background for ${label}`);
    const toggle = (what: "bold" | "italic" | "underline", text: string) => {
      const b = el("button", { type: "button", className: `prefs-toggle prefs-${what}`, textContent: text, title: what[0].toUpperCase() + what.slice(1) });
      b.setAttribute("aria-label", `${what[0].toUpperCase() + what.slice(1)} ${label}`);
      b.addEventListener("click", () => {
        const on = !classStyle(light, name)[what];
        change(name, (c) => ({ ...c, [what]: on }));
        show();
      });
      return b;
    };
    const bold = toggle("bold", "B"), italic = toggle("italic", "I"), underline = toggle("underline", "U");
    const reset = el("button", { type: "button", textContent: "Reset" });
    reset.setAttribute("aria-label", `Reset ${label}`);
    const show = () => {
      const s = classStyle(light, name);
      Object.assign(sample.style, {
        color: s.color, background: s.background ?? "transparent", fontWeight: s.bold ? "600" : "normal",
        fontStyle: s.italic ? "italic" : "normal", textDecoration: s.underline ? "underline" : "none" });
      color.value = s.color;
      background.value = s.background ?? editorBackground(light);
      bold.setAttribute("aria-pressed", String(s.bold));
      italic.setAttribute("aria-pressed", String(s.italic));
      underline.setAttribute("aria-pressed", String(s.underline));
      reset.disabled = !changes()[name];
      resetAll.disabled = Object.keys(changes()).length === 0;
    };
    color.addEventListener("input", () => { change(name, (c) => ({ ...c, color: color.value })); show(); });
    background.addEventListener("input", () => { change(name, (c) => ({ ...c, background: background.value })); show(); });
    reset.addEventListener("click", () => { change(name, () => null); show(); });
    show();
    return el("div", { className: "prefs-color-row" }, sample,
      el("span", { className: "prefs-color-controls" }, color, background, bold, italic, underline, reset));
  };
  which.addEventListener("change", () => { light = which.value === "light"; render(); });
  resetAll.addEventListener("click", () => { setPreferences({ [key()]: {} }); render(); });
  render();
  return el("div", { className: "prefs-pane" },
    el("div", { className: "prefs-row" }, el("span", { textContent: "Script colors for" }), which,
       el("span", { className: "form-gap" }), resetAll),
    el("p", { className: "prefs-hint", textContent: "For each kind of text: its color, its background, and bold, italic or underlined." }),
    rows);
}
