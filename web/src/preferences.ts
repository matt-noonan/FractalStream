// The page's preferences: this browser's, not a project's, kept in its
// local storage as JSON (and so read at once, before anything's shown).
// They're named as the desktop app's settings (FractalStream.Settings,
// in settings.yaml) where they're the same; a preference that's its
// default is left out; and keys this page doesn't know are kept as they
// are (from a newer version of it).
//
// Changes are made with `setPreferences` (which saves them at once, as
// the desktop's Preferences window does), and whatever shows them
// follows them with `onPreferences`.

export interface ClassStyle {
  color?: string;
  background?: string;
  bold?: boolean;
  italic?: boolean;
  underline?: boolean;
}

export type Smoothing = "auto" | "on" | "off";

export interface Preferences {
  /** The script editor's font (a CSS font family), or null for the page's. */
  editorFont: string | null;
  /** Its size, in points, or null for the editor's usual size. */
  editorFontSize: number | null;
  lineNumbers: boolean;
  /** Emacs-style movement keys, where Control is the shortcut key (on a
      Mac, the editor always has them). */
  emacsKeys: boolean;
  /** Writing scripts with mathematical symbols as they're edited. */
  mathSymbols: boolean;
  /** Folding any block of code, not only stanzas and tool handlers. */
  foldCodeBlocks: boolean;
  /** Changes to how the editor shows each class of a script's spans (by
      Language.Editor.Highlight's names), with the page light or dark. */
  editorColorsLight: Record<string, ClassStyle>;
  editorColorsDark: Record<string, ClassStyle>;
  /** How strongly sphere viewers shade the globe: 0 to 100. */
  sphereShading: number;
  /** Whether new viewers are smoothed: "auto" is on, except on a high-DPI
      screen (drawn at twice the resolution already). */
  smoothing: Smoothing;
}

export const defaults: Preferences = {
  editorFont: null, editorFontSize: null, lineNumbers: false, emacsKeys: false, mathSymbols: true,
  foldCodeBlocks: false, editorColorsLight: {}, editorColorsDark: {}, sphereShading: 75, smoothing: "auto",
};

const KEY = "fractalstream.preferences";

/** Whether this is a Mac (or iPhone, or iPad): its editor has emacs keys
    already. */
export const isMac = /Mac|iPhone|iPad|iPod/.test(navigator.platform || navigator.userAgent);

// --- reading and writing them

type Json = Record<string, unknown>;

const keys: Record<keyof Preferences, string> = {
  editorFont: "editor-font", editorFontSize: "editor-font-size", lineNumbers: "line-numbers",
  emacsKeys: "emacs-keys", mathSymbols: "math-symbols", foldCodeBlocks: "fold-code-blocks",
  editorColorsLight: "editor-colors-light", editorColorsDark: "editor-colors-dark",
  sphereShading: "sphere-shading", smoothing: "smoothing",
};

const hexColor = (x: unknown): x is string => typeof x === "string" && /^#[0-9a-f]{6}$/i.test(x);

function readStyles(x: unknown): Record<string, ClassStyle> {
  const out: Record<string, ClassStyle> = {};
  if (!x || typeof x !== "object") return out;
  for (const [name, s] of Object.entries(x as Json)) {
    if (!s || typeof s !== "object") continue;
    const o = s as Json, style: ClassStyle = {};
    if (hexColor(o.color)) style.color = o.color.toLowerCase();
    if (hexColor(o.background)) style.background = o.background.toLowerCase();
    for (const k of ["bold", "italic", "underline"] as const) if (typeof o[k] === "boolean") style[k] = o[k] as boolean;
    if (Object.keys(style).length) out[name] = style;
  }
  return out;
}

/** Preferences from what's stored: each one that isn't what it should be
    is its default. */
function read(stored: Json): Preferences {
  const p: Preferences = { ...defaults };
  const v = (k: keyof Preferences) => stored[keys[k]];
  const flag = (k: "lineNumbers" | "emacsKeys" | "mathSymbols" | "foldCodeBlocks") => {
    const x = v(k);
    if (typeof x === "boolean") p[k] = x;
  };
  const font = v("editorFont");
  if (typeof font === "string" && font.trim()) p.editorFont = font.trim();
  const size = v("editorFontSize");
  if (typeof size === "number" && Number.isInteger(size) && size >= 4 && size <= 200) p.editorFontSize = size;
  flag("lineNumbers"); flag("emacsKeys"); flag("mathSymbols"); flag("foldCodeBlocks");
  p.editorColorsLight = readStyles(v("editorColorsLight"));
  p.editorColorsDark = readStyles(v("editorColorsDark"));
  const shading = v("sphereShading");
  if (typeof shading === "number" && Number.isInteger(shading) && shading >= 0 && shading <= 100) p.sphereShading = shading;
  const smoothing = v("smoothing");
  if (smoothing === "auto" || smoothing === "on" || smoothing === "off") p.smoothing = smoothing;
  return p;
}

/** What to store for the preferences: only what isn't the default, with
    the keys this page doesn't know (`other`). */
function write(p: Preferences, other: Json): Json {
  const out: Json = { ...other };
  for (const k of Object.keys(keys) as (keyof Preferences)[]) {
    const value = p[k], def = defaults[k];
    const same = typeof value === "object" && value !== null
      ? Object.keys(value).length === 0 : value === def;
    if (same) delete out[keys[k]]; else out[keys[k]] = value;
  }
  return out;
}

function load(): { prefs: Preferences; other: Json } {
  let stored: Json = {};
  try {
    const text = localStorage.getItem(KEY);
    const x = text ? JSON.parse(text) : {};
    if (x && typeof x === "object" && !Array.isArray(x)) stored = x;
  } catch { /* (none, or not readable: the defaults) */ }
  const known = new Set(Object.values(keys));
  const other = Object.fromEntries(Object.entries(stored).filter(([k]) => !known.has(k)));
  return { prefs: read(stored), other };
}

let { prefs: current, other } = load();
const followers: ((p: Preferences) => void)[] = [];

export function preferences(): Preferences { return current; }

/** Change some preferences, and save them. */
export function setPreferences(change: Partial<Preferences>) {
  current = { ...current, ...change };
  try { localStorage.setItem(KEY, JSON.stringify(write(current, other))); } catch { /* (not saved, this time) */ }
  applyEditorStyles();
  for (const f of followers) f(current);
}

/** Follow the preferences: now, and as they change. */
export function onPreferences(f: (p: Preferences) => void) {
  followers.push(f);
  f(current);
}

/** Whether new viewers are smoothed. */
export function smoothingNow(p: Preferences = current): boolean {
  return p.smoothing === "auto" ? (window.devicePixelRatio || 1) < 1.5 : p.smoothing === "on";
}

// --- how the editor shows a script: each class's style, as the page has
// it, with the preferences' changes; in a stylesheet of its own

export interface Style { color: string; background: string | null; bold: boolean; italic: boolean; underline: boolean }

/** The classes, as Language.Editor.Highlight names them, and as the
    Colors preferences call them (in its order). */
export const classes: [string, string][] = [
  ["keyword", "keywords"], ["builtin-function", "built-in functions"], ["builtin-value", "built-in values"],
  ["type", "types"], ["color", "color names"], ["number", "numbers"], ["string", "\"text\""],
  ["comment", "# comments"], ["operator", "operators"], ["stanza-header", "@stanza headers"],
  ["parameter", "parameters"], ["variable", "variables"], ["function", "functions"],
  ["code-block", "code blocks"], ["undefined", "undefined names"],
];

/** The editor's text and background. */
export const editorText = (light: boolean) => (light ? "#1d1d1b" : "#ececea");
export const editorBackground = (light: boolean) => (light ? "#ffffff" : "#252524");

/** A class's own style, with the page light or dark. */
export function defaultStyle(light: boolean, name: string): Style {
  const plain = (l: string, d: string, more: Partial<Style> = {}): Style =>
    ({ color: light ? l : d, background: null, bold: false, italic: false, underline: false, ...more });
  switch (name) {
    case "keyword": return plain("#a626a4", "#c678dd");
    case "builtin-function": return plain("#4078f2", "#61afef");
    case "builtin-value": return plain("#986801", "#d19a66");
    case "type": return plain("#c18401", "#e5c07b");
    case "color": return plain("#0184bc", "#56b6c2");
    case "number": return plain("#986801", "#d19a66");
    case "string": return plain("#50a14f", "#98c379");
    case "comment": return plain("#8a8a85", "#7f848e", { italic: true });
    case "operator": return plain("#6b6b66", "#abb2bf");
    case "stanza-header": return plain("#2f6fd6", "#6ea2ff", { bold: true });
    case "parameter": return plain("#e45649", "#e06c75");
    case "function": case "code-block": return plain("#4078f2", "#61afef", { italic: true });
    // (with a dotted underline, in the error's color, unless it's changed)
    case "undefined": return plain(editorText(true), editorText(false), { underline: true });
    default: return plain(editorText(true), editorText(false));
  }
}

/** A class's style, with the page light or dark, and the preferences'
    changes to it. */
export function classStyle(light: boolean, name: string, p: Preferences = current): Style {
  const s = defaultStyle(light, name);
  const c = (light ? p.editorColorsLight : p.editorColorsDark)[name] ?? {};
  return { color: c.color ?? s.color, background: c.background ?? s.background,
           bold: c.bold ?? s.bold, italic: c.italic ?? s.italic, underline: c.underline ?? s.underline };
}

/** A style as CSS declarations. */
export function styleCss(light: boolean, name: string, p: Preferences = current): string {
  const s = classStyle(light, name, p);
  const changed = (light ? p.editorColorsLight : p.editorColorsDark)[name]?.underline !== undefined;
  const underline = !s.underline ? "none"
    : name === "undefined" && !changed ? `underline dotted ${light ? "#c0392b" : "#ff7a6b"}` : "underline";
  return `color: ${s.color}; background-color: ${s.background ?? "transparent"}; ` +
    `font-weight: ${s.bold ? 600 : "normal"}; font-style: ${s.italic ? "italic" : "normal"}; text-decoration: ${underline};`;
}

const DEFAULT_FONT = `ui-monospace, "SF Mono", Menlo, monospace`;

/** A font's name as CSS has it: quoted, unless it's a list already. */
const fontFamily = (name: string) => (/[,"']/.test(name) ? name : `"${name}", monospace`);

const sheet = document.createElement("style");
sheet.id = "editor-styles";
document.head.append(sheet);

function applyEditorStyles() {
  const rules = (light: boolean) => classes.map(([name]) => `.tok-${name} { ${styleCss(light, name)} }`).join("\n");
  const p = current;
  sheet.textContent =
    `:root { --editor-font: ${p.editorFont ? fontFamily(p.editorFont) : DEFAULT_FONT}; ` +
    `--editor-font-size: ${p.editorFontSize ? `${p.editorFontSize}pt` : "13px"}; }\n` +
    `${rules(true)}\n@media (prefers-color-scheme: dark) {\n${rules(false)}\n}`;
}
applyEditorStyles();
