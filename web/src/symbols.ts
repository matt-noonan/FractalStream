// Typing mathematical symbols by their LaTeX-style names: a port of the
// core's Language.Editor.Symbols (escapeAt, escapePreview, typedName,
// completeSymbolName, replaceSymbolNames), which the script editor needs
// at every keystroke, so it can't ask the engine. The names themselves
// come from the engine ("symbols" message), so there's one list of them.
//
// Strings here are JavaScript's (UTF-16); lengths are in UTF-16 units,
// as the editor's positions are.

type Named = { symbol: string } | { accent: string };

export class Symbols {
  private readonly table = new Map<string, Named>();

  constructor(names: [string, string][], accents: [string, string][]) {
    for (const [n, s] of names) this.table.set(n, { symbol: s });
    for (const [n, m] of accents) this.table.set(n, { accent: m });
  }

  /** The names that start with a prefix, and how what they're for looks
      (an accent on a dotted circle): the shortest first. */
  startingWith(prefix: string): [string, string][] {
    const out: [string, string][] = [];
    for (const [n, x] of this.table) {
      if (n.startsWith(prefix)) out.push([n, "symbol" in x ? x.symbol : "◌" + x.accent]);
    }
    return out.sort(([a], [b]) => a.length - b.length || (a < b ? -1 : a > b ? 1 : 0));
  }

  /** What the text at a backslash is (see Escape): with atEnd, the text
      ends there; otherwise more may be typed after it. */
  escapeAt(atEnd: boolean, text: string): Escape {
    if (text.startsWith("\\\\")) return { kind: "literal" };
    if (!text.startsWith("\\")) return { kind: "notAName" };
    const rest = text.slice(1);
    const name = leadingName(rest);
    const after = rest.slice(name.length);
    const nameLen = 1 + name.length;
    if (after === "" && !atEnd) return { kind: "incomplete" };
    const x = this.table.get(name);
    if (!x) return { kind: "notAName" };
    if ("symbol" in x) return { kind: "complete", text: x.symbol, length: nameLen };
    const arg = this.argument(atEnd, after);
    if (arg === "none") return { kind: "notAName" };
    if (arg === "waiting") return { kind: "incomplete" };
    return { kind: "complete", text: (arg.base + x.accent).normalize("NFC"), length: nameLen + arg.length };
  }

  /** What an accent goes on (after any spaces), and how long that is; or
      nothing yet ("waiting"); or nothing it can go on ("none"). */
  private argument(atEnd: boolean, s: string): { base: string; length: number } | "waiting" | "none" {
    const spaces = s.length - s.replace(/^ +/, "").length;
    const t = s.slice(spaces);
    const withSpaces = (r: { base: string; length: number }) => ({ base: r.base, length: r.length + spaces });
    if (t === "") return atEnd ? "none" : "waiting";
    if (t[0] === "{") {
      const n = closingBrace(t.slice(1));
      if (n === null) return atEnd ? "none" : "waiting";
      const base = this.replaceNames(t.slice(1, 1 + n));
      return oneCharacter(base) ? withSpaces({ base, length: n + 2 }) : "none";
    }
    if (t[0] === "\\") {
      const e = this.escapeAt(atEnd, t);
      if (e.kind === "complete" && oneCharacter(e.text)) return withSpaces({ base: e.text, length: e.length });
      return e.kind === "incomplete" ? "waiting" : "none";
    }
    const c = String.fromCodePoint(t.codePointAt(0)!);
    if (c === "}" || c <= " ") return "none";
    let marks = "";
    for (const m of Array.from(t.slice(c.length))) { if (!isMark(m)) break; marks += m; }
    return withSpaces({ base: c + marks, length: c.length + marks.length });
  }

  /** What to show while an escape is typed (the text from its backslash to
      the cursor): the names it could become, and which part of that line
      is the name as it is now (if it's one); or an accent, waiting for
      what it goes on. Nothing, before any letters. */
  preview(text: string): { line: string; exact: [number, number] | null } | null {
    const prefix = typedName(text);
    const waiting = () => {
      const rest = text.slice(1);
      const n = leadingName(rest);
      const x = this.table.get(n);
      return rest.length > n.length && x && "accent" in x
        ? { line: `◌${x.accent} \\${n}, on what comes next`, exact: null } : null;
    };
    if (prefix === null || prefix === "") return waiting();
    const matches = this.startingWith(prefix);
    if (matches.length === 0) return { line: `no symbol named \\${prefix}`, exact: null };
    const shown = matches.slice(0, 8);
    const entries = shown.map(([n, sym]) => `${sym} \\${n}`);
    const line = entries.join("    ") + (matches.length > 8 ? "    …" : "");
    return { line, exact: shown[0][0] === prefix ? [0, entries[0].length] : null };
  }

  /** What Tab does with a name being typed: finish it (a symbol, or for an
      accent its whole name and a space), or go on with it as far as every
      name it could become agrees. */
  complete(prefix: string): { finished: string } | { longer: string } | null {
    const matches = this.startingWith(prefix);
    if (matches.length === 0) return null;
    const finished = (n: string) => {
      const x = this.table.get(n)!;
      return { finished: "symbol" in x ? x.symbol : `\\${n} ` };
    };
    if (matches[0][0] === prefix) return finished(prefix);
    if (matches.length === 1) return finished(matches[0][0]);
    let common = matches[0][0];
    for (const [n] of matches) {
      let k = 0;
      while (k < common.length && k < n.length && common[k] === n[k]) k++;
      common = common.slice(0, k);
    }
    return common.length > prefix.length ? { longer: common } : null;
  }

  /** Every name in some text turned into its symbol (for pasted text). */
  replaceNames(text: string): string {
    let out = "";
    let i = 0;
    while (i < text.length) {
      if (text[i] !== "\\") { out += text[i]; i++; continue; }
      const e = this.escapeAt(true, text.slice(i));
      if (e.kind === "complete") { out += e.text; i += e.length; }
      else if (e.kind === "literal") { out += "\\\\"; i += 2; }
      else { out += "\\"; i++; }
    }
    return out;
  }
}

/** What the text at a backslash is: finished (its first `length` units
    become `text`), not yet, two backslashes, or not a name. */
export type Escape =
  | { kind: "complete"; text: string; length: number }
  | { kind: "incomplete" }
  | { kind: "literal" }
  | { kind: "notAName" };

const isNameChar = (c: string) => /^[A-Za-z]$/.test(c);
const isMark = (c: string) => /^\p{M}$/u.test(c);

function leadingName(s: string): string {
  let n = 0;
  while (n < s.length && isNameChar(s[n])) n++;
  return s.slice(0, n);
}

/** The letters after the last backslash, if the text ends with them. */
export function typedName(text: string): string | null {
  const b = text.lastIndexOf("\\");
  if (b < 0) return null;
  const name = text.slice(b + 1);
  return /^[A-Za-z]*$/.test(name) ? name : null;
}

function closingBrace(s: string): number | null {
  let depth = 0;
  for (let i = 0; i < s.length; i++) {
    if (s[i] === "}") { if (depth === 0) return i; depth--; }
    else if (s[i] === "{") depth++;
  }
  return null;
}

function oneCharacter(s: string): boolean {
  const cs = Array.from(s);
  return cs.length > 0 && !isMark(cs[0]) && cs.slice(1).every(isMark);
}
