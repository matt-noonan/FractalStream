// The library, as the page's welcome screen (while no project is open),
// as the desktop app's welcome window shows it: the sections down the
// left, the chosen section's projects as a grid of previews, and the
// chosen project's preview, title and description on the right, with
// ways to start a new project or open a file. On a narrow screen the
// sections are a row of chips over the grid, and a project's details
// slide up from the bottom when it's tapped.
//
// A project's chosen with a click (or tap), or the arrow keys, and opened
// with a double click, Return, or its details' Open button.

import type { LibrarySection } from "./protocol.js";

function el<K extends keyof HTMLElementTagNameMap>(tag: K, props: Partial<HTMLElementTagNameMap[K]> = {},
                                                 ...children: (Node | string)[]): HTMLElementTagNameMap[K] {
  const e = Object.assign(document.createElement(tag), props);
  e.append(...children);
  return e;
}

/** A project's description as paragraphs: its file's lines (wrapped to
    fit a text editor) run together, with a blank line between paragraphs.
    (A line that starts with a space or a "-" or "*" starts a new line.) */
function paragraphs(text: string): HTMLElement[] {
  return text.trim().split(/\n\s*\n/).map((para) => {
    const lines: string[] = [];
    for (const line of para.split("\n")) {
      if (lines.length && !/^\s|^[-*•] /.test(line)) lines[lines.length - 1] += " " + line.trim();
      else lines.push(line.trim());
    }
    return el("p", { textContent: lines.join("\n") });
  });
}

export interface WelcomeHost {
  open(section: number, entry: number, title: string): void;
  openFile(): void;
  preferences(): void;
}

/** Where the details are a sheet from the bottom, not a pane. */
const narrow = window.matchMedia("(max-width: 720px)");

export class Welcome {
  readonly element: HTMLElement;
  private readonly sectionsEl: HTMLElement;
  private readonly gridEl: HTMLElement;
  private readonly detailsEl: HTMLElement;
  private sections: LibrarySection[] = [];
  private section = -1;
  private entry = -1;
  private openButton: HTMLButtonElement | null = null;
  private opening = false;

  constructor(private readonly host: WelcomeHost) {
    const newButton = el("button", { type: "button", textContent: "New project",
                                     title: "Start a project from one of the starting points" });
    newButton.addEventListener("click", () => this.chooseSection(this.sections.findIndex((s) => s.startingPoints)));
    const openFile = el("button", { type: "button", textContent: "Open a file…", title: "Open a FractalStream project file (⌘O)" });
    openFile.addEventListener("click", () => host.openFile());
    const prefs = el("button", { type: "button", className: "welcome-preferences", textContent: "⚙",
                                 title: "Preferences" });
    prefs.setAttribute("aria-label", "Preferences");
    prefs.addEventListener("click", () => host.preferences());
    this.sectionsEl = el("nav", { className: "welcome-sections" });
    this.sectionsEl.setAttribute("aria-label", "Library sections");
    this.gridEl = el("ul", { className: "welcome-grid", tabIndex: 0 });
    this.gridEl.setAttribute("role", "listbox");
    this.gridEl.setAttribute("aria-label", "Projects");
    this.gridEl.addEventListener("keydown", (e) => this.key(e));
    // (until the engine's started, and said what's in the library)
    this.gridEl.append(el("li", { className: "welcome-loading", textContent: "Loading the library…" }));
    this.detailsEl = el("aside", { className: "welcome-details" });
    this.element = el("div", { className: "welcome" },
      el("header", { className: "welcome-header" },
        el("h1", { textContent: "FractalStream" }), el("span", { className: "welcome-gap" }), newButton, openFile, prefs),
      el("div", { className: "welcome-panes" }, this.sectionsEl, this.gridEl, this.detailsEl),
      el("p", { className: "welcome-hint", textContent: "You can also drop a project file anywhere on this page." }));
    this.element.addEventListener("keydown", (e) => { if (e.key === "Escape") this.closeSheet(); });
    this.showDetails();
  }

  /** The library's sections, from the engine: shown from the first
      section of finished projects to explore, as the desktop's is. */
  show(sections: LibrarySection[]) {
    this.sections = sections;
    this.sectionsEl.replaceChildren(...sections.map((s, i) => {
      const b = el("button", { type: "button", textContent: s.name });
      b.addEventListener("click", () => this.chooseSection(i));
      return b;
    }));
    const first = sections.findIndex((s) => !s.startingPoints);
    this.section = -1;
    this.chooseSection(first >= 0 ? first : 0);
  }

  /** A project's being opened (or not, any more: it failed, or it's open). */
  setOpening(on: boolean) {
    this.opening = on;
    this.element.classList.toggle("opening", on);
    if (this.openButton) {
      this.openButton.disabled = on;
      this.openButton.textContent = on ? "Opening…" : this.openLabel();
    }
  }

  focus() { this.gridEl.focus({ preventScroll: true }); }

  private chooseSection(i: number) {
    if (i < 0 || i >= this.sections.length) return;
    this.closeSheet();
    const changed = i !== this.section;
    this.section = i;
    this.sectionsEl.querySelectorAll("button").forEach((b, k) => b.setAttribute("aria-current", String(k === i)));
    this.sectionsEl.children[i]?.scrollIntoView({ block: "nearest", inline: "nearest" });
    if (!changed) return;
    const s = this.sections[i];
    this.gridEl.replaceChildren(...s.entries.map((e, k) => {
      const tile = el("li", { className: "welcome-tile", title: e.title },
        e.preview ? el("img", { src: `data:image/png;base64,${e.preview}`, alt: "", draggable: false })
                  : el("span", { className: "welcome-no-preview" }),
        el("span", { className: "welcome-tile-title", textContent: e.title }));
      tile.setAttribute("role", "option");
      tile.addEventListener("click", () => { this.choose(k); this.openSheet(); });
      tile.addEventListener("dblclick", () => this.open());
      return tile;
    }));
    this.gridEl.scrollTop = 0;
    // (the first project's chosen: on a wide screen, its details are shown)
    this.choose(s.entries.length ? 0 : -1);
  }

  private choose(k: number) {
    this.entry = k;
    this.gridEl.querySelectorAll("li").forEach((li, j) => li.setAttribute("aria-selected", String(j === k)));
    const tile = this.gridEl.children[k] as HTMLElement | undefined;
    tile?.scrollIntoView({ block: "nearest" });
    this.showDetails();
  }

  private openLabel() {
    return this.sections[this.section]?.startingPoints ? "Start a project" : "Open";
  }

  private showDetails() {
    const e = this.sections[this.section]?.entries[this.entry];
    const close = el("button", { type: "button", className: "welcome-sheet-close", textContent: "×", title: "Close" });
    close.setAttribute("aria-label", "Close");
    close.addEventListener("click", () => this.closeSheet());
    if (!e) {
      this.openButton = null;
      this.detailsEl.replaceChildren(el("p", { className: "welcome-empty", textContent: "Choose a project to see what it is." }));
      return;
    }
    this.openButton = el("button", { type: "button", className: "welcome-open", textContent: this.openLabel() });
    this.openButton.addEventListener("click", () => this.open());
    this.setOpening(this.opening);
    this.detailsEl.replaceChildren(
      close,
      e.preview ? el("img", { className: "welcome-preview", src: `data:image/png;base64,${e.preview}`, alt: "" }) : "",
      el("h2", { textContent: e.title }),
      el("div", { className: "welcome-description" }, ...paragraphs(e.description)),
      this.openButton);
  }

  private open() {
    const e = this.sections[this.section]?.entries[this.entry];
    if (!e || this.opening) return;
    this.setOpening(true);
    this.host.open(this.section, this.entry, e.title);
  }

  private openSheet() { if (narrow.matches) this.element.classList.add("sheet-open"); }
  private closeSheet() { this.element.classList.remove("sheet-open"); }

  /** The arrow keys move around the grid (by its columns, as it's laid
      out); Return opens. */
  private key(e: KeyboardEvent) {
    const tiles = [...this.gridEl.children] as HTMLElement[];
    if (!tiles.length) return;
    if (e.key === "Enter") { e.preventDefault(); this.open(); return; }
    const top = tiles[0].offsetTop;
    const columns = Math.max(1, tiles.findIndex((t) => t.offsetTop !== top) === -1
                                  ? tiles.length : tiles.findIndex((t) => t.offsetTop !== top));
    const step: Record<string, number> = { ArrowLeft: -1, ArrowRight: 1, ArrowUp: -columns, ArrowDown: columns };
    if (!(e.key in step)) return;
    e.preventDefault();
    const k = Math.max(0, Math.min(tiles.length - 1, Math.max(0, this.entry) + step[e.key]));
    this.choose(k);
  }
}
