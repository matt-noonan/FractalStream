// Basic window management: a desk of windows, each with a title bar to
// drag it by and a corner to resize it from; clicking one brings it to the
// front. A window can be minimized to just its title bar (and back), by
// double-clicking the title bar, or with its button there. A window can
// have a menu (☰, in its title bar): actions (with their keyboard
// shortcuts shown), options on or off, and submenus, which open in its
// place (with a way back). Windows are kept out from under a part of the
// desk that's covered (the project's title, floating over its corner), so
// that every one can be grabbed by its title bar.

export interface WindowOptions {
  title: string;
  /** Where it goes on the desk, and the size of its contents (in CSS pixels). */
  x: number;
  y: number;
  width: number;
  height: number;
  content: HTMLElement;
  /** Its contents' new size, as it's resized. */
  onResize?: (width: number, height: number) => void;
  /** Whether it starts minimized. */
  minimized?: boolean;
  /** Its menu's items, as they are when it's opened. */
  menu?: () => MenuItem[];
  /** Something more in its title bar, after its title. */
  titleExtra?: HTMLElement;
}

/** A part of the desk (in its coordinates) windows are kept out from under. */
export interface Covered { left: number; top: number; right: number; bottom: number }

export type MenuItem =
  /** Something to do (unless it's not `enabled`). */
  | { kind: "action"; label: string; run: () => void; shortcut?: string; enabled?: boolean; help?: string }
  /** An option, on or off, toggled by choosing it. */
  | { kind: "toggle"; label: string; on: boolean; toggle: () => void; shortcut?: string; help?: string }
  | { kind: "submenu"; label: string; items: () => MenuItem[] }
  | { kind: "separator" };

const MIN_SIZE = 80;

export class Desk {
  private top = 0;
  private covered: () => Covered | null = () => null;

  constructor(readonly element: HTMLElement) {
    element.classList.add("desk");
  }

  /** Keep windows out from under this part of the desk: none is placed or
      dragged so that it's under it (one that would be goes just below). */
  keepClearOf(covered: () => Covered | null) {
    this.covered = covered;
    this.keepAllClear();
  }

  /** Move any window that's under the covered part out from under it (as
      when the covered part has grown, or the desk's been resized). */
  keepAllClear() {
    for (const win of this.element.querySelectorAll<HTMLElement>(":scope > .window")) {
      const [x, y] = this.clearSpot(win.offsetLeft, win.offsetTop, win.offsetWidth);
      win.style.left = `${x}px`;
      win.style.top = `${y}px`;
    }
  }

  /** Where a window (this wide) can go, nearest (x, y): on the desk, and
      not under its covered part. */
  private clearSpot(x: number, y: number, width: number): [number, number] {
    x = Math.max(0, x);
    y = Math.max(0, y);
    const c = this.covered();
    // (anywhere above its bottom: the covered part's at the desk's top)
    if (c && x < c.right && x + width > c.left && y < c.bottom) y = Math.ceil(c.bottom);
    return [x, y];
  }

  clear() {
    this.element.replaceChildren();
    this.top = 0;
  }

  add(options: WindowOptions): HTMLElement {
    const label = Object.assign(document.createElement("span"), { className: "window-label", textContent: options.title });
    const minimize = Object.assign(document.createElement("button"), { type: "button", className: "window-minimize" });
    const title = Object.assign(document.createElement("div"), { className: "window-title" });
    title.append(label);
    if (options.titleExtra) title.append(options.titleExtra);
    if (options.menu) title.append(menuButton(options.menu));
    title.append(minimize);
    const body = Object.assign(document.createElement("div"), { className: "window-body" });
    const grip = Object.assign(document.createElement("div"), { className: "window-grip", title: "Resize" });
    body.append(options.content);
    const win = Object.assign(document.createElement("section"), { className: "window" });
    win.append(title, body, grip);
    body.style.width = `${options.width}px`;
    body.style.height = `${options.height}px`;
    this.element.append(win);
    const [x0, y0] = this.clearSpot(options.x, options.y, win.offsetWidth);
    win.style.left = `${x0}px`;
    win.style.top = `${y0}px`;
    this.raise(win);

    win.addEventListener("pointerdown", () => this.raise(win), { capture: true });

    // minimizing: to the title bar, keeping the window's width
    const setMinimized = (on: boolean) => {
      win.style.width = on ? `${win.offsetWidth}px` : "";
      win.classList.toggle("minimized", on);
      minimize.textContent = on ? "▢" : "–";
      minimize.title = on ? "Restore" : "Minimize to the title bar";
      minimize.setAttribute("aria-label", minimize.title);
    };
    setMinimized(options.minimized ?? false);
    minimize.addEventListener("pointerdown", (e) => e.stopPropagation());
    minimize.addEventListener("click", () => setMinimized(!win.classList.contains("minimized")));
    title.addEventListener("dblclick", (e) => {
      if (e.target === minimize) return;
      setMinimized(!win.classList.contains("minimized"));
    });

    // dragging the title bar moves it (not off the top or left of the
    // desk, nor under its covered part)
    drag(title, (dx, dy, start: { x: number; y: number }) => {
      const [x, y] = this.clearSpot(start.x + dx, start.y + dy, win.offsetWidth);
      win.style.left = `${x}px`;
      win.style.top = `${y}px`;
    }, () => ({ x: win.offsetLeft, y: win.offsetTop }));

    // dragging the corner resizes its contents
    drag(grip, (dx, dy, start: { w: number; h: number }) => {
      const w = Math.max(MIN_SIZE, Math.round(start.w + dx));
      const h = Math.max(MIN_SIZE, Math.round(start.h + dy));
      body.style.width = `${w}px`;
      body.style.height = `${h}px`;
      options.onResize?.(w, h);
    }, () => ({ w: body.clientWidth, h: body.clientHeight }));
    return win;
  }

  /** Whether a window is minimized to its title bar. */
  static isMinimized(win: HTMLElement) {
    return win.classList.contains("minimized");
  }

  private raise(win: HTMLElement) {
    win.style.zIndex = String(++this.top);
  }
}

/** Follow a pointer dragged from `handle`, with where things were when it started. */
function drag<S>(handle: HTMLElement, move: (dx: number, dy: number, start: S) => void, begin: () => S) {
  handle.addEventListener("pointerdown", (e) => {
    if (e.button !== 0) return;
    e.preventDefault();
    handle.setPointerCapture(e.pointerId);
    const x0 = e.clientX, y0 = e.clientY, start = begin();
    const onMove = (m: PointerEvent) => move(m.clientX - x0, m.clientY - y0, start);
    const onUp = () => {
      handle.removeEventListener("pointermove", onMove);
      handle.removeEventListener("pointerup", onUp);
      handle.removeEventListener("pointercancel", onUp);
    };
    handle.addEventListener("pointermove", onMove);
    handle.addEventListener("pointerup", onUp);
    handle.addEventListener("pointercancel", onUp);
  });
}

/** How a menu's button looks: its contents (☰, to begin with), its
    class, its tooltip (and label), and which side its menu lines up with. */
export interface MenuButtonOptions {
  content?: (string | Node)[];
  className?: string;
  title?: string;
  align?: "left" | "right";
}

/** A window's ☰ button (or another, as `options` say), and the menu it
    opens (closed by choosing an item, by clicking elsewhere, or with
    Escape). */
export function menuButton(items: () => MenuItem[], options: MenuButtonOptions = {}): HTMLElement {
  const button = Object.assign(document.createElement("button"), {
    type: "button", className: options.className ?? "window-menu-button", title: options.title ?? "Options" });
  button.append(...(options.content ?? ["☰"]));
  button.setAttribute("aria-label", options.title ?? "Options");
  button.setAttribute("aria-haspopup", "menu");
  const wrap = Object.assign(document.createElement("span"), { className: "window-menu-wrap" });
  if (options.align === "left") wrap.classList.add("menu-left");
  wrap.append(button);
  let menu: HTMLElement | null = null;
  const close = () => {
    menu?.remove();
    menu = null;
    button.setAttribute("aria-expanded", "false");
    document.removeEventListener("pointerdown", outside, { capture: true });
    document.removeEventListener("keydown", escape);
  };
  const outside = (e: Event) => { if (!wrap.contains(e.target as Node)) close(); };
  const escape = (e: KeyboardEvent) => { if (e.key === "Escape") close(); };
  // (a menu's items, and, for a submenu, the way back to its parent's)
  const show = (list: MenuItem[], back: (() => void) | null) => {
    const m = menu!;
    m.replaceChildren();
    const row = (label: string, role: string, shortcut?: string) => {
      const b = Object.assign(document.createElement("button"), { type: "button" });
      b.setAttribute("role", role);
      b.append(Object.assign(document.createElement("span"), { className: "menu-label", textContent: label }));
      if (shortcut) b.append(Object.assign(document.createElement("kbd"), { textContent: shortcut }));
      m.append(b);
      return b;
    };
    if (back) {
      const b = row("‹ Back", "menuitem");
      b.classList.add("menu-back");
      b.addEventListener("click", back);
    }
    for (const item of list) {
      switch (item.kind) {
        case "separator":
          m.append(Object.assign(document.createElement("hr"), { className: "menu-separator" }));
          break;
        case "action": {
          const b = row(item.label, "menuitem", item.shortcut);
          b.disabled = item.enabled === false;
          if (item.help) b.title = item.help;
          b.addEventListener("click", () => { close(); item.run(); });
          break;
        }
        case "toggle": {
          const b = row(item.label, "menuitemcheckbox", item.shortcut);
          b.setAttribute("aria-checked", String(item.on));
          if (item.help) b.title = item.help;
          b.addEventListener("click", () => { close(); item.toggle(); });
          break;
        }
        case "submenu": {
          const b = row(item.label, "menuitem", "›");
          b.setAttribute("aria-haspopup", "menu");
          b.addEventListener("click", () => show(item.items(), () => show(list, back)));
          break;
        }
      }
    }
  };
  const open = () => {
    menu = Object.assign(document.createElement("div"), { className: "window-menu" });
    menu.setAttribute("role", "menu");
    show(items(), null);
    wrap.append(menu);
    button.setAttribute("aria-expanded", "true");
    document.addEventListener("pointerdown", outside, { capture: true });
    document.addEventListener("keydown", escape);
  };
  // (not a drag of the title bar, nor a double click on it)
  wrap.addEventListener("pointerdown", (e) => e.stopPropagation());
  wrap.addEventListener("dblclick", (e) => e.stopPropagation());
  button.addEventListener("click", () => (menu ? close() : open()));
  return wrap;
}
