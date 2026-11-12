// Basic window management: a desk of windows, each with a title bar to
// drag it by and a corner to resize it from; clicking one brings it to the
// front. A window can be minimized to just its title bar (and back), by
// double-clicking the title bar, or with its button there.

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
}

const MIN_SIZE = 80;

export class Desk {
  private top = 0;

  constructor(readonly element: HTMLElement) {
    element.classList.add("desk");
  }

  clear() {
    this.element.replaceChildren();
    this.top = 0;
  }

  add(options: WindowOptions): HTMLElement {
    const label = Object.assign(document.createElement("span"), { className: "window-label", textContent: options.title });
    const minimize = Object.assign(document.createElement("button"), { type: "button", className: "window-minimize" });
    const title = Object.assign(document.createElement("div"), { className: "window-title" });
    title.append(label, minimize);
    const body = Object.assign(document.createElement("div"), { className: "window-body" });
    const grip = Object.assign(document.createElement("div"), { className: "window-grip", title: "Resize" });
    body.append(options.content);
    const win = Object.assign(document.createElement("section"), { className: "window" });
    win.append(title, body, grip);
    win.style.left = `${options.x}px`;
    win.style.top = `${options.y}px`;
    body.style.width = `${options.width}px`;
    body.style.height = `${options.height}px`;
    this.element.append(win);
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

    // dragging the title bar moves it (not off the top or left of the desk)
    drag(title, (dx, dy, start: { x: number; y: number }) => {
      win.style.left = `${Math.max(0, start.x + dx)}px`;
      win.style.top = `${Math.max(0, start.y + dy)}px`;
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
