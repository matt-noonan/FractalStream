// The messages between the page (main.ts) and the engine's worker
// (engine.ts). The engine's own messages are described in
// fractalstream-ui-web/src/Main.hs.

/** From the page to the engine. */
export type Request =
  | { type: "library" }
  | { type: "open"; section: number; entry: number }
  | { type: "openFile"; name: string; bytes: ArrayBuffer }
  /** Translate the old FractalStream file the engine said it had ("ancient"). */
  | { type: "translate" }
  /** What a viewer's "Show pointer position" shows for a screen-plane
      point (as for "event"): answered by "pointer". */
  | { type: "pointer"; viewer: number; x: number; y: number }
  | { type: "save"; views: string }
  /** Turn a sphere viewer's globe, as the pointer drags from one screen
      point to another (in units of the sphere's radius, y up). */
  | { type: "rotate"; viewer: number; fx: number; fy: number; tx: number; ty: number }
  | { type: "set"; name: string; text: string }
  /** Move, zoom, or resize a viewer: with where (in the new view's
      pixels: left, top, right and bottom, the last two not included) the
      page still shows the last picture, moved into the new view, if it
      does anywhere. */
  | { type: "view"; viewer: number; cx: number; cy: number; pixelSize: number; width: number; height: number;
      covered: [number, number, number, number] | null }
  | { type: "resize"; viewer: number; width: number; height: number }
  /** Whether a viewer is smoothed (four samples a pixel) to begin with. */
  | { type: "smoothing"; on: boolean }
  /** Whether a viewer is smoothed ("Use subpixel smoothing"). */
  | { type: "viewerSmoothing"; viewer: number; on: boolean }
  /** The last view, the next one, the original one, or the sphere (or
      the plane) instead: answered by "view". */
  | { type: "navigate"; viewer: number; what: Navigation }
  /** The view as it is now is a step in the viewer's history. */
  | { type: "record"; viewer: number }
  /** Go to a view: centered on (x, y) (on the sphere, facing it), with a
      CSS pixel this size. "where" asks where the viewer is now. */
  | { type: "jump"; viewer: number; x: number; y: number; pixelSize: number }
  | { type: "where"; viewer: number }
  /** Use a colorizer for the viewer (one it can't fill in the parameters
      of asks for its arguments first: "colorizerArguments"), ask for the
      arguments of the one in use, or give the answers. */
  | { type: "colorizer"; viewer: number; name: string }
  | { type: "colorizerArguments"; viewer: number }
  | { type: "colorizerAnswers"; viewer: number; name: string; answers: string[] }
  /** How many canvas pixels a CSS pixel is drawn with (see main.ts's
      `density`): tools are given a CSS pixel's size, as `pixel_size`. */
  | { type: "density"; value: number }
  /** Select a viewer's tool ("": none, to navigate). */
  | { type: "selectTool"; viewer: number; name: string }
  /** A pointer event for a viewer's tool, at screen-plane points: a click
      (at x1, y1), a double click, a drag (from x1, y1 to x2, y2) so far,
      or one finished. */
  | { type: "event"; viewer: number; kind: EventKind; x1: number; y1: number; x2: number; y2: number }
  | { type: "button"; viewer: number; label: string }
  /** Set a field of a viewer's tool's panel. */
  | { type: "setTool"; viewer: number; name: string; text: string }
  /** Stop the viewer's running handler (its tool's, or its overlay's);
      a tool's, also switching back to navigating, if `navigate`. */
  | { type: "stop"; viewer: number; navigate: boolean }
  /** The script editor: color and check a script (answered by "analysis"
      with the same id), the symbols' names, and rebuilding the project
      from an edited script. */
  | { type: "analyze"; id: number; text: string }
  | { type: "symbols" }
  | { type: "rebuild"; text: string };

/** 0 the last view, 1 the next, 2 the original one, 3 the sphere or the plane. */
export type Navigation = 0 | 1 | 2 | 3;

/** 0 a click, 1 a double click, 2 a drag so far, 3 a drag finished. */
export type EventKind = 0 | 1 | 2 | 3;

export interface LibraryEntry {
  title: string;
  description: string;
  /** A PNG, base64-encoded. */
  preview: string | null;
}

export interface LibrarySection {
  name: string;
  entries: LibraryEntry[];
}

export type Layout =
  | null
  | { kind: "vertical" | "horizontal"; children: Layout[] }
  | { kind: "panel"; title: string; body: Layout }
  | { kind: "tabs"; tabs: { label: string; body: Layout }[] }
  | { kind: "plain"; text: string }
  | { kind: "button"; label: string }
  | FieldLayout;

export interface FieldLayout {
  kind: "text" | "check" | "select" | "color";
  label: string;
  name: string;
  /** Which field it is, among the project's and the tools' (see Main.hs). */
  key: string;
  text: string;
  options?: string[];
  /** A color field's value, as #rrggbb. */
  color?: string | null;
}

export interface ViewerInfo {
  id: number;
  title: string;
  /** Its size, as its project gives it (in pixels). */
  width: number;
  height: number;
  /** Whether it shows the Riemann sphere (a globe), not the plane. */
  sphere: boolean;
  /** Whether it can (a complex viewer). */
  complex: boolean;
  /** Whether it's smoothed. */
  smooth: boolean;
  /** Its colorizer menu, if it has one. */
  colorizer: ColorizerMenu | null;
  tools: ToolInfo[];
}

export interface ColorizerMenu {
  /** The one in use, as chosen (with any arguments). */
  inUse: string;
  /** Whether the one in use has parameters ("Arguments..."). */
  arguments: boolean;
  items: { name: string; label: string; description: string | null; inUse: boolean; asks: boolean }[];
}

export interface ToolInfo {
  name: string;
  /** Its key (a single character), or "" if it has none. */
  shortcut: string;
  help: string;
}

/** The pointer events a tool handles (the rest navigate). */
export type Handled = "click" | "doubleClick" | "drag" | "dragDone";

/** A drawn shape, in screen-plane coordinates (see Drawing.hs), with its
    pen (s) and fill (f) colors as #rrggbb. */
export type Shape =
  | { k: "point"; p: [number, number]; s: string }
  | { k: "line"; a: [number, number]; b: [number, number]; s: string }
  | { k: "circle"; c: [number, number]; r: number; f: string | null; s: string }
  | { k: "rect"; a: [number, number]; b: [number, number]; f: string | null; s: string }
  | { k: "text"; p: [number, number]; t: string; s: string }
  | { k: "runs"; runs: [number, number][][]; s: string }
  | { k: "polygon"; pts: [number, number][]; f: string; s: string };

/** Where a script's error is (no place, if it hasn't one), and what it is. */
export interface ScriptError {
  from: number | null;
  to: number | null;
  message: string;
}

/** The page's view state, saved in a project file (web-properties): each
    viewer's window (in CSS pixels), and what it shows (a pixel's size is
    per CSS pixel, so it's the same at any screen density). */
export interface SavedViews {
  viewers?: Record<string, { x: number; y: number; width: number; height: number;
                             cx: number; cy: number; "pixel-size": number; minimized?: boolean }>;
}

/** Part of a viewer's render, as it goes (see "Frames" in
    fractalstream-ui-web/src/Main.hs): which render it is (each viewer's
    are numbered), its image's size and view, the rectangles drawn since
    the last patch (x, y, width, height, four numbers each) and their
    pixels (RGBA, a rectangle after another, row by row), and whether the
    render's done. */
export interface Patch {
  type: "patch";
  viewer: number;
  id: number;
  width: number;
  height: number;
  /** The center of the view, and a pixel's width and height, in model coordinates. */
  cx: number;
  cy: number;
  px: number;
  py: number;
  rects: number[];
  pixels: ArrayBuffer;
  done: boolean;
}

/** From the engine to the page. */
export type Message =
  | { type: "ready" }
  | { type: "library"; sections: LibrarySection[] }
  /** A project to show: one opened, or (rebuilt) the open one, built again
      from its edited script. */
  | { type: "project"; title: string; fileName: string; library: boolean; panel: Layout;
      viewers: ViewerInfo[]; views: SavedViews | null; script: string; rebuilt: boolean;
      /** A new project, with no file of its own yet (from a starting
          point, or a translation): saving it asks where. */
      new: boolean;
      /** Open its script editor: it's new, or its script doesn't build
          (it has no viewers until it does). */
      edit: boolean;
      /** A translation's notes. */
      notes: string[] }
  /** An old FractalStream 1.0 file, not opened: "translate" translates it. */
  | { type: "ancient"; name: string }
  | { type: "pointer"; viewer: number; text: string }
  /** The engine moved a viewer's view (a pixel's size in the canvas's pixels). */
  | { type: "view"; viewer: number; cx: number; cy: number; px: number; py: number; sphere: boolean }
  | { type: "where"; viewer: number; x: number; y: number; pixelSize: number }
  /** What to ask for a colorizer's arguments: each parameter, a note, and its text to start with. */
  | { type: "colorizerArguments"; viewer: number; name: string; fields: { param: string; note: string; text: string }[] }
  /** An edited script, colored and checked, with how to write it with
      mathematical symbols (offsets in UTF-16 units, the end not included). */
  | { type: "analysis"; id: number; spans: [number, number, string][]; error: ScriptError | null;
      normalizations: [number, number, string][] }
  | { type: "symbols"; names: [string, string][]; accents: [string, string][] }
  /** An edited script didn't build: the project's as it was. */
  | { type: "rebuildError"; error: ScriptError }
  /** The open project, saved: its file's name and contents. */
  | { type: "file"; name: string; bytes: ArrayBuffer }
  | { type: "field"; key: string; error: string | null; color?: string | null }
  /** A tool's handler changed a field. */
  | { type: "fieldText"; key: string; text: string; color?: string | null }
  | { type: "tool"; viewer: number; name: string | null; panel: Layout; handles: Handled[] }
  /** A tool's handler failed (and the viewer navigates again). */
  | { type: "toolError"; viewer: number; message: string }
  | { type: "drawing"; viewer: number; layers: Shape[][] }
  | { type: "eventDone"; viewer: number }
  /** A viewer's handler has been running for a while (and until runDone). */
  | { type: "running"; viewer: number; what: "tool" | "overlay"; name: string | null }
  | { type: "runDone"; viewer: number }
  /** A viewer couldn't be drawn (its next frame clears this). */
  | { type: "viewerError"; viewer: number; message: string }
  | { type: "error"; message: string }
  | Patch;
