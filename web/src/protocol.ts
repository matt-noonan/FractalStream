// The messages between the page (main.ts) and the engine's worker
// (engine.ts). The engine's own messages are described in
// fractalstream-ui-web/src/Main.hs.

/** From the page to the engine. */
export type Request =
  | { type: "library" }
  | { type: "open"; section: number; entry: number }
  | { type: "openFile"; name: string; bytes: ArrayBuffer }
  | { type: "save"; views: string }
  /** Turn a sphere viewer's globe, as the pointer drags from one screen
      point to another (in units of the sphere's radius, y up). */
  | { type: "rotate"; viewer: number; fx: number; fy: number; tx: number; ty: number }
  | { type: "set"; name: string; text: string }
  | { type: "view"; viewer: number; cx: number; cy: number; pixelSize: number; width: number; height: number }
  | { type: "resize"; viewer: number; width: number; height: number }
  /** Whether frames are smoothed (four samples a pixel), for every viewer. */
  | { type: "smoothing"; on: boolean }
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
  tools: ToolInfo[];
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

/** A viewer's frame: RGBA pixels, and the view they show. */
export interface Frame {
  type: "frame";
  viewer: number;
  width: number;
  height: number;
  /** The center of the view, and a pixel's width and height, in model coordinates. */
  cx: number;
  cy: number;
  px: number;
  py: number;
  pixels: ArrayBuffer;
}

/** From the engine to the page. */
export type Message =
  | { type: "ready" }
  | { type: "library"; sections: LibrarySection[] }
  /** A project to show: one opened, or (rebuilt) the open one, built again
      from its edited script. */
  | { type: "project"; title: string; fileName: string; library: boolean; panel: Layout;
      viewers: ViewerInfo[]; views: SavedViews | null; script: string; rebuilt: boolean }
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
  | Frame;
