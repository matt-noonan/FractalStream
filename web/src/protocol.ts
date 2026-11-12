// The messages between the page (main.ts) and the engine's worker
// (engine.ts). The engine's own messages are described in
// fractalstream-ui-web/src/Main.hs.

/** From the page to the engine. */
export type Request =
  | { type: "library" }
  | { type: "open"; section: number; entry: number }
  | { type: "openFile"; name: string; bytes: ArrayBuffer }
  | { type: "save" }
  | { type: "set"; name: string; text: string }
  | { type: "view"; viewer: number; cx: number; cy: number; pixelSize: number; width: number; height: number }
  | { type: "resize"; viewer: number; width: number; height: number }
  /** Whether frames are smoothed (four samples a pixel), for every viewer. */
  | { type: "smoothing"; on: boolean };

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
  | FieldLayout;

export interface FieldLayout {
  kind: "text" | "check" | "select" | "color";
  label: string;
  name: string;
  text: string;
  options?: string[];
}

export interface ViewerInfo {
  id: number;
  title: string;
  /** Its size, as its project gives it (in pixels). */
  width: number;
  height: number;
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
  | { type: "project"; title: string; fileName: string; library: boolean; panel: Layout; viewers: ViewerInfo[] }
  /** The open project, saved: its file's name and contents. */
  | { type: "file"; name: string; bytes: ArrayBuffer }
  | { type: "field"; name: string; error: string | null }
  | { type: "error"; message: string }
  | Frame;
