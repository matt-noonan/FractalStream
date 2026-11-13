// Capturing what a viewer shows, from its menu's Capture submenu, as the
// desktop app's does: a picture (Image...), or a movie made by running a
// script for each frame (Movie...), or (from the "Animate along path"
// tool) one moving a viewer's coordinate along a path drawn in it, with
// that viewer as an inset if asked for. A movie can be of any of the
// project's viewers. The engine renders (a picture comes
// back as a PNG to save; a movie's frames come one by one); the page
// encodes a movie's frames as an MP4, with the browser's own video
// encoder (WebCodecs), by way of Mediabunny.

import { Output, Mp4OutputFormat, BufferTarget, VideoSampleSource, VideoSample, canEncodeVideo,
         QUALITY_HIGH, type VideoCodec } from "mediabunny";
import type { Request, Message } from "./protocol.js";
import { download } from "./files.js";

type Send = (r: Request) => void;
/** Tell the user something: an error (as a notice), or what happened. */
type Say = (text: string, isError?: boolean) => void;

function el<K extends keyof HTMLElementTagNameMap>(tag: K, props: Partial<HTMLElementTagNameMap[K]> = {},
                                                 ...children: (Node | string)[]): HTMLElementTagNameMap[K] {
  const e = Object.assign(document.createElement(tag), props);
  e.append(...children);
  return e;
}

/** A dialog over the whole page (as the one asking about saving tables):
    its box, and a way to close it. Escape closes it too, unless `escape`
    says otherwise. */
function dialog(title: string, escape: () => boolean = () => true): { box: HTMLElement; close: () => void } {
  const box = el("div", { className: "page-dialog-box" }, el("div", { className: "form-title", textContent: title }));
  const outer = el("div", { className: "page-dialog", role: "dialog" }, box);
  const close = () => outer.remove();
  outer.addEventListener("keydown", (e) => { if (e.key === "Escape" && escape()) { e.preventDefault(); close(); } });
  document.body.append(outer);
  return { box, close };
}

function checkbox(label: string, on: boolean): { row: HTMLElement; input: HTMLInputElement } {
  const input = el("input", { type: "checkbox", checked: on });
  return { row: el("label", { className: "capture-check" }, input, ` ${label}`), input };
}

function buttons(...bs: HTMLButtonElement[]): HTMLElement {
  return el("div", { className: "form-buttons" }, bs[0], el("span", { className: "form-gap" }), ...bs.slice(1));
}

function button(label: string, run: () => void): HTMLButtonElement {
  const b = el("button", { type: "button", textContent: label });
  b.addEventListener("click", run);
  return b;
}

// ---------------------------------------------------------------------
// Pictures

/** Capture > Image...: as the desktop asks, how many times the viewer's
    size to make the picture, and whether to smooth it. */
export function askImage(viewer: number, send: Send) {
  const { box, close } = dialog("Save a picture of the view");
  const multiplier = el("input", { type: "text", value: "1", spellcheck: false });
  const smooth = checkbox("Use subpixel smoothing", true);
  const error = el("div", { className: "form-error" });
  const ok = () => {
    const m = Number(multiplier.value.trim());
    if (!(m > 0) || !Number.isFinite(m)) { error.textContent = "The size multiplier should be a positive number."; return; }
    send({ type: "captureImage", viewer, multiplier: m, smooth: smooth.input.checked });
    close();
  };
  multiplier.addEventListener("keydown", (e) => { if (e.key === "Enter") ok(); });
  box.append(
    el("label", { className: "form-field" }, el("span", { textContent: "Size multiplier:" }), multiplier),
    smooth.row, error,
    buttons(button("Cancel", close), button("Save", ok)));
  multiplier.focus();
  multiplier.select();
}

/** The name and kind of a file the engine made, to save it as. */
export function saveDownload(m: Extract<Message, { type: "download" }>) {
  download(m.name, m.bytes, m.name.endsWith(".png") ? "image/png" : "application/octet-stream");
}

// ---------------------------------------------------------------------
// Movies

/** The last movie's settings (and script), to start the next one from:
    which viewer it was of, too, and whether it had an inset, and where. */
const last = { script: null as string | null, frames: 150, fps: 30, loop: false, smooth: true, drawings: false,
               viewer: null as string | null, inset: false, corner: 3 };

/** A project's viewer, as a movie can be made of it. */
export interface MovieViewer { id: number; title: string; animates: string | null }

/** What a movie animates: a script, or a path drawn in a viewer (its
    points in that viewer's screen plane). */
type Source = { kind: "script"; script: string } | { kind: "path"; drawnIn: MovieViewer; path: [number, number][] };

const CORNERS = ["top left", "top right", "bottom left", "bottom right"];

/** Whether a path ends about where it starts (within 5% of its length,
    as the desktop has it): a movie along it loops, to begin with. */
function closedPath(path: [number, number][]): boolean {
  let total = 0;
  for (let i = 1; i < path.length; i++) total += Math.hypot(path[i][0] - path[i - 1][0], path[i][1] - path[i - 1][1]);
  const [a, b] = [path[0], path[path.length - 1]];
  return path.length > 2 && Math.hypot(a[0] - b[0], a[1] - b[1]) < 0.05 * total;
}

/** Making a movie: the dialog asking how (its script checked as it's
    typed, as the desktop's is), then its progress while it's recorded
    and encoded, and saving it. One at a time. */
export class Movies {
  private box: HTMLElement | null = null;
  private close: (() => void) | null = null;
  private viewer = -1;
  private title = "";
  private script: HTMLTextAreaElement | null = null;
  private problem: HTMLElement | null = null;
  private checkId = 0;
  private checkTimer: number | undefined;
  private recording = false;
  // the encoder, once the first frame says how big the movie is
  private fps = 30;
  private output: Output | null = null;
  private source: VideoSampleSource | null = null;
  private chain: Promise<void> = Promise.resolve();
  private failed: string | null = null;
  private progress: HTMLProgressElement | null = null;
  private progressText: HTMLElement | null = null;

  /** (`viewers`: the project's, as they are now) */
  constructor(private readonly send: Send, private readonly say: Say,
              private readonly viewers: () => MovieViewer[]) {}

  /** Capture > Movie...: the dialog (with the last movie's script, or one
      to start with from the engine), from this viewer's menu. */
  ask(viewer: number, title: string) {
    if (this.recording) { this.say("A movie's being recorded already."); return; }
    this.viewer = viewer;
    this.title = title;
    if (last.script === null) this.send({ type: "movieTemplate", viewer });
    else this.showDialog({ kind: "script", script: last.script });
  }

  template(m: Extract<Message, { type: "movieTemplate" }>) {
    if (m.viewer === this.viewer && !this.box) this.showDialog({ kind: "script", script: m.script });
  }

  /** "Animate along path": a path's been drawn in a viewer. */
  askPath(drawnIn: number, path: [number, number][]) {
    if (this.recording) { this.say("A movie's being recorded already."); return; }
    if (this.box) return;
    const from = this.viewers().find((v) => v.id === drawnIn);
    if (!from?.animates) return;
    this.viewer = drawnIn;
    this.title = from.title;
    this.showDialog({ kind: "path", drawnIn: from, path });
  }

  private showDialog(source: Source) {
    const { box, close } = dialog("Make a movie", () => !this.recording);
    this.box = box;
    this.close = () => { close(); this.box = null; };
    box.classList.add("movie-dialog");
    // which viewer to record: the last one, if it's still here; else, for
    // a path, another than the one it was drawn in (a path in a parameter
    // plane is for watching the dynamical plane change); else this one
    const viewers = this.viewers();
    const others = viewers.filter((v) => v.id !== this.viewer);
    const first = viewers.find((v) => v.title === last.viewer)
      ?? (source.kind === "path" ? others[0] : undefined) ?? viewers.find((v) => v.id === this.viewer) ?? viewers[0];
    const which = el("select", {}, ...viewers.map((v) =>
      el("option", { value: String(v.id), textContent: v.title, selected: v === first })));
    which.setAttribute("aria-label", "Viewer to record");
    this.problem = el("div", { className: "form-error" });
    const number = (value: number) => el("input", { type: "text", value: String(value), spellcheck: false });
    const frames = number(last.frames), fps = number(last.fps);
    const smooth = checkbox("Use subpixel smoothing", last.smooth);
    const drawings = checkbox("Include what the viewer's tools have drawn", last.drawings);
    const parts: HTMLElement[] = [];
    let loop: ReturnType<typeof checkbox>;
    let inset: ReturnType<typeof checkbox> | null = null;
    let corner: HTMLSelectElement | null = null;
    if (source.kind === "script") {
      this.script = el("textarea", { className: "movie-script", value: source.script, spellcheck: false, rows: 7 });
      this.script.addEventListener("input", () => {
        clearTimeout(this.checkTimer);
        this.checkTimer = window.setTimeout(() => this.check(), 300);
      });
      parts.push(el("p", { textContent: "Run this script for each frame, with t going from 0 to 1:" }),
                 this.script, this.problem);
      loop = checkbox("Loop (the last frame comes just before the first again)", last.loop);
    } else {
      this.script = null;
      const v = source.drawnIn;
      parts.push(el("p", { textContent: `${v.animates} will move along the path drawn in “${v.title}”.` }), this.problem);
      loop = checkbox("Loop back to start (along a straight line from the path's end)", closedPath(source.path));
      inset = checkbox(`Show “${v.title}” as an inset, marking ${v.animates}, in the`, last.inset);
      corner = el("select", { disabled: !last.inset }, ...CORNERS.map((c, i) =>
        el("option", { value: String(i), textContent: `${c} corner`, selected: i === last.corner })));
      corner.setAttribute("aria-label", "The inset's corner");
      inset.input.addEventListener("change", () => { corner!.disabled = !inset!.input.checked; });
    }
    const record = () => {
      const f = Math.round(Number(frames.value)), r = Math.round(Number(fps.value));
      if (!(f >= 1) || !(r >= 1)) { this.problem!.textContent = "The frames and frames per second should be positive whole numbers."; return; }
      const chosen = viewers.find((v) => String(v.id) === which.value) ?? first;
      Object.assign(last, { frames: f, fps: r, smooth: smooth.input.checked, drawings: drawings.input.checked,
                            viewer: chosen.title });
      this.title = chosen.title;
      const common = { viewer: chosen.id, frames: f, fps: r, loop: loop.input.checked,
                       smooth: smooth.input.checked, drawings: drawings.input.checked };
      if (source.kind === "script") {
        Object.assign(last, { script: this.script!.value, loop: loop.input.checked });
        this.start({ type: "movieStart", script: last.script!, ...common });
      } else {
        Object.assign(last, { inset: inset!.input.checked, corner: Number(corner!.value) });
        this.start({ type: "moviePathStart", drawnIn: source.drawnIn.id, path: source.path, ...common,
                     corner: last.inset ? last.corner : -1 });
      }
    };
    box.append(
      el("label", { className: "form-field" }, el("span", { textContent: "Record:" }), which),
      ...parts,
      el("div", { className: "capture-grid" },
         el("span", { textContent: "Frames:" }), frames, el("span", { textContent: "Frames per second:" }), fps),
      loop.row, smooth.row, drawings.row,
      ...(inset && corner ? [el("div", { className: "capture-inset" }, inset.row, corner)] : []),
      buttons(button("Cancel", () => this.close?.()), button("Record", record)));
    if (this.script) { this.script.focus(); this.check(); } else which.focus();
  }

  private check() {
    if (!this.script) return;
    this.send({ type: "movieCheck", id: ++this.checkId, script: this.script.value });
  }

  checked(m: Extract<Message, { type: "movieCheck" }>) {
    if (m.id !== this.checkId || !this.problem || this.recording) return;
    this.problem.textContent = m.error ? `⚠ ${m.error.message}` : "";
  }

  /** Record it: the dialog shows the progress (and Stop). */
  private start(request: Extract<Request, { type: "movieStart" | "moviePathStart" }>) {
    this.recording = true;
    this.failed = null;
    this.output = null;
    this.source = null;
    this.chain = Promise.resolve();
    this.fps = last.fps;
    this.progress = el("progress", { max: last.frames, value: 0 });
    this.progressText = el("p", { textContent: "Starting…" });
    const stop = button("Stop", () => { stop.disabled = true; this.send({ type: "movieStop" }); });
    this.box!.replaceChildren(
      el("div", { className: "form-title", textContent: `Making a movie of ${this.title}` }),
      this.progressText, this.progress, el("div", { className: "form-buttons" }, el("span", { className: "form-gap" }), stop));
    this.send(request);
  }

  /** A frame: encoded in turn (the engine's told once it has been, so
      that it doesn't run far ahead of the encoder). */
  frame(m: Extract<Message, { type: "movieFrame" }>) {
    this.chain = this.chain.then(async () => {
      if (this.failed) return;
      try {
        if (!this.source) await this.startEncoder(m.width, m.height);
        const sample = new VideoSample(new Uint8Array(m.pixels), {
          format: "RGBA", codedWidth: m.width, codedHeight: m.height,
          timestamp: (m.index - 1) / this.fps, duration: 1 / this.fps });
        await this.source!.add(sample);
        sample.close();
        if (this.progress) this.progress.value = m.index;
        if (this.progressText) this.progressText.textContent = `Frame ${m.index} of ${m.total}`;
      } catch (e) {
        this.failed = `Couldn't encode the movie: ${e instanceof Error ? e.message : e}`;
        this.send({ type: "movieStop" });
      } finally {
        this.send({ type: "movieAck", frame: m.index });
      }
    });
  }

  private async startEncoder(width: number, height: number) {
    // (H.264, if the browser can; else another codec MP4 can hold)
    let codec: VideoCodec | null = null;
    for (const c of ["avc", "vp9", "av1"] as VideoCodec[]) {
      if (await canEncodeVideo(c, { width, height })) { codec = c; break; }
    }
    if (!codec) throw new Error("this browser has no video encoder for an MP4 (WebCodecs)");
    this.output = new Output({ format: new Mp4OutputFormat(), target: new BufferTarget() });
    this.source = new VideoSampleSource({ codec, bitrate: QUALITY_HIGH });
    this.output.addVideoTrack(this.source, { frameRate: this.fps });
    await this.output.start();
  }

  /** The engine's done: the movie's finished and saved (unless it was
      stopped, or failed). */
  done(m: Extract<Message, { type: "movieDone" }>) {
    this.chain = this.chain.then(async () => {
      const output = this.output;
      this.recording = false;
      this.close?.();
      const failed = m.error ?? this.failed;
      if (failed) { await output?.cancel(); this.say(failed, true); return; }
      if (m.stopped) { await output?.cancel(); this.say("The movie was stopped."); return; }
      if (!output) { this.say("The movie has no frames."); return; }
      await output.finalize();
      const buffer = (output.target as BufferTarget).buffer!;
      download(`${this.title}.mp4`, new Blob([buffer], { type: "video/mp4" }));
      this.say(m.issues ? `Saved ${this.title}.mp4. ${m.issues}` : `Saved ${this.title}.mp4.`);
    });
  }
}
