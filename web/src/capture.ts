// Capturing what a viewer shows, from its menu's Capture submenu, as the
// desktop app's does: a picture (Image...), or a movie made by running a
// script for each frame (Movie...). The engine renders (a picture comes
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

/** The last movie's settings (and script), to start the next one from. */
const last = { script: null as string | null, frames: 150, fps: 30, loop: false, smooth: true, drawings: false };

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

  constructor(private readonly send: Send, private readonly say: Say) {}

  /** Capture > Movie...: the dialog (with the last movie's script, or one
      to start with from the engine). */
  ask(viewer: number, title: string) {
    if (this.recording) { this.say("A movie's being recorded already."); return; }
    this.viewer = viewer;
    this.title = title;
    if (last.script === null) this.send({ type: "movieTemplate", viewer });
    else this.showDialog(last.script);
  }

  template(m: Extract<Message, { type: "movieTemplate" }>) {
    if (m.viewer === this.viewer && !this.box) this.showDialog(m.script);
  }

  private showDialog(script: string) {
    const { box, close } = dialog(`Make a movie of ${this.title}`, () => !this.recording);
    this.box = box;
    this.close = () => { close(); this.box = null; };
    box.classList.add("movie-dialog");
    this.script = el("textarea", { className: "movie-script", value: script, spellcheck: false, rows: 7 });
    this.problem = el("div", { className: "form-error" });
    this.script.addEventListener("input", () => {
      clearTimeout(this.checkTimer);
      this.checkTimer = window.setTimeout(() => this.check(), 300);
    });
    const number = (value: number) => el("input", { type: "text", value: String(value), spellcheck: false });
    const frames = number(last.frames), fps = number(last.fps);
    const loop = checkbox("Loop (the last frame comes just before the first again)", last.loop);
    const smooth = checkbox("Use subpixel smoothing", last.smooth);
    const drawings = checkbox("Include what the viewer's tools have drawn", last.drawings);
    const record = () => {
      const f = Math.round(Number(frames.value)), r = Math.round(Number(fps.value));
      if (!(f >= 1) || !(r >= 1)) { this.problem!.textContent = "The frames and frames per second should be positive whole numbers."; return; }
      Object.assign(last, { script: this.script!.value, frames: f, fps: r, loop: loop.input.checked,
                            smooth: smooth.input.checked, drawings: drawings.input.checked });
      this.start();
    };
    box.append(
      el("p", { textContent: "Run this script for each frame, with t going from 0 to 1:" }),
      this.script, this.problem,
      el("div", { className: "capture-grid" },
         el("span", { textContent: "Frames:" }), frames, el("span", { textContent: "Frames per second:" }), fps),
      loop.row, smooth.row, drawings.row,
      buttons(button("Cancel", () => this.close?.()), button("Record", record)));
    this.script.focus();
    this.check();
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
  private start() {
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
    this.send({ type: "movieStart", viewer: this.viewer, script: last.script!, frames: last.frames, fps: last.fps,
                loop: last.loop, smooth: last.smooth, drawings: last.drawings });
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
