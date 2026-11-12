// What the web app's checks (check.mjs, check-tools.mjs) share: web/dist
// served on a free port, a headless Chromium page with the app open, every
// message the engine sends kept for looking at, and helpers to drive it.
//
// Uses Playwright and its browsers: the GHC wasm toolchain's (in
// ~/.ghc-wasm; install a browser with
// ~/.ghc-wasm/nodejs/lib/node_modules/.bin/playwright install chromium).
// Chromium, unless FS_BROWSER says firefox or webkit (Safari's engine);
// with FS_DPR=2, on a high-DPI screen.
import fs from "node:fs";
import http from "node:http";
import os from "node:os";
import path from "node:path";

const playwright = path.join(os.homedir(), ".ghc-wasm/nodejs/lib/node_modules/playwright/index.mjs");
const browsers = await import(playwright);
const browserType = browsers[process.env.FS_BROWSER || "chromium"];
if (!browserType) throw new Error(`no browser ${process.env.FS_BROWSER} (chromium, firefox or webkit)`);

const root = path.resolve(new URL("../../web/dist", import.meta.url).pathname);
const types = { ".html": "text/html", ".css": "text/css", ".js": "text/javascript",
                ".mjs": "text/javascript", ".wasm": "application/wasm", ".map": "application/json",
                ".ttf": "font/ttf" };

/** Serve web/dist (as dev-tools/web/serve.mjs does), open the app in a
    headless browser, and wait for the library. */
export async function start() {
  const server = http.createServer((req, res) => {
    const p = new URL(req.url, "http://localhost").pathname;
    const file = path.join(root, decodeURIComponent(p === "/" ? "/index.html" : p));
    if (!file.startsWith(root + path.sep) || !fs.existsSync(file)) { res.writeHead(404).end(); return; }
    res.writeHead(200, { "content-type": types[path.extname(file)] ?? "application/octet-stream" });
    fs.createReadStream(file).pipe(res);
  });
  await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
  const browser = await browserType.launch();
  // (FS_DPR=2 makes it a high-DPI screen, as a Retina one is)
  const page = await browser.newPage({ viewport: { width: 1600, height: 1000 },
                                       deviceScaleFactor: Number(process.env.FS_DPR || 1) });
  const app = new App(page, () => { browser.close(); server.close(); });
  // (every message from the engine, kept on the page; and a way to send
  // it requests directly)
  await page.addInitScript(() => {
    const W = window.Worker;
    window.__messages = [];
    window.Worker = class extends W {
      constructor(...a) {
        super(...a);
        window.__send = (m) => this.postMessage(m);
        this.addEventListener("message", (e) => window.__messages.push(e.data));
      }
    };
  });
  await page.goto(`http://127.0.0.1:${server.address().port}/`);
  await page.waitForSelector("#library li", { timeout: 60000 });
  return app;
}

export class App {
  constructor(page, close) {
    this.page = page;
    this.close = close;
    /** Errors on the page (exceptions, and console errors) since `clearErrors`. */
    this.errors = [];
    page.on("pageerror", (e) => this.errors.push(e.message));
    page.on("console", (m) => { if (m.type() === "error") this.errors.push(m.text().split("\n")[0]); });
  }

  clearErrors() { this.errors = []; }

  /** The engine's messages of a type, so far (frames without their pixels). */
  messages(type) {
    return this.page.evaluate((t) => window.__messages.filter((m) => m.type === t)
      .map((m) => ({ ...m, pixels: undefined })), type);
  }

  forgetMessages() { return this.page.evaluate(() => (window.__messages = [])); }

  send(request) { return this.page.evaluate((r) => window.__send(r), request); }

  /** Wait until a message passes the test (a function's source, run on the page with the message). */
  waitForMessage(test, timeout = 30000) {
    return this.page.waitForFunction((src) => window.__messages.some(eval(src)), test.toString(), { timeout });
  }

  /** Wait for a project to open, and every viewer to have a frame; give
      its "project" message. */
  async opened(timeout = 60000) {
    await this.page.waitForFunction(() => window.__messages.some((m) => m.type === "project"), null, { timeout });
    await this.page.waitForFunction(() => {
      const vs = [...document.querySelectorAll(".viewer")];
      return vs.length > 0 && vs.every((v) => Number(v.dataset.frames ?? 0) > 0);
    }, null, { timeout });
    return (await this.messages("project")).pop();
  }

  /** Open a project from the library, by its title (or the start of it). */
  async openLibrary(title) {
    await this.forgetMessages();
    await this.page.locator("#library li", { hasText: title }).first().click();
    return this.opened();
  }

  /** Open a project file, as if it were dropped on the page. */
  async openFile(file) {
    await this.forgetMessages();
    const text = fs.readFileSync(file, "utf8");
    await this.page.evaluate(([name, text]) => {
      const dt = new DataTransfer();
      dt.items.add(new File([text], name));
      document.dispatchEvent(new DragEvent("drop", { dataTransfer: dt, bubbles: true, cancelable: true }));
    }, [path.basename(file), text]);
    return this.opened();
  }

  viewer(id) { return this.page.locator(".viewer").nth(id); }
  canvas(id) { return this.page.locator(".viewer canvas").nth(id); }
  async frames(id) { return Number(await this.viewer(id).getAttribute("data-frames")); }

  /** Make a viewer the active one (the palette's), by right-clicking it. */
  async activate(id) {
    const box = await this.canvas(id).boundingBox();
    await this.page.mouse.click(box.x + 5, box.y + 5, { button: "right" });
  }

  /** Select a tool of the active viewer by its shortcut key, and wait for it. */
  async pressTool(key, name) {
    await this.page.keyboard.press(key);
    await this.waitForMessage(`(m) => m.type === "tool" && m.name === ${JSON.stringify(name)}`, 10000);
  }

  /** The tool palette's pressed button(s). */
  async pressed() {
    return (await this.page.locator("#tools button[aria-pressed=true]").allTextContents()).join();
  }

  /** A viewer's latest drawing, as a flat list of shapes (or null). */
  async drawing(id) {
    const d = (await this.messages("drawing")).filter((m) => m.viewer === id).pop();
    return d ? d.layers.flat() : null;
  }
}

/** Counting checks: each prints "ok" or "FAIL" and what it checked. */
export class Checks {
  constructor() { this.failed = 0; }
  check(ok, what) {
    console.log(`${ok ? "ok  " : "FAIL"} ${what}`);
    if (!ok) this.failed++;
  }
  note(what) { console.log(`     ${what}`); }
}

/** "3 shapes: point, line" */
export function kinds(shapes) {
  return shapes ? `${shapes.length} shapes: ${[...new Set(shapes.map((s) => s.k))].join(", ")}` : "no drawing";
}
