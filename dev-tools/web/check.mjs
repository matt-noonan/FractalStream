// Check the web app (web/dist, from dev-tools/web/build.sh) in a headless
// browser: open every project in the library, one after another, and see
// that each of its viewers is drawn, with no errors on the page.
//
//   node dev-tools/web/check.mjs [--screenshots DIR]
//
// Uses Playwright and its Chromium: the GHC wasm toolchain's (in
// ~/.ghc-wasm; install the browser with
// ~/.ghc-wasm/nodejs/lib/node_modules/.bin/playwright install chromium).
// The exit status is 1 if any project failed.
import fs from "node:fs";
import http from "node:http";
import os from "node:os";
import path from "node:path";

const playwright = path.join(os.homedir(), ".ghc-wasm/nodejs/lib/node_modules/playwright/index.mjs");
const { chromium } = await import(playwright);

const args = process.argv.slice(2);
const shots = args[0] === "--screenshots" ? args[1] : null;
if (shots) fs.mkdirSync(shots, { recursive: true });

// Serve web/dist, as dev-tools/web/serve.mjs does, on a free port.
const root = path.resolve(new URL("../../web/dist", import.meta.url).pathname);
const types = { ".html": "text/html", ".css": "text/css", ".js": "text/javascript",
                ".mjs": "text/javascript", ".wasm": "application/wasm", ".map": "application/json" };
const server = http.createServer((req, res) => {
  const p = new URL(req.url, "http://localhost").pathname;
  const file = path.join(root, decodeURIComponent(p === "/" ? "/index.html" : p));
  if (!file.startsWith(root + path.sep) || !fs.existsSync(file)) { res.writeHead(404).end(); return; }
  res.writeHead(200, { "content-type": types[path.extname(file)] ?? "application/octet-stream" });
  fs.createReadStream(file).pipe(res);
});
await new Promise((resolve) => server.listen(0, "127.0.0.1", resolve));
const url = `http://127.0.0.1:${server.address().port}/`;

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1600, height: 1000 } });
let errors = [];
page.on("pageerror", (e) => errors.push(e.message));
page.on("console", (m) => { if (m.type() === "error") errors.push(m.text().split("\n")[0]); });

let failed = 0;
try {
  await page.goto(url);
  await page.waitForSelector("#library li", { timeout: 60000 });
  const n = await page.locator("#library li").count();
  for (let i = 0; i < n; i++) {
    errors = [];
    const item = page.locator("#library li").nth(i);
    const title = (await item.textContent()).trim();
    const t = Date.now();
    await item.click();
    let result;
    try {
      await page.waitForFunction((t) => document.querySelector("#project h1")?.textContent === t,
                                 title, { timeout: 30000 });
      await page.waitForFunction(() => {
        const vs = [...document.querySelectorAll(".viewer")];
        return vs.length > 0 && vs.every((v) => Number(v.dataset.frames ?? 0) > 0);
      }, null, { timeout: 60000 });
      const viewers = await page.locator(".viewer").count();
      result = `${viewers} viewer(s) drawn in ${Date.now() - t} ms`;
    } catch (e) {
      result = `FAILED: ${e.message.split("\n")[0]}`;
    }
    if (errors.length) result += `; errors: ${errors.join(" | ")}`;
    if (result.startsWith("FAILED") || errors.length) failed++;
    if (shots) await page.screenshot({ path: path.join(shots, `${String(i).padStart(2, "0")}.png`) });
    console.log(`${title}: ${result}`);
  }
  console.log(`${n} projects, ${failed} failed`);
} catch (e) {
  console.log(`FAILED to load the app: ${e.message.split("\n")[0]}`);
  failed++;
} finally {
  await browser.close();
  server.close();
}
process.exit(failed ? 1 : 0);
