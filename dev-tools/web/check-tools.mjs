// Check the web app's tools (web/dist, from dev-tools/web/build.sh) in a
// headless browser:
//
//   node dev-tools/web/check-tools.mjs [--screenshots DIR] [SECTION ...]
//
// Sections (all of them, by default):
//
//   drawing   the page's drawing of tools' shapes, on its own: shapes
//             reaching very far from the view (a line to 10^10, say) are
//             drawn quickly, and on the pixels they'd have up close
//   tools     selecting tools (the default Select tool, shortcut keys),
//             clicks, drags, double-clicks routed to them or navigating;
//             a tool's panel, its checkbox and button; a tool setting its
//             own fields; overlays; drawing on the sphere
//   refresh   overlays and tools refreshed as the desktop app does: an
//             overlay when a value changes, a tool after each render
//   sweep     every tool of every library project compiles
//   stop      a handler that never finishes (dev-tools/web/endless.fs):
//             the bar, Stop, Stop and switch to Navigate, and an overlay
//   editor    the script editor: coloring, an error marked, symbols, and
//             rebuilding (windows, views and tools kept), or failing to
//   panel     the configuration panel: a text box's whole value as its
//             tooltip, as a tool changes it; widening the panel from its
//             left edge, remembered
//
// See harness.mjs for what it needs. The exit status is 1 if any check
// failed.
import fs from "node:fs";
import path from "node:path";
import { start, Checks, kinds } from "./harness.mjs";

const args = process.argv.slice(2);
let shots = null;
if (args[0] === "--screenshots") { shots = args[1]; args.splice(0, 2); fs.mkdirSync(shots, { recursive: true }); }
const all = { drawing, tools, refresh, sweep, stop, editor, panel };
const wanted = args.length ? args : Object.keys(all);
for (const w of wanted) if (!all[w]) { console.error(`no section "${w}" (there are: ${Object.keys(all).join(", ")})`); process.exit(2); }

const c = new Checks();
const app = await start();
const { page } = app;
const shot = (name, clip) => shots ? page.screenshot({ path: path.join(shots, `${name}.png`), clip }) : null;
try {
  for (const w of wanted) {
    console.log(`== ${w}`);
    app.clearErrors();
    try {
      await all[w]();
    } catch (e) {
      c.check(false, `${w}: ${e.message.split("\n")[0]}`);
    }
    c.check(app.errors.length === 0, `no errors on the page${app.errors.length ? ": " + app.errors.join(" | ") : ""}`);
  }
} finally {
  await app.close();
}
console.log(c.failed ? `${c.failed} failed` : "all ok");
process.exit(c.failed ? 1 : 0);

// ---------------------------------------------------------------------

async function drawing() {
  const r = await page.evaluate(async () => {
    const { drawLayers } = await import("./drawing.js");
    const canvas = Object.assign(document.createElement("canvas"), { width: 1200, height: 1200 });
    const ctx = canvas.getContext("2d");
    // a view centered at 0, 1/256 a pixel
    const px = 1 / 256, toPx = ([x, y]) => [x / px + 600, -y / px + 600];
    const draw = (layers) => { ctx.clearRect(0, 0, 1200, 1200); drawLayers(ctx, layers, toPx, [px, px], 2); };
    const time = (layers) => {
      const t = performance.now();
      for (let i = 0; i < 10; i++) draw(layers);
      ctx.getImageData(0, 0, 1, 1); // (waits for the drawing to be done)
      return (performance.now() - t) / 10;
    };
    const s = "#ffffff";
    const orbit = [];
    let x = 0.3, y = 0.5;
    for (let i = 0; i < 2000; i++) {
      const nx = x * x - y * y - 0.1226, ny = 2 * x * y + 0.7449;
      orbit.push({ k: "line", a: [x, y], b: [nx, ny], s });
      x = nx; y = ny;
    }
    const times = {
      "a line from 0 to 10^10": time([[{ k: "line", a: [0, 0], b: [1e10, 0], s }]]),
      "a line from -10^300 to 10^300": time([[{ k: "line", a: [-1e300, -1e300], b: [1e300, 1e300], s }]]),
      "a circle of radius 10^10 through the view": time([[{ k: "circle", c: [1e10, 0], r: 1e10, f: null, s }]]),
      "a filled circle of radius 10^10 around the view": time([[{ k: "circle", c: [0, 0], r: 1e10, f: "#ff0000", s }]]),
      "a filled rectangle out to 10^12": time([[{ k: "rect", a: [-1e12, -1e12], b: [1, 1], f: "#ff0000", s }]]),
      "an orbit of 2000 lines": time([orbit]),
    };
    // a line to 10^10 is drawn on the pixels a short one in the same
    // direction is
    draw([[{ k: "line", a: [0, 0], b: [1e10, 1e10], s }]]);
    const far = ctx.getImageData(0, 0, 1200, 1200).data.slice();
    draw([[{ k: "line", a: [0, 0], b: [10, 10], s }]]);
    const near = ctx.getImageData(0, 0, 1200, 1200).data;
    let differing = 0, lit = 0;
    for (let i = 0; i < far.length; i++) if (Math.abs(far[i] - near[i]) > 8) differing++;
    for (let i = 3; i < near.length; i += 4) if (near[i]) lit++;
    return { times, differing, lit };
  });
  for (const [what, ms] of Object.entries(r.times)) c.check(ms < 20, `${what}: ${ms.toFixed(2)} ms`);
  c.check(r.lit > 1000 && r.differing === 0,
          `a line to 10^10 covers the pixels a short one does (${r.lit} lit, ${r.differing} bytes differ)`);
}

async function tools() {
  // Mandelbrot and Julia: the parameter plane's Select tool picks C
  let project = await app.openLibrary("Mandelbrot and Julia sets together");
  const id = (title) => project.viewers.find((v) => v.title === title).id;
  let pp = id("Parameter plane"), dp = id("Dynamical plane");
  const selected = (await app.messages("tool")).find((t) => t.viewer === pp);
  c.check(selected?.name === "Select C", `the parameter plane starts with its Select tool (${selected?.name})`);
  const cBox = page.locator(".panel .field input[type=text]").first();
  const before = await cBox.inputValue();
  const framesBefore = await app.frames(dp);
  const pbox = await app.canvas(pp).boundingBox();
  await page.mouse.click(pbox.x + pbox.width * 0.3, pbox.y + pbox.height * 0.4);
  await page.waitForFunction(([b]) => document.querySelector(".panel .field input[type=text]").value !== b, [before], { timeout: 10000 }).catch(() => {});
  c.check((await cBox.inputValue()) !== before, `clicking the parameter plane sets C (${before} -> ${await cBox.inputValue()})`);
  await page.waitForFunction(([n, v]) => Number(document.querySelectorAll(".viewer")[v].dataset.frames) > n,
                             [framesBefore, dp], { timeout: 10000 }).catch(() => {});
  c.check((await app.frames(dp)) > framesBefore, "and the dynamical plane redraws");

  // Trace, by its key: a click traces an orbit, a drag retraces it (and
  // doesn't pan), and n deselects it (erasing, its deactivated handler)
  await app.activate(dp);
  await app.pressTool("t", "Trace");
  c.check((await app.pressed()) === "Trace", `t selects Trace (${await app.pressed()})`);
  const dbox = await app.canvas(dp).boundingBox();
  const drawings = async () => (await app.messages("drawing")).filter((m) => m.viewer === dp).length;
  let n = await drawings();
  await page.mouse.click(dbox.x + dbox.width * 0.55, dbox.y + dbox.height * 0.45);
  await app.waitForMessage(`(m) => m.type === "eventDone"`, 10000);
  await page.waitForTimeout(200);
  const traced = await app.drawing(dp);
  c.check(traced?.some((s) => s.k === "line") && traced.some((s) => s.k === "point"), `a click traces an orbit (${kinds(traced)})`);
  await shot("trace", dbox);
  n = await drawings();
  const framesD = await app.frames(dp);
  await page.mouse.move(dbox.x + dbox.width * 0.55, dbox.y + dbox.height * 0.45);
  await page.mouse.down();
  for (let i = 1; i <= 20; i++) await page.mouse.move(dbox.x + dbox.width * (0.55 - i * 0.01), dbox.y + dbox.height * 0.45);
  await page.mouse.up();
  await page.waitForTimeout(1000);
  c.check((await drawings()) - n >= 2, `dragging retraces it (${(await drawings()) - n} new drawings)`);
  c.check((await app.frames(dp)) === framesD, "and doesn't pan the view");
  await page.keyboard.press("n");
  await page.waitForTimeout(500);
  const erased = await app.drawing(dp);
  c.check(erased?.length === 0 && (await app.pressed()) === "Navigate", "n goes back to Navigate, erasing the orbit");

  // an overlay, drawn with the frames
  project = await app.openLibrary("Period 2: parameter plane");
  const overlay = await app.drawing(id("Parameter plane"));
  c.check(overlay?.length > 0, `period 2's overlay is drawn (${kinds(overlay)})`);

  // a tool's panel: refreshed on activation, by its checkbox; its button
  project = await app.openLibrary("Pullbacks and matings");
  const dyn = id("Dynamics");
  await app.activate(dyn);
  await app.pressTool("o", "Mark orbit of critical point");
  await page.waitForTimeout(500);
  const panel = await page.locator(".tool-panel").innerText();
  c.check(panel.includes("Draw lines between iterations") && panel.split("Mark orbit of critical point").length === 2,
          `its panel is shown, titled once (${JSON.stringify(panel.slice(0, 60))}…)`);
  const d0 = await app.drawing(dyn);
  c.check(d0?.length > 0 && !d0.some((s) => s.k === "line"), `activating it refreshes it (${kinds(d0)})`);
  await page.locator(".tool-panel input[type=checkbox]").check();
  await page.waitForTimeout(800);
  const d1 = await app.drawing(dyn);
  c.check(d1?.some((s) => s.k === "line"), `its checkbox refreshes it, with lines (${kinds(d1)})`);
  await shot("panel");
  await page.locator(".tool-panel button", { hasText: "Erase postcritical points" }).click();
  await page.waitForTimeout(500);
  c.check((await app.drawing(dyn))?.length === 0, "its button erases");
  // a tool setting its own fields
  await app.pressTool("i", "Preimages");
  const pre = page.locator(".tool-panel .field input[type=text]").first();
  const preBefore = await pre.inputValue();
  const box = await app.canvas(dyn).boundingBox();
  await page.mouse.click(box.x + box.width * 0.7, box.y + box.height * 0.3);
  await page.waitForTimeout(800);
  c.check((await pre.inputValue()) !== preBefore, `a click sets the tool's own field (${preBefore} -> ${await pre.inputValue()})`);

  // the sphere: an orbit drawn on the globe (curving runs)
  project = await app.openLibrary("Newton's method on the sphere");
  const globe = project.viewers[0].id;
  await app.activate(globe);
  await app.pressTool("t", "Trace");
  const gbox = await app.canvas(globe).boundingBox();
  await page.mouse.click(gbox.x + gbox.width * 0.6, gbox.y + gbox.height * 0.4);
  await page.waitForTimeout(800);
  const sd = await app.drawing(globe);
  c.check(sd?.some((s) => s.k === "runs"), `Trace on the sphere draws runs on the globe (${kinds(sd)})`);
  await shot("sphere", gbox);
}

async function refresh() {
  // Böttcher coordinates: the overlay's C marker follows C, though its
  // viewer doesn't re-render
  let project = await app.openLibrary("Böttcher coordinates");
  const id = (title) => project.viewers.find((v) => v.title === title).id;
  const pp = id("Parameter plane");
  const marker = async () => (await app.drawing(pp))?.find((s) => s.k === "circle")?.c;
  const m0 = await marker();
  const box = await app.canvas(pp).boundingBox();
  await page.mouse.click(box.x + box.width * 0.35, box.y + box.height * 0.3);
  await page.waitForTimeout(800);
  const m1 = await marker();
  c.check(m0 && m1 && (m0[0] !== m1[0] || m0[1] !== m1[1]), `the C marker moves with C (${JSON.stringify(m0)} -> ${JSON.stringify(m1)})`);

  // External rays: the dynamical plane's rays (drawn on refresh) follow C
  project = await app.openLibrary("External rays");
  const dp = id("Dynamical plane"), pp2 = id("Parameter plane");
  await app.activate(dp);
  const t = Date.now();
  await app.pressTool("r", "Dynamical plane external rays");
  await page.waitForFunction(([v]) => window.__messages.some((m) => m.type === "drawing" && m.viewer === v && m.layers.flat().length > 0),
                             [dp], { timeout: 30000 });
  c.check(Date.now() - t < 5000, `its rays are drawn on selecting it (in ${Date.now() - t} ms)`);
  const r0 = JSON.stringify(await app.drawing(dp));
  await app.activate(pp2);
  const pbox = await app.canvas(pp2).boundingBox();
  await page.mouse.click(pbox.x + pbox.width * 0.4, pbox.y + pbox.height * 0.45);
  await page.waitForTimeout(2000);
  const r1 = await app.drawing(dp);
  c.check(r1?.length > 0 && JSON.stringify(r1) !== r0, `and redrawn for a new C (${kinds(r1)})`);
  await shot("rays");
}

async function sweep() {
  const n = await page.locator("#library li").count();
  let count = 0, notCompiled = 0;
  for (let i = 0; i < n; i++) {
    const item = page.locator("#library li").nth(i);
    const title = (await item.textContent()).trim();
    await app.forgetMessages();
    await item.click();
    const project = await app.opened();
    for (const v of project.viewers) {
      for (const tool of v.tools) {
        count++;
        await page.evaluate(() => (window.__messages = window.__messages.filter((m) => m.type === "project")));
        await app.send({ type: "selectTool", viewer: v.id, name: tool.name });
        await page.waitForFunction(([viewer]) => window.__messages.some((m) =>
          (m.type === "tool" || m.type === "toolError") && m.viewer === viewer), [v.id], { timeout: 30000 }).catch(() => {});
        const err = (await app.messages("toolError")).find((m) => m.viewer === v.id);
        if (err) { notCompiled++; c.check(false, `${title} / ${v.title} / ${tool.name}: ${err.message}`); }
      }
    }
  }
  c.check(notCompiled === 0, `${count} tools of ${n} projects, ${notCompiled} not compiled`);
}

async function stop() {
  const project = await app.openFile(new URL("endless.fs", import.meta.url).pathname);
  const V = project.viewers.find((v) => v.title === "V").id, W = project.viewers.find((v) => v.title === "W").id;
  const bar = (id) => app.viewer(id).locator(".viewer-running");
  const vbox = await app.canvas(V).boundingBox();
  const count = page.locator(".panel .field input[type=text]").first();

  // Loop: the bar comes after a moment; Stop leaves the tool selected,
  // with no effect
  await app.activate(V);
  await app.pressTool("l", "Loop");
  const t0 = Date.now();
  await page.mouse.click(vbox.x + vbox.width / 2, vbox.y + vbox.height / 2);
  await bar(V).waitFor({ state: "visible", timeout: 5000 });
  const after = Date.now() - t0;
  c.check(after >= 200 && after < 2000, `a handler that doesn't finish shows the bar after a moment (${after} ms)`);
  await page.waitForTimeout(1200);
  const text = await bar(V).innerText();
  c.check(/Running the “Loop” tool/.test(text) && /[1-9] s/.test(text), `saying what's running, and for how long (${JSON.stringify(text)})`);
  await shot("running", { x: vbox.x, y: vbox.y + vbox.height - 80, width: vbox.width, height: 80 });
  const wbox = await app.canvas(W).boundingBox();
  const wf = await app.frames(W);
  await page.mouse.move(wbox.x + 50, wbox.y + 50);
  await page.mouse.down();
  await page.mouse.move(wbox.x + 90, wbox.y + 70, { steps: 5 });
  await page.mouse.up();
  await page.waitForTimeout(800);
  c.check((await app.frames(W)) > wf, "another viewer pans and renders meanwhile");
  await bar(V).locator("button", { hasText: /^Stop$/ }).click();
  await bar(V).waitFor({ state: "hidden", timeout: 5000 });
  await page.waitForTimeout(300);
  c.check((await count.inputValue()) !== "100", `Stop ends it, with no effect (count = ${await count.inputValue()})`);
  c.check((await app.pressed()) === "Loop", `and leaves the tool selected (${await app.pressed()})`);

  // Count: clicks work afterwards
  await app.pressTool("c", "Count");
  for (let i = 0; i < 6; i++) { await page.mouse.click(vbox.x + 20 + 10 * i, vbox.y + 30); await page.waitForTimeout(150); }
  await page.waitForTimeout(500);
  c.check((await count.inputValue()) === "6", `clicks work afterwards (count = ${await count.inputValue()})`);

  // Slow refresh: Stop and switch to Navigate
  await app.pressTool("s", "Slow refresh");
  await page.mouse.move(vbox.x + vbox.width / 2, vbox.y + vbox.height / 2);
  await page.mouse.wheel(0, -200);
  await bar(V).waitFor({ state: "visible", timeout: 5000 });
  c.check((await bar(V).locator("button").count()) === 2, "a slow refresh shows the bar, with both buttons");
  const tNav = Date.now();
  await bar(V).locator("button", { hasText: "Navigate" }).click();
  await bar(V).waitFor({ state: "hidden", timeout: 5000 });
  await page.waitForFunction(() => document.querySelector("#tools button[aria-pressed=true]")?.textContent === "Navigate",
                             null, { timeout: 5000 }).catch(() => {});
  c.check((await app.pressed()) === "Navigate", `Stop and switch to Navigate does (${await app.pressed()}, after ${Date.now() - tNav} ms)`);
  await page.mouse.wheel(0, 200);
  await page.waitForTimeout(800);
  c.check(await bar(V).isHidden(), "and it doesn't run again");

  // the overlay: Stop only; Stop turns it off
  c.check((await app.drawing(W))?.length === 1, "the overlay draws");
  const hang = page.locator(".panel input[type=checkbox]");
  await hang.check();
  await bar(W).waitFor({ state: "visible", timeout: 5000 });
  c.check((await bar(W).locator("button").count()) === 1 && /overlay/.test(await bar(W).innerText()),
          `an overlay that doesn't finish shows the bar, with Stop only (${JSON.stringify(await bar(W).innerText())})`);
  await bar(W).locator("button").click();
  await bar(W).waitFor({ state: "hidden", timeout: 5000 });
  await page.waitForTimeout(300);
  c.check((await app.drawing(W))?.length === 0, "Stop turns the overlay off, erasing its drawing");
  await hang.uncheck();
  await page.waitForTimeout(500);
  await hang.check();
  await page.waitForTimeout(800);
  c.check((await bar(W).isHidden()) && (await app.drawing(W))?.length === 0, "and it stays off");
}

async function panel() {
  await app.openLibrary("Mandelbrot and Julia sets together");
  const box = page.locator(".panel .field", { hasText: /^Selected value of C/ }).locator("input");
  c.check((await box.getAttribute("title")) === (await box.inputValue()), `a text box's tooltip is its value (${await box.getAttribute("title")})`);
  // (as a tool changes it)
  const pp = (await app.messages("project")).pop().viewers.find((v) => v.title === "Parameter plane").id;
  const pbox = await app.canvas(pp).boundingBox();
  await page.mouse.click(pbox.x + pbox.width * 0.3, pbox.y + pbox.height * 0.4);
  await page.waitForTimeout(800);
  c.check((await box.getAttribute("title")) === (await box.inputValue()), `and stays so when a tool sets it (${await box.getAttribute("title")})`);
  const aside = page.locator(".panel");
  const before = (await aside.boundingBox()).width;
  const grip = await page.locator(".panel-grip").boundingBox();
  await page.mouse.move(grip.x + grip.width / 2, grip.y + 100);
  await page.mouse.down();
  await page.mouse.move(grip.x + grip.width / 2 - 150, grip.y + 100, { steps: 5 });
  await page.mouse.up();
  const after = (await aside.boundingBox()).width;
  c.check(Math.abs(after - before - 150) <= 2, `dragging the panel's left edge widens it (${before} -> ${after})`);
  await page.reload();
  await page.waitForSelector("#library li", { timeout: 60000 });
  await app.openLibrary("Mandelbrot and Julia sets together");
  const again = (await page.locator(".panel").boundingBox()).width;
  c.check(Math.abs(again - after) <= 1, `and the width is remembered (${again})`);
}

async function editor() {
  const project = await app.openLibrary("Mandelbrot and Julia sets together");
  const id = (title) => project.viewers.find((v) => v.title === title).id;
  const pp = id("Parameter plane"), dp = id("Dynamical plane");
  await page.locator("#toggle-script").click();
  const doc = () => page.locator(".editor-window .cm-content").innerText();
  const status = () => page.locator(".editor-status").textContent();
  await page.waitForFunction(() => document.querySelectorAll(".editor-window [class*=tok-]").length > 20, null, { timeout: 10000 });
  c.check(true, `the script is colored (${await page.locator(".editor-window [class*=tok-]").count()} spans)`);
  c.check((await doc()).includes("z^2"), "and left as it is, until it's edited");

  // an error, marked, and undone
  await page.locator(".editor-window .cm-content").click();
  await page.keyboard.press("ControlOrMeta+End");
  // (after characters outside the BMP, two UTF-16 units each, as the
  // engine's offsets have to allow for)
  await page.keyboard.insertText("\n    z <- 2𝑖 + 𝑖 + (");
  await page.waitForFunction(() => document.querySelector(".editor-status.error") && document.querySelector(".cm-script-error"),
                             null, { timeout: 10000 }).catch(() => {});
  const marked = (await page.locator(".cm-script-error").allTextContents()).join("");
  c.check(marked === "(" && /⚠/.test(await status()), `an error is marked where it is (${JSON.stringify(marked)}), and said (${(await status()).trim()})`);
  for (let i = 0; i < 3; i++) await page.keyboard.press("ControlOrMeta+z");
  await page.waitForFunction(() => !document.querySelector(".editor-status.error"), null, { timeout: 10000 }).catch(() => {});
  c.check((await page.locator(".cm-script-error").count()) === 0, "undoing it clears it");

  // mathematical symbols, once it's edited (the cursor's line aside)
  await page.keyboard.press("ControlOrMeta+End");
  await page.keyboard.type("\n# notes: ");
  await page.waitForFunction(() => document.querySelector(".editor-window .cm-content").innerText.includes("z²"), null, { timeout: 10000 }).catch(() => {});
  const d = await doc();
  c.check(d.includes("z²") && d.includes("⟼") && d.includes("≔"), "once edited, it's written with symbols (z², ⟼, ≔)");
  // symbols typed by name: a space finishes one, Tab completes one, an accent waits
  await page.keyboard.type("\\theta ");
  await page.keyboard.type("\\alph");
  await page.keyboard.press("Tab");
  await page.keyboard.type(" \\hat x.");
  await page.waitForTimeout(300);
  const line = (await doc()).split("\n").pop();
  c.check(line === "# notes: θ α x̂.", `symbols are typed by name (${JSON.stringify(line)})`);

  // rebuilding keeps windows, views and tools
  // (the editor, moved down out of the viewers' way)
  const edTitle = await page.locator(".editor-window .window-title").boundingBox();
  await page.mouse.move(edTitle.x + 60, edTitle.y + edTitle.height / 2);
  await page.mouse.down();
  await page.mouse.move(edTitle.x + 60, edTitle.y + edTitle.height / 2 + 520, { steps: 5 });
  await page.mouse.up();
  const ppWin = page.locator(".window", { hasText: "Parameter plane" }).first();
  const title = await ppWin.locator(".window-title").boundingBox();
  await page.mouse.move(title.x + 40, title.y + title.height / 2);
  await page.mouse.down();
  await page.mouse.move(title.x + 40 + 30, title.y + title.height / 2 + 200, { steps: 4 });
  await page.mouse.up();
  const ppBefore = await ppWin.boundingBox();
  // (the editor's over the dynamical plane: its tool, chosen directly)
  await app.send({ type: "selectTool", viewer: dp, name: "Trace" });
  await app.waitForMessage("(m) => m.type === 'tool' && m.name === 'Trace'", 10000);
  const dpWin = page.locator(".window", { hasText: "Dynamical plane" }).first();
  await dpWin.locator(".window-title").dblclick();
  c.check(await dpWin.evaluate((w) => w.classList.contains("minimized")), "double-clicking a title bar minimizes its window");
  // (the configuration's label, renamed)
  const text = (await doc()).replace("Selected value of C:", "The value of C:");
  await page.locator(".editor-window .cm-content").click();
  await page.keyboard.press("ControlOrMeta+a");
  await page.keyboard.insertText(text);
  await app.forgetMessages();
  await page.keyboard.press("ControlOrMeta+Enter");
  await app.waitForMessage("(m) => m.type === 'project' && m.rebuilt", 30000);
  await app.opened();
  await page.waitForTimeout(500);
  c.check((await page.locator(".panel").innerText()).includes("The value of C:"), "Ctrl/Cmd-Enter rebuilds the project from the script");
  const ppAfter = await page.locator(".window", { hasText: "Parameter plane" }).first().boundingBox();
  c.check(Math.abs(ppAfter.x - ppBefore.x) < 2 && Math.abs(ppAfter.y - ppBefore.y) < 2, `a viewer's window stays where it was (${ppBefore.x},${ppBefore.y} -> ${ppAfter.x},${ppAfter.y})`);
  c.check(await page.locator(".window", { hasText: "Dynamical plane" }).first().evaluate((w) => w.classList.contains("minimized")), "and a minimized one stays minimized");
  const tool = (await app.messages("tool")).filter((m) => m.viewer === dp).pop();
  c.check(tool?.name === "Trace", `and a viewer keeps its tool (${tool?.name})`);
  c.check(await page.locator(".editor-window").count() === 1 && (await doc()).includes("The value of C:"), "and the editor stays open, with its text");
  c.check(/Rebuilt/.test(await status()), `saying so (${await status()})`);

  // a script that doesn't build leaves the project as it was
  await page.locator(".editor-window .cm-content").click();
  await page.keyboard.press("ControlOrMeta+End");
  await page.keyboard.type("\n@viewer");
  await app.forgetMessages();
  await page.locator(".editor-buttons button", { hasText: "Rebuild" }).click();
  await app.waitForMessage("(m) => m.type === 'rebuildError'", 30000).catch(() => {});
  c.check((await app.messages("rebuildError")).length === 1 && (await app.messages("project")).length === 0 && /⚠|error|expected/i.test(await status()),
          `one that doesn't build says why, and leaves the project (${await status()})`);
  // Revert: back to the project's script
  await page.locator(".editor-buttons button", { hasText: "Revert" }).click();
  await page.waitForTimeout(300);
  c.check(!(await doc()).includes("@viewer\n@viewer") && !(await doc()).trimEnd().endsWith("@viewer") && (await doc()).includes("The value of C:"),
          "Revert goes back to the project's script");
  await shot("editor");
}
