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
//   density   pixel_size, in a viewer's code, is a CSS pixel's size, on any
//             screen (dev-tools/web/pixel-size.fs; try FS_DPR=2)
//   opening   opening projects as the desktop app does: an old FractalStream
//             1.0 file, translated if asked (into a new project, with its
//             editor open); a script that doesn't build (dev-tools/web/
//             broken.fs), with its editor open, and its saved value and
//             window once it's fixed; a starting point, as a new project;
//             and an error, as a notice to dismiss
//   disk      a disk viewer (view=disk, dev-tools/web/disk.fs): Navigate's
//             drag moves the view by a hyperbolic translation (the point
//             under the pointer stays under it); its menu has no "Show on
//             the sphere", and shows it as the half-plane, keeping the view
//   pointer   "Show pointer position", from a viewer's menu: the point
//             under the pointer, on the plane and on the sphere
//   progressive  a slow viewer (dev-tools/web/slow.fs) rendered in pieces:
//             patches long before it's done; a pan drops the render going
//             on, and the next one's done; new parameters' first patch
//             is held back; parameters changed faster than renders go
//             each show something
//   menu      a viewer's menu: last, next, original view (and their keys),
//             jump to a view, the sphere and the plane (saved, and
//             reopened so), its colorizer and a colorizer's arguments,
//             subpixel smoothing, and a real viewer's aspect ratio (saved,
//             and reopened so)
//   autocolor the autocolor editor (from a viewer's Colorizer submenu), on
//             Newton's method for z⁵ - z - 1: its groups and points; a
//             point recolored (and the viewer drawn again), added, moved,
//             deleted; lock, tolerance (and a bad one), clear; saved
//   tables    saving what a project's tables hold, as the desktop does
//             (dev-tools/web/tables.fs): asked, with each table's size;
//             saved with them (and reopened, filled) or without them;
//             not asked when they're empty
//   capture   a viewer's Capture submenu: a picture (its size times the
//             multiplier, as a PNG), and a movie from a script (checked as
//             it's typed; recorded, encoded and saved as an MP4; stopped;
//             the values put back afterwards)
//   panel     the configuration panel: a text box's whole value as its
//             tooltip, as a tool changes it; widening the panel from its
//             left edge, remembered
//   path      the "Animate along path" tool (where a viewer's coordinate is
//             a configuration variable): a path dragged out, and a movie
//             along it of another viewer, with the first as an inset
//             marking the point; the values put back; and the viewer to
//             record, chosen in Capture > Movie… too
//   preferences  the Preferences dialog (from the welcome screen, and the
//             title's menu): the editor's line numbers, folds (stanzas and
//             handlers, and code blocks if asked), symbols, font and
//             colors, as they're changed; the sphere's shading (drawn
//             again) and new viewers' smoothing; kept in local storage,
//             by the desktop's settings' names, over a reload
//
// See harness.mjs for what it needs. The exit status is 1 if any check
// failed.
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { start, Checks, kinds } from "./harness.mjs";

const args = process.argv.slice(2);
let shots = null;
if (args[0] === "--screenshots") { shots = args[1]; args.splice(0, 2); fs.mkdirSync(shots, { recursive: true }); }
const all = { welcome, project, fit, links, drawing, tools, refresh, sweep, stop, editor, density, opening, pointer, disk, progressive, menu, autocolor, tables, capture, path: pathMovie, panel, preferences };
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
      c.check(false, `${w}: ${e.message.split("\n").slice(0, 6).join(" ")}`);
    }
    c.check(app.errors.length === 0, `no errors on the page${app.errors.length ? ": " + app.errors.join(" | ") : ""}`);
  }
} finally {
  await app.close();
}
console.log(c.failed ? `${c.failed} failed` : "all ok");
process.exit(c.failed ? 1 : 0);

// ---------------------------------------------------------------------

async function welcome() {
  const inProject = () => page.evaluate(() => document.body.classList.contains("in-project"));
  const sentClose = () => page.evaluate(() => window.__sent.filter((m) => m.type === "close").length);
  const dialog = page.locator(".page-dialog");
  // the library, as the welcome screen: the first section of projects to
  // explore, its first project chosen, and its details
  await app.backToLibrary();
  // which FractalStream it is, after its title: as git describes it
  const version = await page.locator(".welcome-version").textContent();
  const described = (await app.messages("library")).pop().version;
  c.check(version === described && /^(v[\w.-]+|[0-9a-f]{7,})(-dirty)?$/.test(version),
          `which FractalStream it is, after the title (${version})`);
  const current = page.locator(".welcome-sections button[aria-current=true]");
  c.check((await current.textContent()) === "Getting started" && !(await inProject()),
          `the library's the welcome screen, from "${await current.textContent()}"`);
  const details = page.locator(".welcome-details");
  const first = await page.locator(".welcome-tile").first().getAttribute("title");
  c.check((await details.locator("h2").textContent()) === first && await details.locator(".welcome-open").isVisible(),
          `its first project chosen, with its details (${first})`);
  c.check((await details.locator(".welcome-description p").count()) >= 1, "its description as paragraphs");
  // a click chooses, the arrow keys move, Return opens
  await app.forgetMessages();
  await page.locator(".welcome-tile").nth(1).click();
  await page.waitForTimeout(500);
  c.check((await details.locator("h2").textContent()) === await page.locator(".welcome-tile").nth(1).getAttribute("title")
          && (await app.messages("project")).length === 0, "a click chooses a project, and doesn't open it");
  await page.locator(".welcome-grid").focus();
  await page.keyboard.press("ArrowRight");
  const third = await page.locator(".welcome-tile").nth(2).getAttribute("title");
  c.check((await details.locator("h2").textContent()) === third, `the arrow keys move (${third})`);
  await page.keyboard.press("Enter");
  let project = await app.opened();
  c.check(project.title === third && await inProject() && !(await page.locator("#welcome").isVisible()),
          "Return opens it, in place of the library");

  // Close, with nothing changed: back to the library, unasked
  await app.projectMenu("Close project");
  await page.waitForTimeout(300);
  c.check(!(await inProject()) && !(await dialog.isVisible()) && (await sentClose()) === 1,
          "Close goes back to the library (nothing changed: nothing asked)");

  // a change: Close asks (Cancel stays); so does Back (Don't save closes)
  project = await app.openLibrary("Mandelbrot and Julia sets together");
  c.check(!(await page.locator(".project-title-dot").isVisible()), "opened, it has no changes to save");
  const box = page.locator(".panel .field", { hasText: /^Selected value of C/ }).locator("input");
  await box.fill("0.25 + 0.5i");
  await box.press("Enter");
  await app.waitForMessage(`(m) => m.type === "unsaved" && m.on`, 10000).catch(() => {});
  c.check((await page.locator(".project-title-dot").isVisible()), "a value changed: it has changes to save");
  await app.projectMenu("Close project");
  await dialog.waitFor({ timeout: 5000 }).catch(() => {});
  c.check(/Save the changes/.test(await dialog.textContent().catch(() => "")), "Close asks whether to save them");
  await dialog.locator("button", { hasText: "Cancel" }).click();
  c.check(await inProject(), "Cancel: it stays open");
  await page.goBack();
  await dialog.waitFor({ timeout: 5000 }).catch(() => {});
  c.check(await dialog.isVisible(), "the browser's Back asks too");
  await dialog.locator("button", { hasText: "Don't save" }).click();
  await page.waitForTimeout(300);
  c.check(!(await inProject()), "Don't save: back to the library");
  c.check(await page.evaluate(() => history.state === null), "(the project's step in the page's history is gone)");

  // a starting point, from New project; typed in its editor: that's a
  // change to save, and saving saves it as the script
  await page.locator(".welcome-header button", { hasText: "New project" }).click();
  c.check((await current.textContent()) === "Starting points", "New project goes to the starting points");
  const start = page.locator(".welcome-tile", { hasText: "Iterate a function" }).first();
  await app.forgetMessages();
  await start.click();
  await details.locator(".welcome-open").click();
  await app.waitForMessage(`(m) => m.type === "project"`, 30000);
  await page.waitForTimeout(500);
  c.check(!(await page.locator(".project-title-dot").isVisible()), "a starting point, unedited: nothing to save");
  await page.locator(".editor-overlay .cm-content").click();
  await page.keyboard.press("ControlOrMeta+End");
  await page.keyboard.type("\n# a note typed in");
  await page.waitForTimeout(200);
  await app.projectMenu("Close project");
  await dialog.waitFor({ timeout: 5000 }).catch(() => {});
  c.check(await dialog.isVisible(), "typed in its script: Close asks");
  // (saved as a download: not with the browser's file picker)
  await page.evaluate(() => { window.showSaveFilePicker = undefined; });
  const download = page.waitForEvent("download", { timeout: 10000 }).catch(() => null);
  await dialog.locator("button", { hasText: /^Save$/ }).click();
  const dl = await download;
  const saved = dl ? fs.readFileSync(await dl.path(), "utf8") : "";
  c.check(saved.includes("# a note typed in"), "Save saves what was typed, as the script");
  await page.waitForTimeout(500);
  c.check(!(await inProject()), "and then it closes");

  // narrow: the sections as chips, and a project's details a sheet from
  // the bottom, when it's tapped
  const size = page.viewportSize();
  await page.setViewportSize({ width: 390, height: 800 });
  await page.locator(".welcome-sections button", { hasText: "Getting started" }).click();
  const sheetShown = async () => {
    const box = await details.boundingBox();
    return (await details.isVisible()) && box !== null && box.y + box.height <= 801 && box.y < 800;
  };
  await page.waitForTimeout(300);
  c.check(!(await sheetShown()), "narrow: no details until a project's tapped");
  await page.locator(".welcome-tile").nth(1).click();
  await page.waitForTimeout(400);
  c.check(await sheetShown(), "tapped: its details slide up from the bottom");
  await page.locator(".welcome-sheet-close").click();
  await page.waitForTimeout(400);
  c.check(!(await sheetShown()), "and × puts them away");
  await page.setViewportSize(size);
  await shot("welcome");
}

async function project() {
  let project = await app.openLibrary("Mandelbrot and Julia sets together");
  c.check((await page.locator("#toolbar, #status").count()) === 0, "no toolbar, and no status line");
  const titleButton = page.locator(".project-title-button");
  c.check((await page.locator(".project-title-text").textContent()) === "Mandelbrot and Julia sets together",
          "the project's title floats in the corner");
  await titleButton.click();
  const items = (await page.locator(".project-title .window-menu .menu-label").allTextContents()).join(" | ");
  c.check(items === "Save project | Save project as… | Close project | Copy link | Edit project details… | Edit project script | Fit windows to the page | Preferences…",
          `its menu: ${items}`);
  await page.keyboard.press("Escape");

  // no window under the title: not to begin with, nor dragged there
  const titleBox = await page.locator(".project-title").boundingBox();
  const under = async () => (await page.locator(".window").evaluateAll((ws, t) => ws.filter((w) => {
    const r = w.getBoundingClientRect();
    return r.left < t.x + t.width && r.right > t.x && r.top < t.y + t.height && r.bottom > t.y;
  }).length, titleBox));
  c.check((await under()) === 0, "no window starts under the title");
  const bar = await page.locator(".window-title").first().boundingBox();
  await page.mouse.move(bar.x + 40, bar.y + bar.height / 2);
  await page.mouse.down();
  await page.mouse.move(titleBox.x + 30, titleBox.y + 5, { steps: 6 });
  await page.mouse.up();
  const top = (await page.locator(".window").first().boundingBox()).y;
  c.check((await under()) === 0 && top >= titleBox.y + titleBox.height,
          `a window dragged onto it stays below it (its top at ${Math.round(top)}, the title's bottom at ${Math.round(titleBox.y + titleBox.height)})`);

  // each viewer's tools, chosen in its title bar
  const pp = project.viewers.find((v) => v.title === "Parameter plane").id;
  const chooser = page.locator(".window").nth(pp).locator(".tool-chooser");
  c.check((await chooser.textContent()).startsWith("Select C"), `a viewer's tool chooser shows its tool (${await chooser.textContent()})`);
  await chooser.click();
  const tools = (await page.locator(".window").nth(pp).locator(".window-menu .menu-label").allTextContents()).join(" | ");
  await page.locator(".window").nth(pp).locator(".window-menu button", { hasText: "Navigate" }).click();
  await app.waitForMessage(`(m) => m.type === "tool" && m.viewer === ${pp} && m.name === null`, 10000).catch(() => {});
  c.check(tools === "Navigate | Select C | Animate along path" && (await chooser.textContent()).startsWith("Navigate"),
          `its menu has its tools (${tools}); choosing one selects it`);
  await app.activate(pp);
  await app.forgetMessages();
  await page.keyboard.press("s");
  await app.waitForMessage(`(m) => m.type === "tool" && m.viewer === ${pp} && m.name === "Select C"`, 10000).catch(() => {});
  c.check((await chooser.textContent()).startsWith("Select C"), "and its shortcut key does too");
  const fits = await page.locator(".tool-chooser-label").evaluateAll((ls) => ls.every((l) => l.scrollWidth <= l.clientWidth));
  c.check(fits, "the tools' names fit");

  // Edit project details: the title (kept through a rebuild), and the preview
  await app.projectMenu("Edit project details");
  const details = page.locator(".details-dialog");
  await details.waitFor({ timeout: 5000 }).catch(() => {});
  c.check((await details.locator("input").inputValue()) === "Mandelbrot and Julia sets together" &&
          (await details.locator(".details-preview").count()) === 1, "Edit project details shows its title and preview");
  await details.locator("input").fill("My own Julia sets");
  await details.locator("button", { hasText: "OK" }).click();
  await app.waitForMessage(`(m) => m.type === "unsaved" && m.on`, 5000).catch(() => {});
  c.check((await page.locator(".project-title-text").textContent()) === "My own Julia sets" &&
          await page.locator(".project-title-dot").isVisible(), "a new title: shown, and a change to save");
  await app.projectMenu("Edit project script");
  await page.locator(".editor-overlay .cm-content").click();
  await app.forgetMessages();
  await page.keyboard.press("ControlOrMeta+Enter");
  await app.opened().catch(() => null);
  c.check((await page.locator(".project-title-text").textContent()) === "My own Julia sets", "kept when it's rebuilt");
  await page.locator(".window").nth(pp).locator(".window-menu-button").click();
  await page.locator(".window-menu button", { hasText: "Use this as preview" }).click();
  await app.waitForMessage(`(m) => m.type === "details" && m.fromViewer`, 15000).catch(() => {});
  const preview = (await app.messages("details")).pop()?.preview ?? "";
  const said = (await page.locator(".notice.passing").allTextContents()).join(" | ");
  c.check(preview.length > 100 && /preview/.test(said), `"Use this as preview" makes one (${preview.length} characters), and says so`);
  await page.evaluate(() => { window.showSaveFilePicker = undefined; });
  const download = page.waitForEvent("download", { timeout: 10000 }).catch(() => null);
  await app.projectMenu("Save project as");
  const dl = await download;
  const saved = dl ? fs.readFileSync(await dl.path(), "utf8") : "";
  c.check(/title: My own Julia sets/.test(saved) && saved.includes(preview.slice(0, 60)),
          "saved, the file has the new title and preview");
  c.check(!(await page.locator(".project-title-dot").isVisible()), "and no changes to save");
  await page.waitForTimeout(4500);
  c.check((await page.locator(".notice.passing").count()) === 0, "what's said goes after a few seconds");

  // a tool with a panel, chosen: the drawer opens to show it
  project = await app.openLibrary("Pullbacks and matings");
  c.check((await page.locator(".panel.open").count()) === 0, "(the drawer's shut)");
  await app.activate(project.viewers.find((v) => v.title === "Dynamics").id);
  await app.chooseTool("Mark orbit of critical point");
  await page.waitForTimeout(400);
  c.check((await page.locator(".panel.open").count()) === 1, "choosing a tool with a panel opens the drawer");
  await shot("project");
}

async function fit() {
  const size = page.viewportSize();
  const boxes = () => page.locator(".window").evaluateAll((ws) => ws.map((w) => {
    const r = w.getBoundingClientRect();
    return { x: r.x, y: r.y, w: r.width, h: r.height };
  }));
  const overlap = (bs) => bs.some((a, i) => bs.some((b, j) => i < j && a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h));
  const sideways = () => page.locator(".desk").evaluate((d) => d.scrollWidth > d.clientWidth + 1);
  // (the regions the viewers show, as a saved file has them: each one's width)
  const regions = (text) => [...text.slice(text.indexOf("web-properties:")).matchAll(/region:\n\s+- (\S+)/g)].map((m) => Number(m[1]));
  const near = (a, b, tol = 0.03) => a.length === b.length && a.length > 0 && a.every((x, i) => Math.abs(x / b[i] - 1) < tol);
  const saveAs = async () => {
    await page.evaluate(() => { window.showSaveFilePicker = undefined; });
    const download = page.waitForEvent("download", { timeout: 10000 });
    await app.projectMenu("Save project as");
    return fs.readFileSync(await (await download).path(), "utf8");
  };
  // (a viewer's saved place, from a saved file's web-properties)
  const placeOf = (text, title) => {
    const section = text.slice(text.indexOf(`    ${title}:`));
    const m = /place:\n((?:\s+- \S+\n){4})/.exec(section);
    return m ? m[1].trim().split("\n").map((l) => Number(l.replace(/^\s*- /, ""))) : null;
  };

  try {
    // a phone: the viewers tiled down it, each as wide as it, both in view
    await page.setViewportSize({ width: 390, height: 844 });
    let project = await app.openLibrary("Mandelbrot and Julia sets together");
    let b = await boxes();
    c.check(b.length === 2 && b.every((r) => r.w > 340 && r.x >= 0 && r.x + r.w <= 390) && !(await sideways()),
            `on a phone, the viewers are as wide as it, and the page doesn't scroll sideways (${b.map((r) => Math.round(r.w)).join(", ")} px)`);
    c.check(!overlap(b) && b.every((r) => r.y + r.h <= 844 + 1), "tiled down it, both in view");
    // turned: side by side
    await page.setViewportSize({ width: 844, height: 390 });
    await page.waitForTimeout(800);
    b = await boxes();
    c.check(!overlap(b) && b.every((r) => r.x + r.w <= 844 && r.y + r.h <= 390 + 1) && b[0].y === b[1].y,
            "turned sideways, they're side by side, on the page");
    // a smaller phone, sideways: tiled across it, on it; and so when
    // they're fitted to it
    await page.setViewportSize({ width: 667, height: 375 });
    await page.waitForTimeout(800);
    b = await boxes();
    c.check(!overlap(b) && b.every((r) => r.x >= 0 && r.x + r.w <= 667 && r.y + r.h <= 375 + 1) && b[0].y === b[1].y && !(await sideways()),
            `a smaller phone, sideways: side by side, both on it (${b.map((r) => `${Math.round(r.w)}×${Math.round(r.h)}`).join(", ")})`);
    await app.projectMenu("Fit windows to the page");
    await page.waitForTimeout(500);
    b = await boxes();
    c.check(!overlap(b) && b.every((r) => r.x + r.w <= 667 && r.y + r.h <= 375 + 1) && b[0].y === b[1].y,
            "and \"Fit windows to the page\" leaves them so");
    project = await app.openLibrary("Period 2");
    b = await boxes();
    c.check(b.length === 3 && !overlap(b) && b.every((r) => r.x + r.w <= 667 && r.y + r.h <= 375 + 1) && new Set(b.map((r) => r.y)).size === 1,
            `three viewers, sideways: in a row, all on it (${b.map((r) => `${Math.round(r.w)}×${Math.round(r.h)}`).join(", ")})`);
    await page.setViewportSize({ width: 375, height: 667 });
    await page.waitForTimeout(800);
    b = await boxes();
    c.check(!overlap(b) && b.every((r) => r.x + r.w <= 375) && !(await sideways()),
            `and upright: down the page, none off its side (${b.map((r) => `${Math.round(r.w)}×${Math.round(r.h)}`).join(", ")})`);

    // a wide screen: saved where they are, as fractions; reopened on a
    // smaller one, in the same places, each showing what it did
    await page.setViewportSize({ width: 1600, height: 1000 });
    project = await app.openLibrary("Mandelbrot and Julia sets together");
    await page.waitForTimeout(500);
    const big = await saveAs();
    const before = regions(big);
    c.check(/place:/.test(big) && /region:/.test(big) && !/pixel-size:/.test(big), "saved: each window's place, and the region it shows");
    const copy = path.join(os.tmpdir(), "fit-saved.fs");
    fs.writeFileSync(copy, big);
    const bigBoxes = await boxes();
    await page.setViewportSize({ width: 1000, height: 640 });
    project = await app.openFile(copy);
    await page.waitForTimeout(800);
    b = await boxes();
    const ratio = b[1].x / bigBoxes[1].x;
    c.check(Math.abs(ratio - 1000 / 1600) < 0.08 && b.every((r) => r.x + r.w <= 1000 && r.y + r.h <= 641) && !overlap(b),
            `reopened on a smaller screen, in the same places, smaller (${ratio.toFixed(2)} as far across), and on it`);
    const after = regions(await saveAs());
    c.check(near(after, before), `each showing the same region (${before.join(", ")} -> ${after.join(", ")} wide)`);

    // made smaller than they fit (but not a phone's): shrunk to fit
    await page.setViewportSize({ width: 760, height: 500 });
    await page.waitForTimeout(800);
    b = await boxes();
    c.check(b.every((r) => r.x + r.w <= 760 && r.y + r.h <= 501) && !overlap(b) && !(await sideways()),
            "the page made smaller: they're shrunk to fit it");
    const shrunk = regions(await saveAs());
    c.check(near(shrunk, before), `still showing the same regions (${shrunk.join(", ")})`);
    // Fit windows to the page: side by side, as big as they fit
    await page.setViewportSize({ width: 1600, height: 1000 });
    await page.waitForTimeout(500);
    await app.projectMenu("Fit windows to the page");
    await page.waitForTimeout(500);
    b = await boxes();
    c.check(b[0].y === b[1].y && b[0].w > 400 && !overlap(b), `"Fit windows to the page" puts them side by side (${Math.round(b[0].w)} px wide)`);

    // saved on a phone: where they go on a wide screen is kept
    project = await app.openFile(copy);
    await page.setViewportSize({ width: 390, height: 844 });
    await page.waitForTimeout(800);
    const fromPhone = await saveAs();
    c.check(JSON.stringify(placeOf(fromPhone, "Dynamical plane")) === JSON.stringify(placeOf(big, "Dynamical plane")),
            `saved on a phone, the wide screen's layout is kept (${placeOf(fromPhone, "Dynamical plane")})`);
  } finally {
    await page.setViewportSize(size);
  }
}

async function links() {
  const copied = async () => {
    await page.evaluate(() => {
      window.__link = null;
      Object.defineProperty(navigator, "clipboard", { configurable: true, value: { writeText: async (t) => { window.__link = t; } } });
    });
    await app.projectMenu("Copy link");
    await page.waitForFunction(() => window.__link, null, { timeout: 10000 }).catch(() => {});
    return page.evaluate(() => window.__link);
  };
  // (a page loaded afresh, at a link)
  const visit = async (link) => {
    await page.goto("about:blank");
    await page.goto(link);
  };
  const field = () => page.locator(".panel .field", { hasText: /^Selected value of C/ }).locator("input");

  // a library project: a short link, with its values and views
  await app.openLibrary("Mandelbrot and Julia sets together");
  await field().fill("0.25 + 0.5i");
  await field().press("Enter");
  await page.waitForTimeout(300);
  const short = await copied();
  c.check(/#open=getting-started\/mandelbrot-and-julia&s=/.test(short ?? "") && short.length < 1500,
          `Copy link: a library project's is short (${short?.length} characters: ${short?.slice(short.indexOf("#"), short.indexOf("#") + 50)}…)`);
  c.check(/Link copied/.test((await page.locator(".notice.passing").allTextContents()).join()), "and says it's copied");
  await visit(short);
  let project = await app.opened();
  c.check(project.title === "Mandelbrot and Julia sets together" && project.library && !project.new,
          "opened, it's the library project");
  c.check((await field().inputValue()).replace(/\s/g, "") === "0.25+0.5i", `with its values (C = ${await field().inputValue()})`);
  c.check(await page.evaluate(() => location.hash === "" && document.body.classList.contains("in-project")),
          "and the link's taken out of the address bar");
  await page.reload();
  await app.waitForLibrary();
  c.check(!(await page.evaluate(() => document.body.classList.contains("in-project"))), "(so reloading goes to the library)");

  // edited: the whole project file, with what was typed, less its preview
  await app.openLibrary("Mandelbrot and Julia sets together");
  await app.projectMenu("Edit project script");
  await page.locator(".editor-overlay .cm-content").click();
  await page.keyboard.press("ControlOrMeta+End");
  await page.keyboard.type("\n# shared with a note");
  await page.waitForTimeout(200);
  const long = await copied();
  c.check(/#project=.*&name=mandelbrot-and-julia\.fs$/.test(long ?? ""), `edited, it's the whole project (${long?.length} characters)`);
  await visit(long);
  project = await app.opened();
  c.check(project.script.includes("# shared with a note") && !project.library, "opened, with what was typed, as a project of its own");

  // a project file on the web, by its address
  const fixed = fs.readFileSync(new URL("pixel-size.fs", import.meta.url).pathname);
  const dataUrl = `data:text/plain;base64,${fixed.toString("base64")}`;
  await visit(`${page.url().split("#")[0]}#url=${encodeURIComponent(dataUrl)}`);
  project = await app.opened().catch(() => null);
  c.check(project !== null && project.viewers.length > 0, `#url= opens a project file from its address (${project?.fileName})`);

  // a link put in the address bar, over a project with changes: asked first
  await app.openLibrary("Mandelbrot and Julia sets together");
  await field().fill("0.1");
  await field().press("Enter");
  await app.waitForMessage(`(m) => m.type === "unsaved" && m.on`, 5000).catch(() => {});
  await app.forgetMessages();
  await page.evaluate((h) => { location.hash = h; }, short.slice(short.indexOf("#")));
  const dialog = page.locator(".page-dialog");
  await dialog.waitFor({ timeout: 5000 }).catch(() => {});
  c.check(/Save the changes/.test(await dialog.textContent().catch(() => "")), "a link put in the address bar asks about unsaved changes first");
  await dialog.locator("button", { hasText: "Don't save" }).click();
  project = await app.opened();
  c.check((await field().inputValue()).replace(/\s/g, "") === "0.25+0.5i", "then opens it");

  // links that can't be opened: said so
  for (const [hash, what] of [["#project=not-really-a-project", "a garbled link"],
                              ["#open=no-such/project&s=", "a library project that isn't there"]]) {
    await page.evaluate(() => { for (const n of document.querySelectorAll(".notice")) n.remove(); });
    await visit(`${page.url().split("#")[0]}${hash}`);
    await app.waitForLibrary();
    const err = page.locator(".notice.error");
    await err.first().waitFor({ timeout: 10000 }).catch(() => {});
    c.check(await err.first().isVisible(), `${what}: an error (${(await err.first().textContent().catch(() => "")).slice(0, 70)}…)`);
  }
  // (those errors were on purpose)
  app.clearErrors();
}

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
  // (its radius is 4 pixel_size: 4 CSS pixels, on any screen, as on the
  // desktop, whatever the canvas's density)
  const frame = (await app.messages("patch")).filter((m) => m.viewer === pp).pop();
  const r = (await app.drawing(pp)).find((s) => s.k === "circle").r / frame.px;
  const dpr = await page.evaluate(() => Math.min(2, Math.max(1, Math.round(window.devicePixelRatio || 1))));
  c.check(Math.abs(r - 4 * dpr) < 1e-6, `pixel_size is a CSS pixel's size: the marker's radius is ${r.toFixed(2)} canvas pixels, at ${dpr}x`);

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
  let count = 0, notCompiled = 0;
  for (const { section, entry, title } of app.libraryProjects()) {
    const project = await app.openLibraryEntry(section, entry);
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
  c.check(notCompiled === 0, `${count} tools of ${app.libraryProjects().length} projects, ${notCompiled} not compiled`);
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
  await page.waitForFunction(() => [...document.querySelectorAll(".tool-chooser-label")].some((l) => l.textContent === "Navigate"),
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
  // the configuration's a drawer: shut to begin with; its tab pulls it
  // out (and pushes it back), with a click or a drag
  const aside = page.locator(".panel");
  const out = async () => {
    const b = await aside.boundingBox();
    return b ? Math.round(page.viewportSize().width - b.x) : 0;
  };
  c.check((await out()) <= 1, `the configuration's a drawer, shut to begin with (${await out()} px out)`);
  await page.locator(".drawer-tab").click();
  await page.waitForTimeout(400);
  const width = (await aside.boundingBox()).width;
  c.check(Math.abs((await out()) - width) <= 1, `its tab pulls it out (${await out()} of ${width} px)`);
  await page.locator(".drawer-tab").click();
  await page.waitForTimeout(400);
  c.check((await out()) <= 1, "and pushes it back");
  const tab = await page.locator(".drawer-tab").boundingBox();
  await page.mouse.move(tab.x + tab.width / 2, tab.y + tab.height / 2);
  await page.mouse.down();
  await page.mouse.move(tab.x - width * 0.25, tab.y + tab.height / 2, { steps: 5 });
  const partly = await out();
  await page.mouse.move(tab.x - width * 0.6, tab.y + tab.height / 2, { steps: 5 });
  await page.mouse.up();
  await page.waitForTimeout(400);
  c.check(partly > 0.2 * width && partly < 0.35 * width && Math.abs((await out()) - width) <= 1,
          `dragging its tab pulls it out as far as it's dragged (${partly} px), and it stays out (${await out()} px)`);
  const before = (await aside.boundingBox()).width;
  const grip = await page.locator(".panel-grip").boundingBox();
  // (by its middle: the panel's short)
  await page.mouse.move(grip.x + grip.width / 2, grip.y + grip.height / 2);
  await page.mouse.down();
  await page.mouse.move(grip.x + grip.width / 2 - 150, grip.y + grip.height / 2, { steps: 5 });
  await page.mouse.up();
  const after = (await aside.boundingBox()).width;
  c.check(Math.abs(after - before - 150) <= 2, `dragging the panel's left edge widens it (${before} -> ${after})`);
  await page.reload();
  await app.waitForLibrary();
  await app.openLibrary("Mandelbrot and Julia sets together");
  const again = (await page.locator(".panel").boundingBox()).width;
  c.check(Math.abs(again - after) <= 1, `and the width is remembered (${again})`);
}

async function editor() {
  const project = await app.openLibrary("Mandelbrot and Julia sets together");
  const id = (title) => project.viewers.find((v) => v.title === title).id;
  const pp = id("Parameter plane"), dp = id("Dynamical plane");
  await app.projectMenu("Edit project script");
  const doc = () => page.locator(".editor-overlay .cm-content").innerText();
  const status = () => page.locator(".editor-status").textContent();
  await page.waitForFunction(() => document.querySelectorAll(".editor-overlay [class*=tok-]").length > 20, null, { timeout: 10000 });
  c.check(true, `the script is colored (${await page.locator(".editor-overlay [class*=tok-]").count()} spans)`);
  c.check((await doc()).includes("z^2"), "and left as it is, until it's edited");

  // an error, marked, and undone
  await page.locator(".editor-overlay .cm-content").click();
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
  await page.waitForFunction(() => document.querySelector(".editor-overlay .cm-content").innerText.includes("z²"), null, { timeout: 10000 }).catch(() => {});
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
  // (with the editor closed: it's over the viewers)
  await page.locator(".editor-close").click();
  c.check(await page.locator(".editor-overlay").count() === 0, "Close closes the editor");
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
  // (the configuration's label, renamed; the editor reopened, as it was)
  await app.projectMenu("Edit project script");
  c.check((await doc()).includes("# notes: θ α x̂."), "the Script button reopens it, with its text");
  const text = (await doc()).replace("Selected value of C:", "The value of C:");
  await page.locator(".editor-overlay .cm-content").click();
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
  c.check(await page.locator(".editor-overlay").count() === 0, "and, since it worked, closes the editor");
  const saidRebuilt = (await page.locator(".notice.passing").allTextContents()).join(" | ");
  c.check(/Rebuilt/.test(saidRebuilt), `saying so (${saidRebuilt})`);
  await app.projectMenu("Edit project script");
  c.check((await doc()).includes("The value of C:"), "which, reopened, has the rebuilt script");
  // (and is over the viewers: what's at its middle is the editor)
  const onTop = await page.evaluate(() => {
    const r = document.querySelector(".editor-overlay").getBoundingClientRect();
    return !!document.elementFromPoint(r.x + r.width / 2, r.y + r.height / 2)?.closest(".editor-overlay");
  });
  c.check(onTop, "over everything in the workspace");

  // a script that doesn't build leaves the project as it was
  await page.locator(".editor-overlay .cm-content").click();
  await page.keyboard.press("ControlOrMeta+End");
  await page.keyboard.type("\n@viewer");
  await app.forgetMessages();
  await page.locator(".editor-buttons button", { hasText: "Rebuild" }).click();
  await app.waitForMessage("(m) => m.type === 'rebuildError'", 30000).catch(() => {});
  c.check((await app.messages("rebuildError")).length === 1 && (await app.messages("project")).length === 0 && /⚠|error|expected/i.test(await status()),
          `one that doesn't build says why, and leaves the project (${await status()})`);
  c.check(await page.locator(".editor-overlay").count() === 1, "with the editor still open");
  // Revert: back to the project's script
  await page.locator(".editor-buttons button", { hasText: "Revert" }).click();
  await page.waitForTimeout(300);
  c.check(!(await doc()).includes("@viewer\n@viewer") && !(await doc()).trimEnd().endsWith("@viewer") && (await doc()).includes("The value of C:"),
          "Revert goes back to the project's script");
  await shot("editor");

  // An analysis that comes back after more's been typed (above what it
  // would rewrite) isn't used: its offsets would garble the script. (The
  // engine's analyses, made 100 ms late here, as a long script's are.)
  await page.addInitScript(() => {
    const add = Worker.prototype.addEventListener;
    Worker.prototype.addEventListener = function (type, f, o) {
      if (type !== "message") return add.call(this, type, f, o);
      return add.call(this, type, (e) => e.data?.type === "analysis" ? setTimeout(() => f(e), 100) : f(e), o);
    };
  });
  await page.reload();
  await app.waitForLibrary();
  // (the starting point: it opens with its editor)
  await app.openLibrary("Newton's method");
  if (!(await editorShown())) await app.projectMenu("Edit project script");
  await page.waitForTimeout(1000);
  await page.locator(".editor-overlay .cm-content").click();
  await page.keyboard.press("ControlOrMeta+Home");
  await page.keyboard.press("Enter");
  await page.keyboard.press("ArrowUp");
  await page.keyboard.type("# typed slowly", { delay: 180 });
  await page.waitForTimeout(1500);
  const lines = (await doc()).split("\n").filter((l) => /newton\(z\)|lastZ/.test(l));
  c.check(lines.length > 0 && lines.every((l) => !/[≔⟵⟼′][=\-'>]|[<:'-][≔⟵⟼′]/.test(l)) && lines.some((l) => l.includes("≔")),
          `typing while an analysis is out doesn't garble the script (${JSON.stringify(lines.slice(0, 3))})`);
}

async function density() {
  await app.openFile(new URL("pixel-size.fs", import.meta.url).pathname);
  await page.waitForTimeout(500);
  // the band's width in the canvas's pixels, along its middle row
  const r = await page.evaluate(() => {
    const canvas = document.querySelector(".viewer canvas");
    const ctx = canvas.getContext("2d");
    const row = ctx.getImageData(0, Math.floor(canvas.height / 2), canvas.width, 1).data;
    let dark = 0;
    for (let i = 0; i < row.length; i += 4) if (row[i] < 128) dark++;
    return { dark, ratio: canvas.width / canvas.getBoundingClientRect().width };
  });
  c.check(Math.abs(r.dark / r.ratio - 4) <= 1,
          `a band 4 pixel_size wide is 4 CSS pixels wide (${r.dark} canvas pixels, at ${r.ratio}x)`);
}

/** Drop a file's bytes on the page (any bytes: not only a project's text). */
async function dropBytes(file, name = path.basename(file)) {
  await app.backToLibrary();
  await app.forgetMessages();
  await page.evaluate(([b, name]) => {
    const dt = new DataTransfer();
    dt.items.add(new File([new Uint8Array(b)], name));
    document.dispatchEvent(new DragEvent("drop", { dataTransfer: dt, bubbles: true, cancelable: true }));
  }, [[...fs.readFileSync(file)], name]);
}

function editorShown() { return page.locator(".editor-overlay").isVisible(); }

async function opening() {
  // an old FractalStream 1.0 file: a notice asks first
  const legacy = new URL("../../fractalstream-core/test/legacy-binary/Sarah3.fs", import.meta.url).pathname;
  await dropBytes(legacy);
  const ask = page.locator(".notice", { hasText: "old version of FractalStream" });
  await ask.waitFor({ timeout: 10000 }).catch(() => {});
  c.check(await ask.isVisible(), "an old FractalStream file: a notice asks whether to translate it");
  await ask.locator("button", { hasText: "Translate" }).click();
  const translated = await app.opened().catch(() => null);
  c.check(translated !== null && translated.new && translated.edit && translated.viewers.length > 0,
          `translated, it opens as a new project (${translated?.fileName}), with ${translated?.viewers.length} viewer(s)`);
  c.check(await editorShown(), "with its script editor open");
  c.check(/Sarah3\.fs/.test(await page.locator(".project-title-button").getAttribute("title")) && translated?.notes !== undefined,
          `and the old file's name (${translated?.notes.length} translation note(s))`);
  await page.locator(".editor-close").click();

  // a script that doesn't build: no viewers, its editor open; fixed, its
  // saved value and window
  await dropBytes(new URL("broken.fs", import.meta.url).pathname);
  await app.waitForMessage(`(m) => m.type === "project"`, 30000);
  const broken = (await app.messages("project")).pop();
  c.check(broken.viewers.length === 0 && broken.edit, "a script that doesn't build opens with no viewers, for its editor");
  c.check(await editorShown(), "with the editor open");
  await page.waitForFunction(() => document.querySelector(".cm-script-error"), null, { timeout: 10000 }).catch(() => {});
  c.check((await page.locator(".cm-script-error").count()) > 0, "and its error marked");
  // (fix it: the stray ` (` ending its last line)
  await page.locator(".editor-overlay .cm-content").click();
  await page.keyboard.press("ControlOrMeta+End");
  for (let i = 0; i < 2; i++) await page.keyboard.press("Backspace");
  await page.waitForFunction(() => !document.querySelector(".editor-status.error"), null, { timeout: 10000 }).catch(() => {});
  await app.forgetMessages();
  await page.locator(".editor-buttons button", { hasText: "Rebuild" }).click();
  const fixed = await app.opened().catch(() => null);
  c.check(fixed?.viewers.length === 1 && !(await editorShown()), "fixed and rebuilt, its viewer comes, and the editor closes");
  const field = page.locator(".panel input[type=text]").first();
  c.check((await field.inputValue().catch(() => "")) === "0.25", `with its saved value (shade = ${await field.inputValue().catch(() => "?")})`);
  const win = page.locator(".window").first();
  const place = await win.evaluate((w) => [w.offsetLeft, w.offsetTop, w.querySelector(".window-body").clientWidth]);
  c.check(place[0] === 300 && place[1] === 40 && place[2] === 220, `and its saved window (${place})`);

  // a starting point: a new, untitled project, its editor open
  const start = await app.openLibrary("Iterate a family of functions");
  c.check(start.title === "Untitled" && start.fileName === "Untitled.fs" && start.new && !start.library,
          `a starting point opens as a new project (${start.title}, ${start.fileName})`);
  c.check(await editorShown(), "with its script editor open");
  await page.locator(".editor-close").click();
  const plain = await app.openLibrary("Mandelbrot and Julia sets together");
  c.check(!plain.edit && !(await editorShown()), "(another library project opens without it)");

  // an error: a notice, dismissed
  fs.writeFileSync(path.join(os.tmpdir(), "not-a-project.fs"), "this: [is not a project");
  await dropBytes(path.join(os.tmpdir(), "not-a-project.fs"));
  const err = page.locator(".notice.error");
  await err.first().waitFor({ timeout: 10000 }).catch(() => {});
  c.check(await err.first().isVisible(), `a file that isn't a project: an error notice (${(await err.first().textContent().catch(() => "")).slice(0, 60)}…)`);
  await err.first().locator(".notice-dismiss").click();
  c.check((await err.count()) === 0, "dismissed, it's gone");
  // (that error was on purpose)
  app.clearErrors();
}

async function pointer() {
  const readout = (id) => page.locator(".viewer-pointer").nth(id);
  const showFor = async (id) => {
    await page.locator(".window").nth(id).locator(".window-menu-button").click();
    await page.locator(".window-menu button", { hasText: "Show pointer position" }).click();
  };
  const hover = async (id, fx, fy) => {
    const box = await app.canvas(id).boundingBox();
    await page.mouse.move(box.x + box.width * fx, box.y + box.height * fy, { steps: 2 });
    await page.waitForTimeout(300);
    return (await readout(id).textContent()).trim();
  };
  // (the plane: pixel-size.fs, centered on 0)
  await app.openFile(new URL("pixel-size.fs", import.meta.url).pathname);
  c.check(!(await readout(0).isVisible()), "the pointer's position isn't shown, at first");
  await showFor(0);
  c.check(await readout(0).isVisible(), "chosen from the viewer's menu, it is");
  const t = await hover(0, 0.75, 0.25);
  const m = /^(-?\d+\.\d{14}) ([+-]) (\d+\.\d{14})𝑖$/.exec(t);
  c.check(m && Number(m[1]) > 0 && m[2] === "+" && Number(m[3]) > 0, `up and to the right of 0, it's there (${t})`);
  await page.locator(".window").nth(0).locator(".window-menu-button").click();
  const item = page.locator(".window-menu button", { hasText: "Show pointer position" });
  const checked = await item.getAttribute("aria-checked");
  await item.click();
  c.check(checked === "true" && !(await readout(0).isVisible()), "the menu shows it's on, and turns it off");

  // the sphere: the globe's center is the view's, and off the globe, "…"
  const project = await app.openLibrary("Newton's method on the sphere");
  const s = project.viewers.findIndex((v) => v.sphere);
  await showFor(s);
  const centre = await hover(s, 0.5, 0.5);
  const off = await hover(s, 0.01, 0.01);
  c.check(/𝑖$/.test(centre) && off === "…", `on the sphere: ${centre} at the globe's center, and "…" off it`);
}

async function disk() {
  await app.openFile(new URL("disk.fs", import.meta.url).pathname);
  const box = await app.canvas(0).boundingBox();
  // (a CSS pixel is 1/250 of the disk's radius: see disk.fs)
  const radius = 250, cx = box.x + box.width / 2, cy = box.y + box.height / 2;
  const pixelAt = (fx, fy) => page.evaluate(([fx, fy]) => {
    const c = document.querySelectorAll(".viewer canvas")[0];
    const d = c.getContext("2d").getImageData(Math.round(fx * c.width), Math.round(fy * c.height), 1, 1).data;
    return [d[0], d[1], d[2]];
  }, [fx, fy]);
  const isRed = ([r, g, b]) => r > 200 && g < 60 && b < 60;
  c.check(isRed(await pixelAt(0.5, 0.5)), "the origin is in the middle of the disk, at first");
  // drag from the middle 0.3 of the disk's radius to the right
  const frames = await app.frames(0);
  await page.mouse.move(cx, cy);
  await page.mouse.down();
  for (let i = 1; i <= 15; i++) await page.mouse.move(cx + 0.3 * radius * i / 15, cy);
  await page.mouse.up();
  await page.waitForFunction((n) => Number(document.querySelector(".viewer").dataset.frames) > n, frames, { timeout: 60000 });
  await page.waitForTimeout(1000);
  const sent = await page.evaluate(() => window.__sent.map((m) => m.type));
  c.check(sent.includes("diskDrag") && !sent.includes("rotate"), "the drag moves the view of the hyperbolic plane");
  await app.send({ type: "where", viewer: 0 });
  await page.waitForTimeout(500);
  const where = (await app.messages("where")).pop();
  c.check(where && Math.abs(where.x + 0.3) < 1e-6 && Math.abs(where.y) < 1e-6,
          `the point that was 0.3 to the left is in the middle now (${where?.x}, ${where?.y})`);
  c.check(isRed(await pixelAt(0.5 + 0.3 * radius / box.width, 0.5)) && !isRed(await pixelAt(0.5, 0.5)),
          "the origin is where the pointer let go of it");
  await page.locator(".window").nth(0).locator(".window-menu-button").click();
  c.check(await page.locator(".window-menu button", { hasText: "Show on the sphere" }).count() === 0,
          "its menu doesn't offer the sphere");
  // the half-plane, from the menu: the same view (the same point in the
  // middle: i, on the half-plane), drawn again
  const framesBefore = await app.frames(0);
  await page.locator(".window-menu button", { hasText: "Show as the half-plane" }).click();
  await page.waitForFunction((n) => Number(document.querySelector(".viewer").dataset.frames) > n, framesBefore, { timeout: 60000 });
  await app.send({ type: "where", viewer: 0 });
  await page.waitForTimeout(500);
  const there = (await app.messages("where")).pop();
  c.check(there && Math.abs(there.x + 0.3) < 1e-6 && Math.abs(there.y) < 1e-6,
          `shown as the half-plane, the same point's in the middle (${there?.x}, ${there?.y})`);
  await page.locator(".window").nth(0).locator(".window-menu-button").click();
  const toggle = page.locator(".window-menu button", { hasText: "Show as the half-plane" });
  c.check(await toggle.getAttribute("aria-checked") === "true", "its menu shows it's the half-plane");
  await page.keyboard.press("Escape");
}

async function progressive() {
  await app.forgetMessages();
  const opened = Date.now();
  await app.openFile(new URL("slow.fs", import.meta.url).pathname);
  const took = Date.now() - opened;
  // (the first render to be done: at a high density, the first one, at
  // the project's size, is dropped for one at the canvas's)
  const all0 = await app.messages("patch");
  const doneId = all0.find((m) => m.done).id;
  const patches = all0.filter((m) => m.id === doneId);
  c.check(patches.length > 2 && patches.findIndex((m) => m.done) === patches.length - 1,
          `render ${doneId} came in ${patches.length} patches, the last one done (after ${took} ms)`);

  // a pan, while a render goes: it's dropped, and the next one's done
  await app.forgetMessages();
  await page.evaluate(() => (window.__set = performance.now()));
  await page.locator(".panel input[type=text]").first().fill("1001");
  await page.keyboard.press("Enter");
  await app.waitForMessage(`(m) => m.type === "patch"`, 10000);
  const first = await page.evaluate(() => {
    const m = window.__messages.find((m) => m.type === "patch");
    return { id: m.id, done: m.done };
  });
  const box = await app.canvas(0).boundingBox();
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + 80, box.y + box.height / 2, { steps: 4 });
  await page.mouse.up();
  await app.waitForMessage(`(m) => m.type === "patch" && m.done`, 60000).catch(() => {});
  const all = await app.messages("patch");
  const ids = [...new Set(all.map((m) => m.id))];
  const finished = all.filter((m) => m.done).map((m) => m.id);
  c.check(!first.done && ids.length >= 2 && !finished.includes(first.id) && finished.length >= 1,
          `a pan dropped the render going on (render ${first.id}), and the next was done (${finished.join(", ")})`);

  // new parameters: the first patch waits a tenth of a second
  await app.forgetMessages();
  await page.locator(".panel input[type=text]").first().fill("1002");
  await page.evaluate(() => (window.__set = performance.now()));
  await page.keyboard.press("Enter");
  await page.waitForFunction(() => window.__messages.some((m) => m.type === "patch"), null, { timeout: 10000 });
  const wait = await page.evaluate(() => performance.now() - window.__set);
  c.check(wait >= 100, `new parameters' first patch was held back (${wait.toFixed(0)} ms)`);

  // new parameters faster than renders go (as a tool's dragged): each
  // render shows its first pass before it's dropped, not nothing
  await app.forgetMessages();
  await page.evaluate(async () => {
    for (let i = 0; i < 20; i++) {
      window.__send({ type: "set", name: "work", text: String(1010 + i) });
      await new Promise((r) => setTimeout(r, 30));
    }
  });
  await page.waitForTimeout(300);
  const shown = new Set((await app.messages("patch")).map((m) => m.id)).size;
  c.check(shown >= 10, `parameters changed 20 times, every 30 ms: ${shown} renders showed something before they were dropped`);
}

async function menu() {
  let project = await app.openLibrary("Mandelbrot and Julia sets together");
  const pp = project.viewers.find((v) => v.title === "Parameter plane").id;
  await app.activate(pp);
  await page.keyboard.press("n");
  await page.waitForTimeout(500);
  const win = page.locator(".window").nth(pp);
  const openMenu = async () => { await win.locator(".window-menu-button").click(); };
  const choose = async (label) => { await page.locator(".window-menu button", { hasText: label }).first().click(); };
  await openMenu();
  const labels = (await page.locator(".window-menu .menu-label").allTextContents()).join(" | ");
  c.check(["Last view", "Next view", "Jump to view", "Original view", "Show on the sphere", "Colorizer",
           "Show pointer position", "Use subpixel smoothing"].every((l) => labels.includes(l)), `the menu has: ${labels}`);
  await page.keyboard.press("Escape");

  // a Shift-drag: a box to zoom into
  {
    const b = await app.canvas(pp).boundingBox();
    // (a small pan first: the view, as it's asked for, to compare with)
    await page.mouse.move(b.x + b.width * 0.6, b.y + b.height * 0.6);
    await page.mouse.down();
    await page.mouse.move(b.x + b.width * 0.6 + 12, b.y + b.height * 0.6, { steps: 3 });
    await page.mouse.up();
    await page.waitForTimeout(400);
    const before = (await page.evaluate((id) => window.__sent.filter((r) => r.viewer === id && r.type === "view").pop(), pp));
    await page.keyboard.down("Shift");
    await page.mouse.move(b.x + b.width * 0.25, b.y + b.height * 0.25);
    await page.mouse.down();
    await page.mouse.move(b.x + b.width * 0.5, b.y + b.height * 0.5, { steps: 5 });
    const shown = await app.viewer(pp).locator(".zoom-box").isVisible();
    await page.mouse.up();
    await page.keyboard.up("Shift");
    await page.waitForTimeout(600);
    const after = (await page.evaluate((id) => window.__sent.filter((r) => r.viewer === id && r.type === "view").pop(), pp));
    const ratio = before && after ? after.pixelSize / before.pixelSize : NaN;
    c.check(shown && Math.abs(ratio - 0.25) < 0.02 && !(await app.viewer(pp).locator(".zoom-box").isVisible()),
            `Shift-drag draws a box, and zooms into it (a pixel ${ratio.toFixed(3)} the size; a quarter of each side)`);
    // (back where it started: before the box, and before the pan)
    await page.keyboard.press("<");
    await page.waitForTimeout(600);
    await page.keyboard.press("<");
    await page.waitForTimeout(600);
  }

  // the view, as the page wants it (its latest request, or the engine's move)
  const view = async () => (await page.evaluate((id) => {
    const vs = window.__sent.filter((r) => r.viewer === id && r.type === "view");
    const moved = window.__messages.filter((m) => m.viewer === id && m.type === "view");
    return { sent: vs[vs.length - 1], moved: moved[moved.length - 1] };
  }, pp));
  const settle = () => page.waitForTimeout(700);
  const box = await app.canvas(pp).boundingBox();
  const drag = async (dx) => {
    await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
    await page.mouse.down();
    await page.mouse.move(box.x + box.width / 2 + dx, box.y + box.height / 2, { steps: 5 });
    await page.mouse.up();
    await settle();
  };
  await settle();
  await drag(100);
  const afterFirst = (await view()).sent;
  await drag(100);
  await page.keyboard.press("<");
  await settle();
  const back = (await view()).moved;
  c.check(back && Math.abs(back.cx - afterFirst.cx) < 1e-12, `< goes back a step (to cx ${back?.cx.toFixed(4)}, as after the first pan: ${afterFirst.cx.toFixed(4)})`);
  await page.keyboard.press("<");
  await settle();
  const start = (await view()).moved;
  c.check(start && start.cx > afterFirst.cx, `and again, to where it started (cx ${start?.cx.toFixed(4)})`);
  await page.keyboard.press(">");
  await settle();
  c.check(Math.abs((await view()).moved.cx - afterFirst.cx) < 1e-12, "> goes forward a step");
  await drag(-50);
  await page.keyboard.press("^");
  await settle();
  const home = (await view()).moved;
  c.check(home && Math.abs(home.cx - start.cx) < 1e-12 && Math.abs(home.cy - start.cy) < 1e-12,
          `^ goes to the original view (cx ${home?.cx.toFixed(4)})`);

  // jump to a view
  await page.keyboard.press("@");
  await page.locator(".viewer-form").waitFor({ timeout: 5000 });
  const inputs = page.locator(".viewer-form input");
  c.check((await inputs.count()) === 3, `@ asks where to jump (starting from ${(await inputs.nth(0).inputValue()).slice(0, 8)}, ...)`);
  await inputs.nth(0).fill("-1.25");
  await inputs.nth(1).fill("0.125");
  await inputs.nth(2).fill("0.001");
  await page.keyboard.press("Enter");
  await settle();
  const jumped = (await view()).moved;
  const dpr = await page.evaluate(() => Math.min(2, Math.max(1, Math.round(window.devicePixelRatio || 1))));
  c.check(jumped && jumped.cx === -1.25 && jumped.cy === 0.125 && Math.abs(jumped.px * dpr - 0.001) < 1e-15,
          `and goes there (${jumped?.cx}, ${jumped?.cy}, a pixel ${(jumped?.px * dpr).toPrecision(3)})`);

  // the sphere, saved, and reopened so
  await openMenu();
  await choose("Show on the sphere");
  await settle();
  c.check((await view()).moved.sphere === true, "Show on the sphere puts it on the sphere");
  const gb = await app.canvas(pp).boundingBox();
  await page.mouse.move(gb.x + gb.width / 2, gb.y + gb.height / 2);
  await page.mouse.down();
  await page.mouse.move(gb.x + gb.width / 2 + 60, gb.y + gb.height / 2 + 20, { steps: 4 });
  await page.mouse.up();
  await settle();
  await openMenu();
  const onSphere = await page.locator(".window-menu button", { hasText: "Show on the sphere" }).getAttribute("aria-checked");
  await page.keyboard.press("Escape");
  c.check(onSphere === "true", "and the menu says so");
  await app.forgetMessages();
  await page.evaluate(() => { window.__saved = null; window.showSaveFilePicker = undefined; });
  const download = page.waitForEvent("download", { timeout: 10000 });
  await app.projectMenu("Save project as");
  const file = await (await download).path();
  const saved = fs.readFileSync(file, "utf8");
  const props = /Parameter plane:[\s\S]*?(?=\n {4}\S|\n\S|$)/.exec(saved.slice(saved.indexOf("web-properties:")))?.[0] ?? "";
  c.check(/view: sphere/.test(props) && /rotation:/.test(props), "saved: on the sphere, and the globe's rotation");
  const copy = path.join(os.tmpdir(), "menu-sphere.fs");
  fs.writeFileSync(copy, saved);
  project = await app.openFile(copy);
  c.check(project.viewers[pp].sphere === true, "reopened, it's on the sphere");
  await openMenu();
  await choose("Show on the sphere");
  await settle();
  c.check((await view()).moved.sphere === false, "and Show on the sphere again puts it back on the plane");

  // its colorizer
  await openMenu();
  await choose("Colorizer");
  const inUse = await page.locator(".window-menu button[aria-checked=true] .menu-label").allTextContents();
  c.check(inUse.join() === "Binary decomposition", `the Colorizer submenu has the one in use checked (${inUse.join()})`);
  await app.forgetMessages();
  await choose("Escape time");
  project = await app.opened().catch(() => null);
  const rebuilt = (await app.messages("project")).pop();
  c.check(rebuilt?.rebuilt && rebuilt.viewers[pp].colorizer.inUse === "Escape time" && rebuilt.viewers[1 - pp].colorizer.inUse === "Binary decomposition",
          `choosing another rebuilds the project, with it for this viewer only (${rebuilt?.viewers[pp].colorizer.inUse})`);
  const said = (await page.locator(".notice.passing").allTextContents()).join(" | ");
  c.check(/Escape time/.test(said), `and says so (${said})`);
  await openMenu();
  await choose("Colorizer");
  await choose("Binary decomposition");
  await page.waitForTimeout(1500);
  await openMenu();
  await choose("Colorizer");
  await choose("Arguments");
  await page.locator(".viewer-form").waitFor({ timeout: 5000 });
  const fieldLabels = await page.locator(".viewer-form .form-field span").allTextContents();
  c.check(fieldLabels.length > 0, `Arguments… asks for them (${fieldLabels.join(" ")})`);
  await page.locator(".viewer-form input").first().fill("12");
  await app.forgetMessages();
  await page.keyboard.press("Enter");
  await app.waitForMessage(`(m) => m.type === "project"`, 30000).catch(() => {});
  const withArgs = (await app.messages("project")).pop();
  c.check(/^Binary decomposition\(12/.test(withArgs?.viewers[pp].colorizer.inUse ?? ""),
          `and uses them (${withArgs?.viewers[pp].colorizer.inUse})`);

  // subpixel smoothing, per viewer
  await page.waitForTimeout(800);
  const before = (await app.messages("patch")).filter((m) => m.viewer === pp && m.done).length;
  await openMenu();
  const smooth = await page.locator(".window-menu button", { hasText: "Use subpixel smoothing" }).getAttribute("aria-checked");
  await choose("Use subpixel smoothing");
  await page.waitForTimeout(1500);
  const after = (await app.messages("patch")).filter((m) => m.viewer === pp && m.done).length;
  await openMenu();
  const smooth2 = await page.locator(".window-menu button", { hasText: "Use subpixel smoothing" }).getAttribute("aria-checked");
  await page.keyboard.press("Escape");
  c.check(smooth !== smooth2 && after > before, `Use subpixel smoothing toggles it (${smooth} -> ${smooth2}), and it's rendered again`);
  c.check(await page.locator("#smoothing").count() === 0, "(and the toolbar has no Smooth switch)");

  // a real viewer's aspect ratio (and no sphere)
  project = await app.openLibrary("The real Hénon map");
  const rv = project.viewers.find((v) => !v.complex).id;
  const rwin = page.locator(".window").nth(rv);
  await rwin.locator(".window-menu-button").click();
  const rlabels = (await page.locator(".window-menu .menu-label").allTextContents()).join(" | ");
  c.check(rlabels.includes("Aspect ratio") && !rlabels.includes("Show on the sphere"),
          "a real viewer's menu has Aspect ratio…, not Show on the sphere");
  await choose("Aspect ratio");
  const ratio = page.locator(".viewer-form input");
  await ratio.fill("-3");
  await page.keyboard.press("Enter");
  await page.waitForTimeout(500);
  c.check(/positive/.test(await page.locator(".viewer-form .form-error").textContent().catch(() => "")),
          "a ratio that isn't positive is refused, in the form");
  // (its script's is 1/2: a ratio it isn't, so it's saved)
  await ratio.fill("3/2");
  await page.keyboard.press("Enter");
  await page.waitForTimeout(800);
  const moved = (await page.evaluate((id) => window.__messages.filter((m) => m.viewer === id && m.type === "view").pop(), rv));
  c.check(await page.locator(".viewer-form").count() === 0 && moved && Math.abs(moved.py / moved.px - 1.5) < 1e-12,
          `3/2 makes a pixel half again as high as it's wide (${moved && (moved.py / moved.px)})`);
  await app.forgetMessages();
  await page.evaluate(() => { window.showSaveFilePicker = undefined; });
  const download2 = page.waitForEvent("download", { timeout: 10000 });
  await app.projectMenu("Save project as");
  const saved2 = fs.readFileSync(await (await download2).path(), "utf8");
  c.check(/aspect: 1\.5/.test(saved2), "saved, with it");
  const copy2 = path.join(os.tmpdir(), "menu-aspect.fs");
  fs.writeFileSync(copy2, saved2);
  await app.openFile(copy2);
  await page.waitForTimeout(800);
  const reopened = (await app.messages("patch")).filter((m) => m.viewer === rv && m.done).pop();
  c.check(reopened && Math.abs(reopened.py / reopened.px - 1.5) < 1e-12, `reopened, it's still 3/2 (${reopened && reopened.py / reopened.px})`);
  await page.locator(".window").nth(rv).locator(".window-menu-button").click();
  await choose("Aspect ratio");
  await page.locator(".viewer-form button", { hasText: "Square pixels" }).click();
  await page.waitForTimeout(800);
  const square = (await page.evaluate((id) => window.__messages.filter((m) => m.viewer === id && m.type === "view").pop(), rv));
  c.check(square && square.py === square.px, "Square pixels makes them square");
}

async function autocolor() {
  const project = await app.openLibrary("Newton's method for z⁵");
  const v = project.viewers.findIndex((x) => x.colorizer?.inUse === "Autocolor");
  c.check(v >= 0, "a viewer colored by Autocolor");
  await page.locator(".window").nth(v).locator(".window-menu-button").click();
  await page.locator(".window-menu button", { hasText: "Colorizer" }).click();
  await page.locator(".window-menu button", { hasText: "Autocolor settings" }).click();
  const overlay = page.locator(".autocolor-overlay");
  await overlay.locator(".autocolor-entry").first().waitFor({ timeout: 10000 }).catch(() => {});
  const latest = async () => (await app.messages("autocolor")).pop()?.groups ?? [];
  const groups = await latest();
  const n0 = groups[0]?.entries.length ?? 0;
  c.check(await overlay.isVisible() && n0 > 0 && (await overlay.locator(".autocolor-entry").count()) === n0,
          `Autocolor settings… opens the editor: ${groups.length} group(s), ${n0} point(s)`);
  const settle = () => page.waitForTimeout(900);
  const entries = () => latest().then((gs) => gs[0]?.entries ?? []);

  // a point recolored: the viewer's drawn again
  const done = async () => (await app.messages("patch")).filter((m) => m.viewer === v && m.done).length;
  const before = await done();
  await overlay.locator(".autocolor-entry input[type=color]").first().evaluate((i) => {
    i.value = "#ff00ff"; i.dispatchEvent(new Event("change", { bubbles: true }));
  });
  await settle();
  c.check((await entries())[0]?.color === "#ff00ff" && (await done()) > before,
          "a point recolored, and the viewer drawn again");

  // lock, and the tolerance (a bad one is refused)
  await overlay.locator(".autocolor-check", { hasText: "Lock" }).locator("input").click();
  await settle();
  c.check((await latest())[0]?.locked === true, "Lock locks the group");
  const tolerance = overlay.locator(".autocolor-grid input[type=text]");
  await tolerance.fill("-1");
  await tolerance.press("Enter");
  await settle();
  c.check(/positive number/.test(await overlay.locator(".autocolor-status").textContent().catch(() => "")),
          "a tolerance that isn't positive is refused, and said so");
  await tolerance.fill("0.02");
  await tolerance.press("Enter");
  await page.locator(".autocolor-overlay .editor-header").click();
  await settle();
  c.check((await latest())[0]?.tolerance === "0.02", `and a good one's set (${(await latest())[0]?.tolerance})`);

  // a point added, moved, and deleted
  const newPoint = overlay.locator(".autocolor-row input[type=text]").last();
  await newPoint.fill("0.5 + 0.25i");
  await newPoint.press("Enter");
  await page.locator(".autocolor-overlay .editor-header").click();
  await settle();
  let es = await entries();
  const added = es.find((e) => e.x === 0.5 && e.y === 0.25);
  c.check(es.length === n0 + 1 && added, `a point added (${added?.point})`);
  // (the rows are in the entries' order)
  const row = overlay.locator(".autocolor-entry").nth(es.indexOf(added));
  await row.locator("input[type=text]").fill("0.75 - 0.25i");
  await row.locator("input[type=text]").press("Enter");
  await page.locator(".autocolor-overlay .editor-header").click();
  await settle();
  es = await entries();
  c.check(es.some((e) => e.x === 0.75 && e.y === -0.25) && !es.some((e) => e.x === 0.5 && e.y === 0.25), "moved");
  await overlay.locator(".autocolor-entry").nth(es.findIndex((e) => e.x === 0.75))
    .locator("button", { hasText: "Delete" }).click();
  await settle();
  c.check((await entries()).length === n0, "and deleted");

  // saved, with the edits
  await page.evaluate(() => { window.showSaveFilePicker = undefined; });
  const download = page.waitForEvent("download", { timeout: 10000 });
  await app.projectMenu("Save project as");
  const saved = fs.readFileSync(await (await download).path(), "utf8");
  const ac = saved.slice(saved.indexOf("\nautocolor:"));
  // (the tolerance as YAML writes 0.02)
  c.check(/locked: true/.test(ac) && /tolerance: 2\.0e-2/.test(ac), "saved, with the lock and the tolerance");

  // clear, and close
  await overlay.locator("button", { hasText: "Clear all" }).click();
  await settle();
  c.check((await entries()).length === 0, "Clear all empties the group");
  await overlay.locator(".editor-close").click();
  await app.forgetMessages();
  await page.waitForTimeout(800);
  c.check(!(await overlay.isVisible()) && (await app.messages("autocolor")).length === 0,
          "Close closes it (and the groups aren't posted any more)");
}

async function tables() {
  const fixture = new URL("tables.fs", import.meta.url).pathname;
  await app.openFile(fixture);
  await page.evaluate(() => { window.showSaveFilePicker = undefined; });
  const saveAs = async () => {
    const download = page.waitForEvent("download", { timeout: 10000 }).catch(() => null);
    await app.projectMenu("Save project as");
    return download;
  };
  const dialog = page.locator(".page-dialog");
  const centre = async () => {
    // (a pixel a quarter of the way across and down the viewer, from its
    // canvas: the table's 64, 64 there, filled; black, empty)
    return page.evaluate(() => {
      const cv = document.querySelector(".viewer canvas");
      return [...cv.getContext("2d").getImageData(cv.width / 4, cv.height / 4, 1, 1).data].slice(0, 3);
    });
  };
  await page.waitForTimeout(500);
  const empty = await centre();
  c.check(empty.join() === "0,0,0", `empty, the viewer's black (${empty})`);

  // empty tables: no question
  let dl = await saveAs();
  await page.waitForTimeout(500);
  c.check(!(await dialog.isVisible()) && (await dl) !== null, "with its tables empty, saving asks nothing");

  // filled by the tool: asked, with the sizes
  await app.chooseTool("Fill");
  await page.waitForTimeout(300);
  const box = await app.canvas(0).boundingBox();
  await page.mouse.click(box.x + box.width / 2, box.y + box.height / 2);
  await page.waitForTimeout(1500);
  const filled = await centre();
  c.check(Math.abs(filled[0] - 64) <= 2 && Math.abs(filled[1] - 64) <= 2 && filled[2] === 128,
          `the tool fills the table (the viewer shows ${filled})`);
  dl = saveAs();
  await dialog.waitFor({ timeout: 5000 }).catch(() => {});
  const text = await dialog.textContent().catch(() => "");
  c.check(/Save table data\?/.test(text) && /T:\s+\d+ × \d+,\s+[\d.]+ (KB|MB)/.test(text) && /before compression/.test(text),
          `filled, saving asks whether to save them (${(/T:[^A]*?(KB|MB)/.exec(text) ?? [""])[0]})`);
  await dialog.locator("button", { hasText: "Save with tables" }).click();
  const withFile = fs.readFileSync(await (await dl).path(), "utf8");
  c.check(/\ntables:/.test(withFile), "Save with tables saves them");
  const copy = path.join(os.tmpdir(), "tables-saved.fs");
  fs.writeFileSync(copy, withFile);
  await app.openFile(copy);
  await page.waitForTimeout(800);
  const reopened = await centre();
  c.check(reopened.join() === filled.join(), `reopened, the table's as it was (the viewer shows ${reopened})`);

  // without them
  dl = saveAs();
  await dialog.waitFor({ timeout: 5000 }).catch(() => {});
  await dialog.locator("button", { hasText: "Save without tables" }).click();
  const withoutFile = fs.readFileSync(await (await dl).path(), "utf8");
  c.check(!/\ntables:/.test(withoutFile), "Save without tables doesn't");

  // and cancelled
  dl = saveAs();
  await dialog.waitFor({ timeout: 5000 }).catch(() => {});
  await dialog.locator("button", { hasText: "Cancel" }).click();
  c.check((await dl) === null && !(await dialog.isVisible()), "Cancel saves nothing");

  // plain saves keep the answer (with a file to save back to: a stand-in
  // for the browser's file picker, which keeps what's written)
  await page.evaluate(() => {
    window.__written = [];
    const handle = { name: "tables-handle.fs", kind: "file",
      createWritable: async () => {
        const parts = [];
        return { write: async (b) => parts.push(b), close: async () => window.__written.push(new Blob(parts)) };
      } };
    window.showSaveFilePicker = async () => handle;
  });
  const written = async (k) => page.evaluate(async (k) => {
    for (let i = 0; i < 50 && window.__written.length <= k; i++) await new Promise((r) => setTimeout(r, 100));
    return window.__written[k] ? await window.__written[k].text() : null;
  }, k);
  await app.projectMenu("Save project as");
  await dialog.waitFor({ timeout: 5000 }).catch(() => {});
  await dialog.locator("button", { hasText: "Save without tables" }).click();
  const first = await written(0);
  await app.projectMenu("Save project");
  await page.waitForTimeout(800);
  const asked = await dialog.isVisible();
  const second = await written(1);
  c.check(first !== null && !/\ntables:/.test(first) && !asked && second !== null && !/\ntables:/.test(second),
          "a plain save after it asks nothing, and keeps the answer (without them)");
  await app.projectMenu("Save project as");
  await dialog.waitFor({ timeout: 5000 }).catch(() => {});
  const askedAgain = await dialog.isVisible();
  await dialog.locator("button", { hasText: "Save with tables" }).click();
  const third = await written(2);
  await app.projectMenu("Save project");
  const fourth = await written(3);
  c.check(askedAgain && /\ntables:/.test(third ?? "") && /\ntables:/.test(fourth ?? ""),
          "Save as asks again, and plain saves keep the new answer (with them)");
}

async function capture() {
  const project = await app.openLibrary("Mandelbrot and Julia sets together");
  const dp = project.viewers.find((v) => v.title === "Dynamical plane").id;
  const win = page.locator(".window").nth(dp);
  const capture = async (what) => {
    await win.locator(".window-menu-button").click();
    await page.locator(".window-menu button", { hasText: "Capture" }).click();
    await page.locator(".window-menu button", { hasText: what }).click();
  };
  const dialog = page.locator(".page-dialog");

  // a picture, twice the size
  await capture("Image");
  await dialog.locator("input[type=text]").fill("2");
  let dl = page.waitForEvent("download", { timeout: 30000 }).catch(() => null);
  await dialog.locator("button", { hasText: "Save" }).click();
  const png = await dl;
  const bytes = png ? fs.readFileSync(await png.path()) : Buffer.alloc(0);
  const css = await app.canvas(dp).evaluate((c) => [c.getBoundingClientRect().width, c.getBoundingClientRect().height]);
  const [w, h] = bytes.length > 24 ? [bytes.readUInt32BE(16), bytes.readUInt32BE(20)] : [0, 0];
  c.check(png?.suggestedFilename() === "Dynamical plane.png" && w === Math.round(css[0] * 2) && h === Math.round(css[1] * 2),
          `Capture > Image… saves a picture, twice the viewer's size (${w}x${h}, named ${png?.suggestedFilename()})`);

  // a movie: its script checked as it's typed
  const before = await page.locator(".panel input[type=text]").first().inputValue();
  await capture("Movie");
  const script = dialog.locator("textarea");
  await script.waitFor({ timeout: 10000 });
  const template = await script.inputValue();
  c.check(/define animate\(t\):/.test(template) && /e\^\(2 pi i t\)/.test(template),
          "Capture > Movie… starts from a script moving the first complex value around a circle");
  await script.fill("define animate(t):\n    C <- (");
  await page.waitForTimeout(900);
  const problem = await dialog.locator(".form-error").textContent();
  c.check(/⚠/.test(problem), `a script that isn't one is said so (${problem.trim().slice(0, 50)})`);
  await script.fill(template);
  await page.waitForTimeout(900);
  c.check((await dialog.locator(".form-error").textContent()) === "", "and fixed, it isn't");
  const fields = dialog.locator(".capture-grid input");
  await fields.nth(0).fill("8");
  await fields.nth(1).fill("4");
  await app.forgetMessages();
  dl = page.waitForEvent("download", { timeout: 60000 }).catch(() => null);
  await dialog.locator("button", { hasText: "Record" }).click();
  const mp4 = await dl;
  const movie = mp4 ? fs.readFileSync(await mp4.path()) : Buffer.alloc(0);
  const frames = (await app.messages("movieFrame")).length;
  c.check(mp4?.suggestedFilename() === "Dynamical plane.mp4" && movie.toString("latin1", 4, 8) === "ftyp" && frames === 8,
          `recorded: ${frames} frames, encoded and saved as an MP4 (${movie.length} bytes)`);
  await page.waitForTimeout(500);
  const after = await page.locator(".panel input[type=text]").first().inputValue();
  c.check(after === before && !(await dialog.isVisible()), `and the values are put back (C = ${after})`);

  // stopped
  await capture("Movie");
  await dialog.locator("textarea").waitFor({ timeout: 10000 });
  await dialog.locator(".capture-grid input").nth(0).fill("400");
  await app.forgetMessages();
  dl = page.waitForEvent("download", { timeout: 4000 }).catch(() => null);
  await dialog.locator("button", { hasText: "Record" }).click();
  await app.waitForMessage(`(m) => m.type === "movieFrame"`, 30000).catch(() => {});
  await dialog.locator("button", { hasText: "Stop" }).click();
  await app.waitForMessage(`(m) => m.type === "movieDone"`, 30000).catch(() => {});
  const stopped = (await app.messages("movieDone")).pop();
  c.check(stopped?.stopped && (await dl) === null && (await app.messages("movieFrame")).length < 400,
          `Stop stops it, after ${(await app.messages("movieFrame")).length} frames, and saves nothing`);
}

async function preferences() {
  const stored = () => page.evaluate(() => JSON.parse(localStorage.getItem("fractalstream.preferences") || "{}"));
  const tab = (name) => page.locator(".prefs-tab", { hasText: name }).click();
  const box = (label) => page.locator(".prefs-check", { hasText: label }).locator("input");
  await app.backToLibrary();
  await page.evaluate(() => localStorage.removeItem("fractalstream.preferences"));
  await page.locator(".welcome-preferences").click();
  c.check(await page.locator(".prefs-dialog").isVisible(), "the welcome screen's ⚙ opens Preferences");
  c.check((await page.locator(".prefs-tab").allTextContents()).join("|") === "Editor|Viewers|Colors", "with its tabs");
  const isMac = await page.evaluate(() => /Mac|iPhone|iPad|iPod/.test(navigator.platform || navigator.userAgent));
  c.check((await box("Emacs").count()) === (isMac ? 0 : 1), `emacs keys offered only where Control is the shortcut key (${isMac ? "not" : "yes"}, here)`);
  await page.keyboard.press("Escape");
  c.check(await page.locator(".prefs-dialog").count() === 0, "Escape closes it");

  // the editor, following the preferences while it's open
  await app.openLibrary("Mandelbrot and Julia sets together");
  await app.projectMenu("Edit project script");
  await page.waitForFunction(() => document.querySelectorAll(".editor-overlay [class*=tok-]").length > 20, null, { timeout: 10000 });
  c.check(await page.locator(".editor-overlay .cm-lineNumbers").count() === 0, "no line numbers, at first");
  const markers = () => page.locator('.editor-overlay .cm-foldGutter span[title="Fold line"]').count();
  const stanzaFolds = await markers();
  const stanzas = (await page.locator(".editor-overlay .cm-content").innerText()).split("\n").filter((l) => /^(@|on )/.test(l)).length;
  c.check(stanzaFolds === stanzas, `stanzas and tool handlers fold (${stanzaFolds} of ${stanzas} such lines)`);
  const linesBefore = await page.locator(".editor-overlay .cm-line").count();
  await page.locator('.editor-overlay .cm-foldGutter span[title="Fold line"]').first().click();
  await page.waitForTimeout(100);
  c.check(await page.locator(".editor-overlay .cm-foldPlaceholder").count() === 1 &&
          (await page.locator(".editor-overlay .cm-line").count()) < linesBefore, "folding a stanza hides it");
  await page.locator(".editor-overlay .cm-foldPlaceholder").click();
  await app.projectMenu("Preferences");
  await tab("Editor");
  await box("line numbers").check();
  await box("Fold blocks").check();
  await box("mathematical symbols").uncheck();
  await page.locator(".prefs-dialog input[type=text]").fill("Courier New");
  await page.locator(".prefs-size").fill("16");
  await page.locator(".prefs-size").press("Tab");
  await page.waitForTimeout(100);
  c.check(await page.locator(".editor-overlay .cm-lineNumbers").count() === 1, "line numbers, once asked for");
  c.check((await markers()) > stanzaFolds, `code blocks fold too, once asked (${await markers()} folds)`);
  const font = await page.locator(".editor-overlay .cm-editor").evaluate((e) => [getComputedStyle(e).fontFamily, getComputedStyle(e).fontSize]);
  c.check(/Courier New/.test(font[0]) && Math.abs(parseFloat(font[1]) - 16 * 4 / 3) < 0.1, `the font, as chosen (${font.join(", ")})`);

  // the keywords' color, and back
  await tab("Colors");
  await page.locator(".prefs-colors select, .prefs-pane select").first().selectOption("light");
  const keywordColor = () => page.locator(".editor-overlay .tok-keyword").first().evaluate((e) => getComputedStyle(e).color);
  const before = await keywordColor();
  await page.locator('input[aria-label="Color for keywords"]').evaluate((i) => {
    i.value = "#ff0000"; i.dispatchEvent(new Event("input", { bubbles: true }));
  });
  await page.locator('button[aria-label="Bold keywords"]').click();
  const after = await keywordColor();
  const bold = await page.locator(".editor-overlay .tok-keyword").first().evaluate((e) => getComputedStyle(e).fontWeight);
  c.check(after === "rgb(255, 0, 0)" && Number(bold) >= 600, `a class's color and boldness, as chosen (${before} to ${after}, ${bold})`);
  const s1 = await stored();
  c.check(s1["editor-colors-light"]?.keyword?.color === "#ff0000" && s1["editor-colors-light"]?.keyword?.bold === true &&
          s1["line-numbers"] === true && s1["math-symbols"] === false && s1["editor-font"] === "Courier New" &&
          s1["editor-font-size"] === 16 && !("sphere-shading" in s1),
          `kept, by the desktop's names, defaults left out (${JSON.stringify(s1)})`);
  await page.locator('button[aria-label="Reset keywords"]').click();
  c.check((await keywordColor()) === before, "Reset puts it back");
  await page.locator(".prefs-dialog button", { hasText: "Done" }).click();

  // no symbols: an edit leaves z^2 as it is
  await page.locator(".editor-overlay .cm-content").click();
  await page.keyboard.press("ControlOrMeta+End");
  await page.keyboard.type("\n# notes");
  await page.keyboard.press("ArrowUp");
  await page.waitForTimeout(800);
  const doc = await page.locator(".editor-overlay .cm-content").innerText();
  c.check(doc.includes("z^2") && !doc.includes("z²"), "without symbols, an edited script isn't rewritten");
  await page.locator(".editor-close").click();

  // the sphere's shading: drawn again; and new viewers' smoothing
  const project = await app.openLibrary("Newton's method on the sphere");
  const s = project.viewers.findIndex((v) => v.sphere);
  const picture = () => app.canvas(s).evaluate((cv) => cv.toDataURL());
  const shaded = await picture();
  const frames = await app.frames(s);
  await app.projectMenu("Preferences");
  await tab("Viewers");
  await page.locator(".prefs-dialog input[type=range]").evaluate((r) => {
    r.value = "0"; r.dispatchEvent(new Event("input", { bubbles: true }));
  });
  await page.waitForFunction(([s, f]) => Number(document.querySelectorAll(".viewer")[s].dataset.frames) > f, [s, frames], { timeout: 30000 }).catch(() => {});
  await page.waitForTimeout(1500);
  const sent = await page.evaluate(() => window.__sent.filter((m) => m.type === "sphereShading").pop());
  c.check(sent?.value === 0 && (await picture()) !== shaded, `0% shading: sent (${JSON.stringify(sent)}), and drawn again`);
  await page.locator(".prefs-dialog select").selectOption("off");
  const smooth = await page.evaluate(() => window.__sent.filter((m) => m.type === "smoothing").pop());
  c.check(smooth?.on === false, "Off: new viewers aren't smoothed");
  await page.locator(".prefs-dialog button", { hasText: "Done" }).click();

  // over a reload
  await app.backToLibrary();
  await page.reload();
  await app.waitForLibrary();
  const sentAgain = await page.evaluate(() => window.__sent.filter((m) => m.type === "sphereShading" || m.type === "smoothing"));
  c.check(sentAgain.some((m) => m.type === "sphereShading" && m.value === 0) && sentAgain.some((m) => m.type === "smoothing" && !m.on),
          "reloaded, the engine's told them again");
  await page.locator(".welcome-preferences").click();
  c.check(await page.locator('.prefs-tab[aria-selected=true]').textContent() === "Viewers", "Preferences opens on the tab it was left on");
  await tab("Editor");
  c.check(await box("line numbers").isChecked() && !(await box("mathematical symbols").isChecked()), "and the dialog shows them");
  await page.keyboard.press("Escape");
  await page.evaluate(() => localStorage.removeItem("fractalstream.preferences"));
  await page.reload();
  await app.waitForLibrary();
}

async function pathMovie() {
  const project = await app.openLibrary("Mandelbrot and Julia sets together");
  const id = (title) => project.viewers.find((v) => v.title === title).id;
  const pp = id("Parameter plane"), dp = id("Dynamical plane");
  const ppWin = page.locator(".window").nth(pp), dpWin = page.locator(".window").nth(dp);
  c.check(project.viewers[pp].animates === "C" && project.viewers[dp].animates === null,
          "the parameter plane's coordinate (C) can be animated; the dynamical plane's (z) can't");
  await dpWin.locator(".tool-chooser").click();
  const dpItems = await page.locator(".window-menu button").allTextContents();
  await page.keyboard.press("Escape");
  await ppWin.locator(".tool-chooser").click();
  await page.locator(".window-menu button", { hasText: "Animate along path" }).click();
  c.check(!dpItems.some((t) => t.includes("Animate along path")) &&
          (await ppWin.locator(".tool-chooser-label").textContent()) === "Animate along path",
          "it's a tool of the parameter plane's (and not the dynamical plane's)");

  // a path dragged out: not a pan
  const box = await app.canvas(pp).boundingBox();
  const views = async () => (await page.evaluate(() => window.__sent.filter((m) => m.type === "view").length));
  const viewsBefore = await views();
  const x0 = box.x + box.width * 0.4, y0 = box.y + box.height * 0.5;
  await page.mouse.move(x0, y0);
  await page.mouse.down();
  await page.mouse.move(x0 + 40, y0, { steps: 6 });
  await page.mouse.move(x0 + 40, y0 - 40, { steps: 6 });
  await page.mouse.up();
  const dialog = page.locator(".page-dialog");
  await dialog.waitFor({ timeout: 10000 });
  c.check((await views()) === viewsBefore, "dragging draws a path, not a pan");
  const text = await dialog.textContent();
  const recordWhich = await dialog.locator("select").first().evaluate((s) => s.selectedOptions[0].textContent);
  c.check(/C will move along the path drawn in “Parameter plane”/.test(text) && recordWhich === "Dynamical plane",
          `letting go asks how to make a movie along it, of the other viewer (${recordWhich})`);
  c.check(!(await dialog.locator(".capture-check input").first().isChecked()), "an open path doesn't loop, to begin with");

  // recorded, with the inset in the top right corner
  await dialog.locator(".capture-inset input").check();
  await dialog.locator(".capture-inset select").selectOption("1");
  const fields = dialog.locator(".capture-grid input");
  await fields.nth(0).fill("6");
  await fields.nth(1).fill("4");
  const before = await page.locator(".panel input[type=text]").first().inputValue();
  await app.forgetMessages();
  await page.evaluate(() => (window.__frames = []));
  const dl = page.waitForEvent("download", { timeout: 60000 }).catch(() => null);
  await dialog.locator("button", { hasText: "Record" }).click();
  const mp4 = await dl;
  const sent = await page.evaluate(() => window.__sent.filter((m) => m.type === "moviePathStart").pop());
  c.check(sent?.viewer === dp && sent?.drawnIn === pp && sent.corner === 1 && sent.path.length >= 10 && !sent.loop,
          `asked for: of the dynamical plane, along ${sent?.path.length} points, inset top right`);
  c.check(mp4?.suggestedFilename() === "Dynamical plane.mp4", `saved as ${mp4?.suggestedFilename()}`);
  const ppSize = await app.canvas(pp).evaluate((cv) => [cv.width, cv.height]);
  const look = await page.evaluate(([pw, ph]) => {
    const ms = window.__messages.filter((m) => m.type === "movieFrame");
    const { width: fw, height: fh } = ms[0];
    const frames = window.__frames.map((b) => new Uint8Array(b));
    const short = Math.min(fw, fh), margin = Math.max(4, Math.floor(short / 40));
    const long = Math.max(16, Math.round(0.3 * short)), scale = long / Math.max(pw, ph);
    const iw = Math.max(1, Math.round(pw * scale)), ih = Math.max(1, Math.round(ph * scale));
    const x0 = fw - margin - iw, y0 = margin;
    const at = (f, x, y) => Array.from(f.subarray(4 * (y * fw + x), 4 * (y * fw + x) + 3));
    const white = (p) => p.every((v) => v === 255);
    // the inset's border (just outside it), and what's inside it, frame by frame
    const border = [at(frames[0], x0 + 3, y0 - 1), at(frames[0], x0 - 1, y0 + 3), at(frames[0], fw - margin, y0 + 3)].every(white);
    const inside = (f) => { let h = 0; for (let y = y0; y < y0 + ih; y++) for (let x = x0; x < x0 + iw; x++) h = (h * 31 + f[4 * (y * fw + x)]) | 0; return h; };
    const last = frames[frames.length - 1];
    return { n: frames.length, border, insetMoves: inside(frames[0]) !== inside(last),
             juliaChanges: frames.some((f, i) => i > 0 && f.some((v, k) => v !== frames[0][k])) };
  }, ppSize);
  c.check(look.n === 6 && look.border, `6 frames, each with the inset, its white border in the top right corner (${JSON.stringify(look)})`);
  c.check(look.insetMoves && look.juliaChanges, "the crosshair moves along the path, and the dynamical plane changes with C");
  await page.waitForTimeout(500);
  const after = await page.locator(".panel input[type=text]").first().inputValue();
  c.check(after === before && await page.locator(".project-title-dot").isHidden(),
          `the values are put back afterwards (C = ${after}), and it isn't a change to save`);

  // Capture > Movie… can record another viewer too (the last one recorded, to begin with)
  await ppWin.locator(".window-menu-button").click();
  await page.locator(".window-menu button", { hasText: "Capture" }).click();
  await page.locator(".window-menu button", { hasText: "Movie" }).click();
  await dialog.locator("textarea").waitFor({ timeout: 10000 });
  const options = await dialog.locator("select").first().evaluate((s) => [...s.options].map((o) => o.textContent));
  const chosen = await dialog.locator("select").first().evaluate((s) => s.selectedOptions[0].textContent);
  c.check(options.join("|") === "Parameter plane|Dynamical plane" && chosen === "Dynamical plane",
          `Capture > Movie… asks which viewer to record (${chosen}, the last one)`);
  await dialog.locator("button", { hasText: "Cancel" }).click();
  await ppWin.locator(".tool-chooser").click();
  await page.locator(".window-menu button", { hasText: "Navigate" }).click();
  c.check((await ppWin.locator(".tool-chooser-label").textContent()) === "Navigate", "Navigate puts the tool down");
}
