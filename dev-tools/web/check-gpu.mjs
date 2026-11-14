// Check the GPU backends by hand (GPU_PLAN.md, G10: CI has no GPU): open
// library projects in a browser with WebGPU, draw each viewer with each
// backend in turn ("Draw with"), and say which drew it (or why it
// declined), and how much of each GPU picture matches the CPU's: as a
// yardstick, how much of the CPU's matches itself, with the view nudged by
// 1e-15 (GPU_PLAN.md, "Testing": a picture's own sensitivity, which a GPU
// picture matching as well as doubles would should come near).
//
//   node dev-tools/web/check-gpu.mjs [--images DIR] [section or file.fs ...]
//
// The library's sections by name, or the start of it (default: "Getting
// started" and "Quadratic family"; "all" for every one), and project files. A pixel matches if no
// channel differs by more than 8. See harness.mjs for what it needs (with
// FS_WEBGPU=1, which this sets: Chromium, the full browser). With
// --images, each picture is saved there too, as a PNG, with one showing
// where each GPU picture differs from the CPU's (in white).
import fs from "node:fs";
import path from "node:path";
process.env.FS_WEBGPU = "1";
const { start } = await import("./harness.mjs");

const argv = process.argv.slice(2);
const imagesAt = argv.indexOf("--images");
const images = imagesAt >= 0 ? argv.splice(imagesAt, 2)[1] : null;
if (images) fs.mkdirSync(images, { recursive: true });
const files = argv.filter((a) => a.endsWith(".fs"));
const wanted = argv.filter((a) => !a.endsWith(".fs"));
const sections = wanted.length || files.length ? wanted : ["Getting started", "Quadratic family"];
const backends = ["cpu", "gpu-df64", "gpu-f32"];

let app = null;
try {
  app = await start();
  const { page } = app;
  const gpu = await page.evaluate(async () => {
    const a = await navigator.gpu?.requestAdapter();
    return a ? `${a.info?.vendor ?? "?"} ${a.info?.architecture ?? ""}`.trim() : "none";
  });
  console.log(`WebGPU adapter: ${gpu}`);
  const projects = app.libraryProjects().filter((p) =>
    wanted.includes("all") || sections.some((s) => app.library[p.section].name.startsWith(s)));
  if (!projects.length && !files.length) throw new Error(`no sections called ${sections.join(", ")}`);
  for (const { section, entry, title, file } of [...projects, ...files.map((f) => ({ file: f, title: path.basename(f) }))]) {
    let project;
    try {
      project = file ? await app.openFile(file) : await app.openLibraryEntry(section, entry);
    } catch (e) {
      console.log(`${title}: FAILED to open: ${e.message.split("\n")[0]}`);
      continue;
    }
    for (const [id, viewer] of project.viewers.entries()) {
      const pictures = {};
      const lines = [];
      for (const b of backends) {
        app.clearErrors();
        await app.forgetMessages();
        const t0 = Date.now();
        await app.send({ type: "viewerBackend", viewer: id, backend: b });
        let ms;
        try {
          await app.waitForMessage(`(m) => m.type === "patch" && m.viewer === ${id} && m.done`, 120000);
          ms = Date.now() - t0;
        } catch {
          lines.push(`${b}: no picture`);
          continue;
        }
        const report = (await app.messages("backend")).filter((m) => m.viewer === id).pop();
        pictures[b] = await page.evaluate((k) => {
          const c = document.querySelectorAll(".viewer canvas")[k];
          return Array.from(c.getContext("2d").getImageData(0, 0, c.width, c.height).data);
        }, id);
        const drew = report?.backend ?? "nothing";
        const why = (report?.declined ?? []).map(([k, w]) => `${k} declined: ${w}`).join("; ");
        let line = `${b}: drawn with ${drew} in ${ms} ms${why ? ` (${why})` : ""}`;
        if (b !== "cpu" && drew === b && pictures.cpu) line += `, ${match(pictures.cpu, pictures[b])} like the CPU's`;
        if (images) await save(page, path.join(images, `${slug(title)}-${id}-${b}.png`), pictures[b], id);
        if (images && b !== "cpu" && pictures.cpu) {
          const diff = pictures[b].map((v, i) => i % 4 === 3 ? 255
            : Math.max(...[0, 1, 2].map((c) => Math.abs(pictures[b][i - i % 4 + c] - pictures.cpu[i - i % 4 + c]))) > 8 ? 255 : 0);
          await save(page, path.join(images, `${slug(title)}-${id}-${b}-diff.png`), diff, id);
        }
        if (app.errors.length) line += `; errors: ${app.errors.join(" | ")}`;
        lines.push(line);
        if (b === "cpu") lines.push(`cpu, nudged: ${await nudged(id, pictures.cpu)} like the CPU's`);
      }
      console.log(`${title} / ${viewer.title}:\n  ${lines.join("\n  ")}`);
      await app.send({ type: "viewerBackend", viewer: id, backend: null });
    }
  }
} finally {
  app?.close();
}

// The CPU's picture with the view nudged by 1e-15 (relative to its
// center): how much of it matches the picture, before it's put back.
async function nudged(id, picture) {
  const v = (await app.messages("patch")).filter((m) => m.viewer === id && m.done).pop();
  const view = (cx, cy) => ({ type: "view", viewer: id, cx, cy, pixelSize: v.px,
                              width: v.width, height: v.height, covered: null });
  const go = async (r) => {
    await app.forgetMessages();
    await app.send(r);
    await app.waitForMessage(`(m) => m.type === "patch" && m.viewer === ${id} && m.done`, 120000);
  };
  const eps = (x) => x + 1e-15 * Math.max(1, Math.abs(x));
  await go(view(eps(v.cx), eps(v.cy)));
  const moved = await app.page.evaluate((k) => {
    const c = document.querySelectorAll(".viewer canvas")[k];
    return Array.from(c.getContext("2d").getImageData(0, 0, c.width, c.height).data);
  }, id);
  await go(view(v.cx, v.cy));
  return match(picture, moved);
}

function slug(title) { return title.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, ""); }

// Save RGBA pixels, of viewer k's canvas's size, as a PNG.
async function save(page, file, rgba, k) {
  const url = await page.evaluate(([px, k]) => {
    const c = document.querySelectorAll(".viewer canvas")[k];
    const out = document.createElement("canvas");
    out.width = c.width; out.height = c.height;
    out.getContext("2d").putImageData(new ImageData(new Uint8ClampedArray(px), c.width, c.height), 0, 0);
    return out.toDataURL("image/png");
  }, [rgba, k]);
  fs.writeFileSync(file, Buffer.from(url.split(",")[1], "base64"));
}

// The share of pixels (RGBA) in which no channel differs by more than 8.
function match(a, b) {
  if (a.length !== b.length) return "a different size";
  let same = 0;
  for (let i = 0; i < a.length; i += 4) {
    if (Math.abs(a[i] - b[i]) <= 8 && Math.abs(a[i + 1] - b[i + 1]) <= 8 && Math.abs(a[i + 2] - b[i + 2]) <= 8) same++;
  }
  return `${(100 * same / (a.length / 4)).toFixed(2)}%`;
}
