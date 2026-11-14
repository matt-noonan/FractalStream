// The GPU spike (WEB_PLAN.md, Phase 0c/0d): in each browser with WebGPU,
// check the df64 primitives against doubles, then time two kernels (a
// deep Mandelbrot zoom, and Per_n's fate loop) in f32 and df64, and see how
// often each agrees with doubles, pixel by pixel.
//
//   node dev-tools/gpu-spike/run.mjs [chromium|webkit ...]
//
// Uses the GHC wasm toolchain's Playwright (see dev-tools/web/harness.mjs).
// Chromium's headless shell has no WebGPU, so this runs the full browser
// (still headless); Playwright's Firefox gives no adapter on macOS.
import fs from "node:fs";
import http from "node:http";
import os from "node:os";
import path from "node:path";

const pw = await import(path.join(os.homedir(), ".ghc-wasm/nodejs/lib/node_modules/playwright/index.mjs"));
const kernels = fs.readFileSync(new URL("kernels.js", import.meta.url), "utf8");
const browsers = process.argv.slice(2).length ? process.argv.slice(2) : ["chromium", "webkit"];

// (WebGPU needs a secure context: localhost is one)
const server = http.createServer((q, r) => { r.setHeader("content-type", "text/html"); r.end("<!doctype html><title>spike</title>"); }).listen(0);
const url = `http://localhost:${server.address().port}/`;

const median = (xs) => [...xs].sort((a, b) => a - b)[Math.floor(xs.length / 2)];

const cases = [
  // (the escape time near a point of the Mandelbrot set's boundary,
  // 10^-12 per pixel: past f32, within df64)
  { name: "mandel, 1e-12 per pixel", kernel: "mandel",
    P: { W: 1024, H: 1024, limit: 2000, cx: -0.743643887037151, cy: 0.131825904205330, px: 1e-12 } },
  // (the same, at 1/300 per pixel, where f32 is enough)
  { name: "mandel, whole set", kernel: "mandel",
    P: { W: 1024, H: 1024, limit: 2000, cx: -0.5, cy: 0, px: 3 / 1024 } },
  // (Per_n's fate loop over a square of x's, y fixed)
  { name: "per-n fate", kernel: "pern",
    P: { W: 1024, H: 1024, limit: 500, cx: 0.4, cy: 0.2, px: 4 / 1024, y: [0.3, 0.2] } },
  { name: "per-n fate, 1e-9 per pixel", kernel: "pern",
    P: { W: 1024, H: 1024, limit: 500, cx: 0.4, cy: 0.2, px: 1e-9, y: [0.3, 0.2] } },
];

for (const which of browsers) {
  const opts = which === "chromium" ? { channel: "chromium", args: ["--enable-unsafe-webgpu"] } : {};
  const browser = await pw[which].launch(opts);
  const page = await browser.newPage();
  await page.goto(url);
  await page.addScriptTag({ content: kernels });
  const adapter = await page.evaluate(async () => {
    const { adapter } = await spike.gpuDevice();
    const i = adapter.info ?? {};
    return `${i.vendor} ${i.architecture} ${i.description ?? ""}`.trim();
  });
  console.log(`== ${which} (${adapter})`);

  for (const mode of ["df64-none", "df64-mul", "df64-bits"]) {
    const worst = await page.evaluate(async (mode) => {
      const { device } = await spike.gpuDevice();
      return spike.checkPrims(device, mode, 100000);
    }, mode);
    console.log(`  ${mode} primitives, worst relative error: ` +
                Object.entries(worst).map(([k, v]) => `${k} ${v.toExponential(1)}`).join(", "));
  }

  for (const { name, kernel, P } of cases) {
    const report = await page.evaluate(async ({ kernel, P }) => {
      const { device } = await spike.gpuDevice();
      const t0 = performance.now();
      const ref = kernel === "mandel" ? spike.refMandel(P) : spike.refPern(P);
      const refMs = performance.now() - t0;
      let steps = 0;
      for (const v of ref) steps += v & 0xffff;
      const rows = [];
      for (const mode of ["f32", "df64-bits"]) {
        const { result, times, compileMs } = await spike.runKernel(device, mode, kernel, P, 5);
        rows.push({ mode, compileMs, ms: times, agree: spike.agreement(result, ref) });
      }
      return { refMs, steps, rows };
    }, { kernel, P });
    const px = P.W * P.H;
    console.log(`  ${name}: ${(report.steps / px).toFixed(1)} steps a pixel; ` +
                `JS doubles (one thread) ${report.refMs.toFixed(0)} ms`);
    for (const { mode, compileMs, ms, agree } of report.rows) {
      const m = median(ms.slice(1));
      console.log(`    ${mode.padEnd(13)} ${m.toFixed(1).padStart(7)} ms (first ${ms[0].toFixed(0)} ms, ` +
                  `pipeline ${compileMs.toFixed(0)} ms), ${(report.refMs / m).toFixed(0)}x JS, ` +
                  `agrees with doubles at ${(100 * agree).toFixed(2)}% of pixels`);
    }
  }
  await browser.close();
}
server.close();
