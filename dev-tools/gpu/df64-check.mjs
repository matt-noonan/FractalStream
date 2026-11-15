// Check the GPU runtime's real functions (GPU_PLAN.md, "Testing": the df64
// harness): each one, on the GPU, over many arguments, against
// JavaScript's doubles (its worst relative error, or absolute error where
// the result is near 0), and how long a million calls take. By hand (CI
// has no GPU), in Chromium with WebGPU:
//
//   FS_WGSL_DUMP=DIR stack test fractalstream-backend-wgsl   (in its folder)
//   node dev-tools/gpu/df64-check.mjs DIR/gpu-df64.wgsl [DIR/gpu-f32.wgsl]
//
// The shaders the spec dumps start with the runtime (Backend.WGSL.Runtime),
// which this takes (everything before the kernel's @compute) and runs its
// own kernels over.
import fs from "node:fs";
import http from "node:http";
import os from "node:os";
import path from "node:path";

const pw = await import(path.join(os.homedir(), ".ghc-wasm/nodejs/lib/node_modules/playwright/index.mjs"));

// (name, the WGSL computing r from a (and b), the double it should be, arguments)
const rnd = (() => { let s = 12345; return () => (s = (s * 1103515245 + 12345) % 2147483648) / 2147483648; })();
const range = (lo, hi) => () => lo + (hi - lo) * rnd();
const logRange = (lo, hi) => () => Math.exp(Math.log(lo) + (Math.log(hi) - Math.log(lo)) * rnd());
const cases = [
  { name: "exp", wgsl: "rexp(a)", f: Math.exp, a: range(-64, 80) },   // (above 1e-28: below, df64's low part is subnormal, and flushed)
  { name: "log", wgsl: "rlog(a)", f: Math.log, a: logRange(1e-25, 1e30), abs: true },
  { name: "log near 1", wgsl: "rlog(a)", f: Math.log, a: range(0.5, 2), abs: true },
  { name: "sin", wgsl: "rsin(a)", f: Math.sin, a: range(-100, 100), abs: true },
  { name: "cos", wgsl: "rcos(a)", f: Math.cos, a: range(-100, 100), abs: true },
  { name: "sin, far", wgsl: "rsin(a)", f: Math.sin, a: range(-1e5, 1e5), abs: true },
  { name: "sin, near", wgsl: "rsin(a)", f: Math.sin, a: range(-1, 1) },
  { name: "exp, near", wgsl: "rexp(a)", f: Math.exp, a: range(-1, 1) },
  { name: "tan", wgsl: "rtan(a)", f: Math.tan, a: range(-1.5, 1.5) },
  { name: "atan2", wgsl: "ratan2(a, b)", f: Math.atan2, a: range(-10, 10), b: range(-10, 10), abs: true },
  { name: "sinh", wgsl: "rsinh(a)", f: Math.sinh, a: range(-20, 20) },
  { name: "cosh", wgsl: "rcosh(a)", f: Math.cosh, a: range(-20, 20) },
  { name: "tanh", wgsl: "rtanh(a)", f: Math.tanh, a: range(-5, 5) },
  { name: "pow", wgsl: "rpow(a, b)", f: Math.pow, a: range(0.01, 10), b: range(-8, 8) },
  { name: "sqrt", wgsl: "rsqrt(a)", f: Math.sqrt, a: logRange(1e-20, 1e20) },
  { name: "div", wgsl: "rdiv(a, b)", f: (a, b) => a / b, a: range(-100, 100), b: range(0.1, 100) },
];
const N = 65536;

function split(x) { const hi = Math.fround(x); return [hi, Math.fround(x - hi)]; }

const runtimes = process.argv.slice(2).map((f) => ({ file: f, text: fs.readFileSync(f, "utf8").split("@compute")[0] }));
if (!runtimes.length) { console.error("usage: df64-check.mjs RUNTIME.wgsl ..."); process.exit(2); }

// (WebGPU needs a secure context: localhost is one)
const server = http.createServer((q, r) => { r.setHeader("content-type", "text/html"); r.end("<!doctype html><title>df64</title>"); }).listen(0);
const browser = await pw.chromium.launch({ channel: "chromium", args: ["--enable-unsafe-webgpu"] });
const page = await browser.newPage();
await page.goto(`http://localhost:${server.address().port}/`);

for (const { file, text } of runtimes) {
  console.log(`== ${path.basename(file)}`);
  for (const c of cases) {
    const as = Array.from({ length: N }, c.a), bs = Array.from({ length: N }, c.b ?? (() => 0));
    // (the arguments as the GPU has them: exactly hi + lo)
    const words = new Float32Array(4 * N);
    as.forEach((x, i) => { const [h, l] = split(x); words[4 * i] = h; words[4 * i + 1] = l; });
    bs.forEach((x, i) => { const [h, l] = split(x); words[4 * i + 2] = h; words[4 * i + 3] = l; });
    const result = await page.evaluate(async ([runtime, call, words, n]) => {
      const adapter = await navigator.gpu.requestAdapter();
      const device = await adapter.requestDevice();
      const kernel = (body) => runtime + `
@compute @workgroup_size(64) fn main(@builtin(global_invocation_id) gid: vec3u) {
  let i = gid.x;
  if (i >= params.width) { return; }
  let a = rbits(args[4u * i], args[4u * i + 1u]);
  let b = rbits(args[4u * i + 2u], args[4u * i + 3u]);
  ${body}
}`;
      // (values; and a timing run: 16 calls an invocation, each on an
      // argument the last result nudges, so none can be skipped)
      const valueKernel = kernel(`let r = ${call}; out[2u * i] = rw0(r); out[2u * i + 1u] = rw1(r);`);
      const timeKernel = kernel(`var acc = ri(0); var x = a;
  for (var k = 0; k < 16; k++) { let r = ${call.replace(/\ba\b/g, "x")}; acc = radd(acc, r); x = radd(x, rmul(r, rbits(0x2b8cbccc, 0u))); }
  out[2u * i] = rw0(acc); out[2u * i + 1u] = rw1(acc);`);
      const layout = device.createBindGroupLayout({ entries: [
        { binding: 0, visibility: GPUShaderStage.COMPUTE, buffer: { type: "uniform" } },
        { binding: 1, visibility: GPUShaderStage.COMPUTE, buffer: { type: "read-only-storage" } },
        { binding: 2, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } },
        { binding: 3, visibility: GPUShaderStage.COMPUTE, buffer: { type: "read-only-storage" } }] });
      const pipeline = async (code) => {
        const module = device.createShaderModule({ code });
        const info = await module.getCompilationInfo();
        const errs = info.messages.filter((m) => m.type === "error");
        if (errs.length) throw new Error(errs.map((m) => `${m.lineNum}: ${m.message}`).join("; "));
        return device.createComputePipelineAsync({ layout: device.createPipelineLayout({ bindGroupLayouts: [layout] }),
                                                   compute: { module, entryPoint: "main" } });
      };
      const params = device.createBuffer({ size: 80, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
      const p = new Uint32Array(20); p[0] = n; p[1] = 1; p[2] = 1;
      device.queue.writeBuffer(params, 0, p);
      const args = device.createBuffer({ size: words.length * 4, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_DST });
      device.queue.writeBuffer(args, 0, new Float32Array(words));
      const out = device.createBuffer({ size: 8 * n, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC });
      const tables = device.createBuffer({ size: 16, usage: GPUBufferUsage.STORAGE });
      const read = device.createBuffer({ size: 8 * n, usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST });
      const bind = device.createBindGroup({ layout, entries: [params, args, out, tables].map((buffer, binding) => ({ binding, resource: { buffer } })) });
      const run = async (pl, reps) => {
        const t = performance.now();
        for (let r = 0; r < reps; r++) {
          const enc = device.createCommandEncoder();
          const pass = enc.beginComputePass();
          pass.setPipeline(pl); pass.setBindGroup(0, bind); pass.dispatchWorkgroups(Math.ceil(n / 64)); pass.end();
          if (r === reps - 1) enc.copyBufferToBuffer(out, 0, read, 0, 8 * n);
          device.queue.submit([enc.finish()]);
        }
        await read.mapAsync(GPUMapMode.READ);
        const v = new Float32Array(read.getMappedRange().slice(0));
        read.unmap();
        return { v, ms: performance.now() - t };
      };
      try {
        const { v } = await run(await pipeline(valueKernel), 1);
        const timing = await pipeline(timeKernel);
        await run(timing, 1);
        const { ms } = await run(timing, 10);
        // (ns a call: 10 runs of n invocations, 16 calls each)
        return { values: Array.from(v), ns: (ms * 1e6) / (10 * n * 16) };
      } catch (e) { return { error: String(e.message ?? e) }; }
    }, [text, c.wgsl, Array.from(words), N]);
    if (result.error) { console.log(`  ${c.name}: ${result.error.slice(0, 300)}`); continue; }
    let worst = 0, at = null;
    for (let i = 0; i < N; i++) {
      const a = words[4 * i] + words[4 * i + 1], b = words[4 * i + 2] + words[4 * i + 3];
      const exact = c.f(a, b), got = result.values[2 * i] + result.values[2 * i + 1];
      if (!Number.isFinite(exact)) continue;
      const err = Math.abs(got - exact) / (c.abs ? Math.max(1, Math.abs(exact)) : Math.abs(exact) || 1);
      if (!(err <= worst)) { worst = err; at = [a, b, exact, got]; }
    }
    console.log(`  ${c.name.padEnd(10)} worst ${(c.abs ? "error" : "rel. error").padEnd(10)} ${worst.toExponential(1).padStart(8)}   ${result.ns.toFixed(2).padStart(7)} ns a call` +
                (worst > 1e-12 && at ? `   (at ${at.slice(0, c.b ? 2 : 1).map((x) => x.toPrecision(6)).join(", ")}: ${at[2]} vs ${at[3]})` : ""));
  }
}
await browser.close();
server.close();
