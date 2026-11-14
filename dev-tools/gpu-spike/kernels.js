// The GPU spike's kernels (WEB_PLAN.md, Phase 0c/0d), run in a page with
// WebGPU: a df64 library, two kernels written once over a number type R
// (f32, or df64: a value as the sum of two f32s), and the same kernels in
// JavaScript's doubles, for reference. See run.mjs.

// ---------------------------------------------------------------------
// The number types

// Every number reaches the kernel as a pair of f32s (hi, lo), hi + lo
// being the double; f32 mode just adds them.
const F32 = `
alias R = f32;
fn r(x: f32) -> R { return x; }
fn rpair(hi: f32, lo: f32) -> R { return hi + lo; }
fn rhi(a: R) -> f32 { return a; }
fn radd(a: R, b: R) -> R { return a + b; }
fn rsub(a: R, b: R) -> R { return a - b; }
fn rmul(a: R, b: R) -> R { return a * b; }
fn rdiv(a: R, b: R) -> R { return a / b; }
fn rsqrt(a: R) -> R { return sqrt(max(a, 0.0)); }
fn rlt(a: R, b: R) -> bool { return a < b; }
`;

// df64, after Dekker, Knuth and the QD library: two_sum and two_prod are
// exact, as long as the compiler neither reassociates nor contracts them.
// Both Chrome's and Safari's compilers do (Metal's fast math), so every
// step goes through g, which (guard "bits") launders it through its bits
// and an xor with 0 from a uniform, which no floating-point rewrite can see
// through. (Guard "mul", multiplying by 1 from a uniform, isn't enough;
// "none" is for comparison.)
const DF64 = (guard) => `
alias R = vec2f;
fn g(x: f32) -> f32 { return ${guard === "bits" ? "bitcast<f32>(bitcast<u32>(x) ^ u32(params[0].w - 1.0))"
                                : guard === "mul" ? "x * params[0].w" : "x"}; }
fn two_sum(a: f32, b: f32) -> vec2f {
  let s = g(a + b);
  let bb = g(s - a);
  let t = g(s - bb);
  return vec2f(s, g(g(a - t) + g(b - bb)));
}
fn quick_two_sum(a: f32, b: f32) -> vec2f {
  let s = g(a + b);
  return vec2f(s, g(b - g(s - a)));
}
fn two_prod(a: f32, b: f32) -> vec2f {
  let p = g(a * b);
  return vec2f(p, g(fma(a, b, -p)));
}
fn r(x: f32) -> R { return R(x, 0.0); }
fn rpair(hi: f32, lo: f32) -> R { return quick_two_sum(hi, lo); }
fn rhi(a: R) -> f32 { return a.x; }
fn radd(a: R, b: R) -> R {
  var s = two_sum(a.x, b.x);
  let t = two_sum(a.y, b.y);
  s.y += t.x;
  s = quick_two_sum(s.x, s.y);
  s.y += t.y;
  return quick_two_sum(s.x, s.y);
}
fn rsub(a: R, b: R) -> R { return radd(a, -b); }
fn rmul(a: R, b: R) -> R {
  var p = two_prod(a.x, b.x);
  p.y += a.x * b.y + a.y * b.x;
  return quick_two_sum(p.x, p.y);
}
fn rdiv(a: R, b: R) -> R {
  let q1 = a.x / b.x;
  var rem = rsub(a, rmul(b, r(q1)));
  let q2 = rem.x / b.x;
  rem = rsub(rem, rmul(b, r(q2)));
  let q3 = rem.x / b.x;
  return radd(quick_two_sum(q1, q2), r(q3));
}
fn rsqrt(a: R) -> R {
  if (a.x <= 0.0) { return r(0.0); }
  let x = 1.0 / sqrt(a.x);
  let ax = a.x * x;
  let d = rsub(a, two_prod(ax, ax));
  return quick_two_sum(ax, d.x * x * 0.5);
}
fn rlt(a: R, b: R) -> bool { return a.x < b.x || (a.x == b.x && a.y < b.y); }
`;

// Complex numbers over R, and what both kernels share.
const COMMON = `
@group(0) @binding(0) var<uniform> params: array<vec4f, 4>;
@group(0) @binding(1) var<storage, read_write> out: array<u32>;
@group(0) @binding(2) var<storage, read_write> outR: array<vec2f>;

struct C { re: R, im: R }
fn cr(x: f32) -> C { return C(r(x), r(0.0)); }
fn cadd(a: C, b: C) -> C { return C(radd(a.re, b.re), radd(a.im, b.im)); }
fn csub(a: C, b: C) -> C { return C(rsub(a.re, b.re), rsub(a.im, b.im)); }
fn cmul(a: C, b: C) -> C {
  return C(rsub(rmul(a.re, b.re), rmul(a.im, b.im)), radd(rmul(a.re, b.im), rmul(a.im, b.re)));
}
fn cabs2(a: C) -> R { return radd(rmul(a.re, a.re), rmul(a.im, a.im)); }
fn cdiv(a: C, b: C) -> C {
  let d = cabs2(b);
  return C(rdiv(radd(rmul(a.re, b.re), rmul(a.im, b.im)), d),
           rdiv(rsub(rmul(a.im, b.re), rmul(a.re, b.im)), d));
}
fn cinv(b: C) -> C { return cdiv(cr(1.0), b); }

// (params[0]: width, height, iterations, 1; [1]: center re hi, lo, im hi, lo;
// [2]: pixel size hi, lo; [3]: the Per kernel's y, re hi, lo, im hi, lo)
fn pixelPoint(id: vec3u) -> C {
  let W = i32(params[0].x);
  let H = i32(params[0].y);
  let px = rpair(params[2].x, params[2].y);
  return C(radd(rpair(params[1].x, params[1].y), rmul(r(f32(i32(id.x) - W / 2)), px)),
           radd(rpair(params[1].z, params[1].w), rmul(r(f32(H / 2 - i32(id.y))), px)));
}
`;

// The df64 primitives, checked against doubles: each invocation reads two
// numbers (as pairs) from outR and writes their sum, product, quotient
// and the first's square root back.
const PRIMS = `
@compute @workgroup_size(64) fn prims(@builtin(global_invocation_id) id: vec3u) {
  let n = u32(params[0].x);
  if (id.x >= n) { return; }
  let a = rpair(outR[2u * id.x].x, outR[2u * id.x].y);
  let b = rpair(outR[2u * id.x + 1u].x, outR[2u * id.x + 1u].y);
  let base = 2u * n + 4u * id.x;
  outR[base] = asPair(radd(a, b));
  outR[base + 1u] = asPair(rmul(a, b));
  outR[base + 2u] = asPair(rdiv(a, b));
  outR[base + 3u] = asPair(rsqrt(a));
}
`;

// Mandelbrot: the escape time of 0 under z^2 + c, up to the limit.
const MANDEL = `
@compute @workgroup_size(8, 8) fn main(@builtin(global_invocation_id) id: vec3u) {
  let W = u32(params[0].x);
  let H = u32(params[0].y);
  if (id.x >= W || id.y >= H) { return; }
  let limit = u32(params[0].z);
  let c = pixelPoint(id);
  var z = cr(0.0);
  var n = 0u;
  loop {
    if (n >= limit) { break; }
    let x2 = rmul(z.re, z.re);
    let y2 = rmul(z.im, z.im);
    if (rhi(radd(x2, y2)) > 4.0) { break; }
    z = C(radd(rsub(x2, y2), c.re), radd(rmul(r(2.0), rmul(z.re, z.im)), c.im));
    n += 1u;
  }
  out[id.y * W + id.x] = n;
}
`;

// Per_n's fate loop, for f(z) = (z - x)(z - w)/z^2, w = (x + y - 1)/(x - 1)
// (Caroline's chart, n = 6): the pixel is x, y is fixed; the marked points
// are 0, oo, 1, y, f(y), x; z starts at the critical value 2 x w/(x + w),
// and is iterated until it's near a marked point, by chordal distance
// (less than 1/100 of that point's distance to the nearest other one).
// Writes (which point) * 65536 + (how many steps).
const PERN = `
fn fz(z: ptr<function, C>, zi: ptr<function, bool>, s: C, p: C) {
  var num = cr(1.0);
  var den = cr(1.0);
  if (!*zi) {
    if (rhi(cabs2(*z)) > 1.0) {
      let iz = cinv(*z);
      num = cadd(csub(cr(1.0), cmul(s, iz)), cmul(p, cmul(iz, iz)));
    } else {
      num = cadd(csub(cmul(*z, *z), cmul(s, *z)), p);
      den = cmul(*z, *z);
    }
  }
  let nn = rhi(cabs2(num));
  let dd = rhi(cabs2(den));
  if (dd == 0.0) {
    *zi = true;
  } else if (dd < nn) {
    let q = cdiv(den, num);
    if (rhi(cabs2(q)) < 1e-30) { *zi = true; } else { *z = cinv(q); *zi = false; }
  } else {
    *z = cdiv(num, den);
    *zi = false;
  }
}

// the chordal distance between two points of the sphere
fn chordal(a: C, ai: bool, b: C, bi: bool) -> R {
  if (ai && bi) { return r(0.0); }
  if (ai) { return rdiv(r(1.0), rsqrt(radd(r(1.0), cabs2(b)))); }
  if (bi) { return rdiv(r(1.0), rsqrt(radd(r(1.0), cabs2(a)))); }
  return rdiv(rsqrt(cabs2(csub(a, b))),
              rsqrt(rmul(radd(r(1.0), cabs2(a)), radd(r(1.0), cabs2(b)))));
}

@compute @workgroup_size(8, 8) fn main(@builtin(global_invocation_id) id: vec3u) {
  let W = u32(params[0].x);
  let H = u32(params[0].y);
  if (id.x >= W || id.y >= H) { return; }
  let limit = u32(params[0].z);
  let x = pixelPoint(id);
  let y = C(rpair(params[3].x, params[3].y), rpair(params[3].z, params[3].w));
  let w = cdiv(csub(cadd(x, y), cr(1.0)), csub(x, cr(1.0)));
  let s = cadd(x, w);
  let p = cmul(x, w);
  let fy = cadd(csub(cr(1.0), cdiv(s, y)), cdiv(p, cmul(y, y)));
  var a: array<C, 6> = array<C, 6>(cr(0.0), cr(0.0), cr(1.0), y, fy, x);
  var ai: array<bool, 6> = array<bool, 6>(false, true, false, false, false, false);
  // each marked point's threshold, k = th^2 (1 + |a|^2) (th^2 for oo), so that
  // chordal(z, a) < th exactly when |z - a|^2 < k (1 + |z|^2)
  var k: array<R, 6>;
  for (var j = 0; j < 6; j++) {
    var m = r(1.0);
    for (var l = 0; l < 6; l++) {
      if (l != j) {
        let d = chordal(a[j], ai[j], a[l], ai[l]);
        if (rlt(d, m)) { m = d; }
      }
    }
    let th = rmul(r(0.01), m);
    k[j] = select(rmul(rmul(th, th), radd(r(1.0), cabs2(a[j]))), rmul(th, th), ai[j]);
  }
  var z = cdiv(rmulc(r(2.0), p), s);
  var zi = false;
  var t = 0u;
  var cj = 0u;
  loop {
    if (t >= limit) { break; }
    fz(&z, &zi, s, p);
    t += 1u;
    let nz = radd(r(1.0), cabs2(z));
    for (var j = 0u; j < 6u; j++) {
      var hit = false;
      if (zi) { hit = ai[j] || rlt(r(1.0), k[j]); }
      else if (ai[j]) { hit = rlt(r(1.0), rmul(k[j], nz)); }
      else { hit = rlt(cabs2(csub(z, a[j])), rmul(k[j], nz)); }
      if (hit) { cj = j + 1u; break; }
    }
    if (cj > 0u) { break; }
  }
  out[id.y * W + id.x] = cj * 65536u + t;
}
fn rmulc(a: R, b: C) -> C { return C(rmul(a, b.re), rmul(a, b.im)); }
`;

function asPair(mode) {
  return mode === "f32"
    ? "fn asPair(a: R) -> vec2f { return vec2f(a, 0.0); }"
    : "fn asPair(a: R) -> vec2f { return a; }";
}

function shader(mode, kernel) {
  const numbers = mode === "f32" ? F32 : DF64(mode.split("-")[1] ?? "none");
  return numbers + COMMON + asPair(mode) + { mandel: MANDEL, pern: PERN, prims: PRIMS }[kernel];
}

// ---------------------------------------------------------------------
// The same kernels in doubles

function split(x) { const hi = Math.fround(x); return [hi, Math.fround(x - hi)]; }

function refMandel(P) {
  const { W, H, limit, cx, cy, px } = P;
  const out = new Uint32Array(W * H);
  for (let j = 0; j < H; j++) for (let i = 0; i < W; i++) {
    const a = cx + (i - (W >> 1)) * px, b = cy + ((H >> 1) - j) * px;
    let x = 0, y = 0, n = 0;
    while (n < limit) {
      const x2 = x * x, y2 = y * y;
      if (x2 + y2 > 4) break;
      y = 2 * x * y + b; x = x2 - y2 + a; n++;
    }
    out[j * W + i] = n;
  }
  return out;
}

const cx = {
  add: (a, b) => [a[0] + b[0], a[1] + b[1]], sub: (a, b) => [a[0] - b[0], a[1] - b[1]],
  mul: (a, b) => [a[0] * b[0] - a[1] * b[1], a[0] * b[1] + a[1] * b[0]],
  abs2: (a) => a[0] * a[0] + a[1] * a[1],
  div: (a, b) => { const d = b[0] * b[0] + b[1] * b[1]; return [(a[0] * b[0] + a[1] * b[1]) / d, (a[1] * b[0] - a[0] * b[1]) / d]; },
};
cx.inv = (b) => cx.div([1, 0], b);

function chordal(a, ai, b, bi) {
  if (ai && bi) return 0;
  if (ai) return 1 / Math.sqrt(1 + cx.abs2(b));
  if (bi) return 1 / Math.sqrt(1 + cx.abs2(a));
  return Math.sqrt(cx.abs2(cx.sub(a, b))) / Math.sqrt((1 + cx.abs2(a)) * (1 + cx.abs2(b)));
}

function refPern(P) {
  const { W, H, limit, cx: x0, cy: y0, px, y } = P;
  const out = new Uint32Array(W * H);
  for (let j = 0; j < H; j++) for (let i = 0; i < W; i++) {
    const x = [x0 + (i - (W >> 1)) * px, y0 + ((H >> 1) - j) * px];
    const w = cx.div(cx.sub(cx.add(x, y), [1, 0]), cx.sub(x, [1, 0]));
    const s = cx.add(x, w), p = cx.mul(x, w);
    const fy = cx.add(cx.sub([1, 0], cx.div(s, y)), cx.div(p, cx.mul(y, y)));
    const a = [[0, 0], [0, 0], [1, 0], y, fy, x], ai = [false, true, false, false, false, false];
    const k = a.map((aj, jj) => {
      let m = 1;
      for (let l = 0; l < 6; l++) if (l !== jj) { const d = chordal(aj, ai[jj], a[l], ai[l]); if (d < m) m = d; }
      const th = 0.01 * m;
      return ai[jj] ? th * th : th * th * (1 + cx.abs2(aj));
    });
    let z = cx.div([2 * p[0], 2 * p[1]], s), zi = false, t = 0, cj = 0;
    while (t < limit) {
      // (fz)
      let num = [1, 0], den = [1, 0];
      if (!zi) {
        if (cx.abs2(z) > 1) { const iz = cx.inv(z); num = cx.add(cx.sub([1, 0], cx.mul(s, iz)), cx.mul(p, cx.mul(iz, iz))); }
        else { num = cx.add(cx.sub(cx.mul(z, z), cx.mul(s, z)), p); den = cx.mul(z, z); }
      }
      const nn = cx.abs2(num), dd = cx.abs2(den);
      if (dd === 0) zi = true;
      else if (dd < nn) { const q = cx.div(den, num); if (cx.abs2(q) < 1e-30) zi = true; else { z = cx.inv(q); zi = false; } }
      else { z = cx.div(num, den); zi = false; }
      t++;
      const nz = 1 + cx.abs2(z);
      for (let jj = 0; jj < 6; jj++) {
        const hit = zi ? (ai[jj] || 1 < k[jj]) : ai[jj] ? 1 < k[jj] * nz : cx.abs2(cx.sub(z, a[jj])) < k[jj] * nz;
        if (hit) { cj = jj + 1; break; }
      }
      if (cj > 0) break;
    }
    out[j * W + i] = cj * 65536 + t;
  }
  return out;
}

// ---------------------------------------------------------------------
// Running them

// One layout for every kernel: params, out, outR (with "auto", bindings a
// shader doesn't use are left out, and binding them fails).
function layoutFor(device) {
  const bgl = device.createBindGroupLayout({ entries: [
    { binding: 0, visibility: GPUShaderStage.COMPUTE, buffer: { type: "uniform" } },
    { binding: 1, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } },
    { binding: 2, visibility: GPUShaderStage.COMPUTE, buffer: { type: "storage" } }] });
  return { bgl, layout: device.createPipelineLayout({ bindGroupLayouts: [bgl] }) };
}

// Run GPU work, failing on any validation error (which WebGPU otherwise
// only reports to the console).
async function checked(device, work) {
  device.pushErrorScope("validation");
  const result = await work();
  const err = await device.popErrorScope();
  if (err) throw new Error("WebGPU: " + err.message);
  return result;
}

async function gpuDevice() {
  const adapter = await navigator.gpu.requestAdapter({ powerPreference: "high-performance" });
  return { adapter, device: await adapter.requestDevice() };
}

function paramsArray(P) {
  const [cxh, cxl] = split(P.cx), [cyh, cyl] = split(P.cy), [pxh, pxl] = split(P.px);
  const y = P.y ?? [0, 0];
  const [yrh, yrl] = split(y[0]), [yih, yil] = split(y[1]);
  return new Float32Array([P.W, P.H, P.limit, 1, cxh, cxl, cyh, cyl, pxh, pxl, 0, 0, yrh, yrl, yih, yil]);
}

// Run a kernel over a W x H image `reps` times; the result, and how long
// each run took (from submitting to having the result back on the CPU).
async function runKernel(device, mode, kernel, P, reps) {
  const module = device.createShaderModule({ code: shader(mode, kernel) });
  const info = await module.getCompilationInfo();
  const errors = info.messages.filter((m) => m.type === "error");
  if (errors.length) throw new Error(errors.map((m) => `${m.lineNum}:${m.linePos} ${m.message}`).join("\n"));
  const t0 = performance.now();
  const { bgl, layout } = layoutFor(device);
  const pipeline = await device.createComputePipelineAsync({ layout, compute: { module, entryPoint: "main" } });
  const compileMs = performance.now() - t0;
  const n = P.W * P.H;
  const params = device.createBuffer({ size: 64, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
  device.queue.writeBuffer(params, 0, paramsArray(P));
  const out = device.createBuffer({ size: n * 4, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC });
  const outR = device.createBuffer({ size: 16, usage: GPUBufferUsage.STORAGE });
  const read = device.createBuffer({ size: n * 4, usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST });
  const bind = device.createBindGroup({ layout: bgl, entries: [
    { binding: 0, resource: { buffer: params } }, { binding: 1, resource: { buffer: out } },
    { binding: 2, resource: { buffer: outR } }] });
  const times = [];
  let result = null;
  for (let rep = 0; rep < reps; rep++) {
    const t = performance.now();
    const enc = device.createCommandEncoder();
    const pass = enc.beginComputePass();
    pass.setPipeline(pipeline);
    pass.setBindGroup(0, bind);
    pass.dispatchWorkgroups(Math.ceil(P.W / 8), Math.ceil(P.H / 8));
    pass.end();
    enc.copyBufferToBuffer(out, 0, read, 0, n * 4);
    await checked(device, async () => device.queue.submit([enc.finish()]));
    await read.mapAsync(GPUMapMode.READ);
    result = new Uint32Array(read.getMappedRange().slice(0));
    read.unmap();
    times.push(performance.now() - t);
  }
  for (const b of [params, out, outR, read]) b.destroy();
  return { result, times, compileMs };
}

// The df64 primitives against doubles: the worst relative error of each,
// over random numbers of all sizes and signs.
async function checkPrims(device, mode, count) {
  const module = device.createShaderModule({ code: shader(mode, "prims") });
  const info = await module.getCompilationInfo();
  const errors = info.messages.filter((m) => m.type === "error");
  if (errors.length) throw new Error(errors.map((m) => `${m.lineNum}:${m.linePos} ${m.message}`).join("\n"));
  const { bgl, layout } = layoutFor(device);
  const pipeline = await device.createComputePipelineAsync({ layout, compute: { module, entryPoint: "prims" } });
  let seed = 12345;
  const rnd = () => (seed = (seed * 1103515245 + 12345) % 2147483648) / 2147483648;
  const xs = [];
  for (let i = 0; i < 2 * count; i++) {
    const v = (rnd() * 2 - 1) * Math.pow(10, Math.floor(rnd() * 16) - 8);
    xs.push(i % 2 === 0 ? Math.abs(v) + 1e-9 : v || 1);
  }
  const data = new Float32Array(2 * (2 * count + 4 * count));
  xs.forEach((v, i) => { const [h, l] = split(v); data[2 * i] = h; data[2 * i + 1] = l; });
  const params = device.createBuffer({ size: 64, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST });
  device.queue.writeBuffer(params, 0, new Float32Array([count, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]));
  const outR = device.createBuffer({ size: data.byteLength, usage: GPUBufferUsage.STORAGE | GPUBufferUsage.COPY_SRC | GPUBufferUsage.COPY_DST });
  device.queue.writeBuffer(outR, 0, data);
  const out = device.createBuffer({ size: 16, usage: GPUBufferUsage.STORAGE });
  const read = device.createBuffer({ size: data.byteLength, usage: GPUBufferUsage.MAP_READ | GPUBufferUsage.COPY_DST });
  const bind = device.createBindGroup({ layout: bgl, entries: [
    { binding: 0, resource: { buffer: params } }, { binding: 1, resource: { buffer: out } },
    { binding: 2, resource: { buffer: outR } }] });
  const enc = device.createCommandEncoder();
  const pass = enc.beginComputePass();
  pass.setPipeline(pipeline); pass.setBindGroup(0, bind); pass.dispatchWorkgroups(Math.ceil(count / 64)); pass.end();
  enc.copyBufferToBuffer(outR, 0, read, 0, data.byteLength);
  await checked(device, async () => device.queue.submit([enc.finish()]));
  await read.mapAsync(GPUMapMode.READ);
  const got = new Float32Array(read.getMappedRange().slice(0));
  const worst = { add: 0, mul: 0, div: 0, sqrt: 0 };
  // (the inputs as the GPU had them: exactly hi + lo)
  const input = (i) => data[2 * i] + data[2 * i + 1];
  for (let i = 0; i < count; i++) {
    const a = input(2 * i), b = input(2 * i + 1);
    const exact = { add: a + b, mul: a * b, div: a / b, sqrt: Math.sqrt(a) };
    ["add", "mul", "div", "sqrt"].forEach((op, k) => {
      const base = 2 * (2 * count + 4 * i + k);
      const v = got[base] + got[base + 1];
      const e = exact[op];
      // (relative to the operands' size for a sum, which can cancel)
      const scale = op === "add" ? Math.max(Math.abs(a), Math.abs(b)) : Math.abs(e);
      if (scale > 0) worst[op] = Math.max(worst[op], Math.abs(v - e) / scale);
    });
  }
  return worst;
}

function agreement(a, b) {
  let same = 0;
  for (let i = 0; i < a.length; i++) if (a[i] === b[i]) same++;
  return same / a.length;
}

globalThis.spike = { gpuDevice, runKernel, checkPrims, refMandel, refPern, agreement, shader };
