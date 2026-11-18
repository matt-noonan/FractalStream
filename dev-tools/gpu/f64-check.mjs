// Check the GPU f64 runtime's real functions (Backend.WGSL.Runtime's
// f64Numbers) without a GPU: f64 arithmetic is JavaScript's own, so the
// runtime's WGSL, translated to JavaScript, computes what a GPU with
// correctly rounded doubles would. Each function, over many arguments,
// against JavaScript's Math (its worst error in units in the last place,
// or absolute error where the result is near 0), and the bits' round trip.
//
//   FS_WGSL_DUMP=DIR stack test fractalstream-backend-wgsl   (in its folder)
//   node dev-tools/gpu/f64-check.mjs DIR/gpu-f64.wgsl
//
// The translation is a few rewrites, enough for the runtime's own
// functions (not for kernels): it takes everything from `alias R = f64;`
// up to the lists' arena.
import fs from "node:fs";

const source = fs.readFileSync(process.argv[2], "utf8");
const start = source.indexOf("alias R = f64;");
const end = source.indexOf("var<private> arena");
if (start < 0 || end < 0) throw new Error("not an f64 kernel");
let wgsl = source.slice(start, end);
// (fnan and friends come before R, in common: take those too)
const common = source.match(/fn fnan[\s\S]*?fn ordered[^\n]*\n/);
wgsl = common[0] + wgsl;

const js = wgsl
  .replace(/^alias R = f64;$/m, "")
  .replace(/^struct C \{[^}]*\}$/m, "")
  .replace(/fn (\w+)\(([^)]*)\)\s*(->\s*[\w<>]+)?\s*\{/g,
           (_, name, args) => `function ${name}(${args.split(",").map(a => a.split(":")[0].trim()).filter(Boolean).join(", ")}) {`)
  .replace(/\b(let|var) (\w+): [\w<>]+ =/g, "let $2 =")
  .replace(/\bvar (\w+): [\w<>]+;/g, "let $1;")
  .replace(/\bvar\b/g, "let")
  .replace(/\bloop \{/g, "for (;;) {")
  .replace(/(\d)lf\b/g, "$1")
  .replace(/(\d(?:\.\d+)?(?:e[+-]?\d+)?)f\b/g, "$1")
  .replace(/\b(0x[0-9a-fA-F]+|\d+)u\b/g, "$1")
  .replace(/\bvec2<f64>\(/g, "vec2(")
  .replace(/\bvec2u\(/g, "vec2u(")
  .replace(/\barray<f64, \d+>\(([^;]*)\);/g, "[$1];")
  .replace(/bitcast<f32>\(/g, "bcF32(")
  .replace(/bitcast<u32>\(/g, "bcU32(")
  .replace(/\.x\b/g, "[0]")
  .replace(/\.y\b/g, "[1]")
  .replace(/\bf64\(/g, "(")
  .replace(/\bf32\(/g, "Math.fround(")
  .replace(/\bi32\(/g, "toI32(")
  .replace(/\bu32\(/g, "toU32(")
  .replace(/>> 1(?!\d)/g, ">> 1");

const prelude = `
const params = { zero: 0 };
const dv = new DataView(new ArrayBuffer(8));
function bcF32(u) { dv.setUint32(0, u >>> 0); return dv.getFloat32(0); }
function bcU32(f) { dv.setFloat32(0, f); return dv.getUint32(0); }
function toI32(x) { if (Number.isNaN(x)) return 0; return Math.max(-2147483648, Math.min(2147483647, Math.trunc(x))); }
function toU32(x) { if (Number.isNaN(x)) return 0; return Math.max(0, Math.min(4294967295, Math.trunc(x))) >>> 0; }
function select(a, b, c) { return c ? b : a; }
function abs(x) { return Math.abs(x); }
function min(a, b) { return Math.min(a, b); }
function max(a, b) { return Math.max(a, b); }
function clamp(x, a, b) { return Math.min(Math.max(x, a), b); }
function ldexp(x, k) { return Math.fround(x * Math.pow(2, k)); }
function inverseSqrt(x) { return Math.fround(1 / Math.sqrt(Math.fround(x))); }
function vec2(a, b) { return [a, b]; }
function vec2u(a, b) { return [a >>> 0, b >>> 0]; }
`;
let lib;
try {
  lib = new Function(prelude + js + "\nreturn { rexp, rlog, rsin, rcos, rtan, ratan2, rsinh, rcosh, rtanh, rsqrt, rfloor, rtoi, rexpo, rldexp, rf64, rtobits, risnan, risinf };")();
} catch (e) {
  fs.writeFileSync("/tmp/f64-check-translated.js", prelude + js);
  throw e;
}

const rnd = (() => { let s = 12345; return () => (s = (s * 1103515245 + 12345) % 2147483648) / 2147483648; })();
const range = (lo, hi) => () => lo + (hi - lo) * rnd();
const logRange = (lo, hi) => () => Math.exp(Math.log(lo) + (Math.log(hi) - Math.log(lo)) * rnd());
const ulp = x => { if (x === 0) return Number.MIN_VALUE; const e = Math.floor(Math.log2(Math.abs(x))); return Math.pow(2, Math.max(e, -1022) - 52); };

const cases = [
  ["exp", lib.rexp, Math.exp, range(-700, 700)],
  ["exp, near 0", lib.rexp, Math.exp, range(-1, 1)],
  ["log", lib.rlog, Math.log, logRange(1e-300, 1e300), true],
  ["log near 1", lib.rlog, Math.log, range(0.5, 2), true],
  ["sin", lib.rsin, Math.sin, range(-100, 100), true],
  ["cos", lib.rcos, Math.cos, range(-100, 100), true],
  ["sin, far", lib.rsin, Math.sin, range(-1e5, 1e5), true],
  ["sin, near", lib.rsin, Math.sin, range(-1, 1)],
  ["tan", lib.rtan, Math.tan, range(-1.5, 1.5)],
  ["atan2", lib.ratan2, Math.atan2, range(-10, 10), true, range(-10, 10)],
  ["sinh", lib.rsinh, Math.sinh, range(-20, 20)],
  ["sinh, near 0", lib.rsinh, Math.sinh, range(-0.1, 0.1)],
  ["cosh", lib.rcosh, Math.cosh, range(-20, 20)],
  ["tanh", lib.rtanh, Math.tanh, range(-5, 5)],
  ["sqrt", lib.rsqrt, Math.sqrt, logRange(1e-300, 1e300)],
  ["floor", lib.rfloor, Math.floor, range(-1e15, 1e15)],
  ["floor, small", lib.rfloor, Math.floor, range(-10, 10)],
  ["exponent", lib.rexpo, x => Math.floor(Math.log2(Math.abs(x))), logRange(1e-300, 1e300)],
];

let bad = false;
for (const [name, f, g, a, absolute, b] of cases) {
  let worst = 0, at = null;
  for (let i = 0; i < 200000; i++) {
    const x = a(), y = b ? b() : undefined;
    const got = b ? f(x, y) : f(x), want = b ? g(x, y) : g(x);
    const err = absolute && Math.abs(want) < 1 ? Math.abs(got - want) / ulp(1) : Math.abs(got - want) / ulp(want);
    if (!(err <= worst)) { worst = err; at = b ? [x, y] : x; }
  }
  const ok = worst <= 4;
  if (!ok) bad = true;
  console.log(`${ok ? "  " : "! "}${name.padEnd(14)} worst ${worst.toFixed(2)} ulp${absolute ? " (absolute below 1)" : ""}  at ${JSON.stringify(at)}`);
}

// the bits, there and back, and special values
let roundTrip = 0;
const dv = new DataView(new ArrayBuffer(8));
const samples = [0, -0, 1, -1, 0.1, 1e-310, -5e-324, 2.2250738585072014e-308, 1.7976931348623157e308, Infinity, -Infinity, NaN, Math.PI * 1e200];
for (let i = 0; i < 100000; i++) samples.push((rnd() - 0.5) * Math.pow(2, Math.floor(rnd() * 2000) - 1000));
for (const x of samples) {
  dv.setFloat64(0, x, true);
  const lo = dv.getUint32(0, true), hi = dv.getUint32(4, true);
  const back = lib.rf64(lo, hi);
  const bits = lib.rtobits(x);
  const same = Object.is(back, x) && (Number.isNaN(x) ? bits[1] === 0x7ff80000 : bits[0] === lo && bits[1] === hi);
  if (!same) { if (roundTrip++ < 5) console.log(`! bits of ${x}: ${hi.toString(16)} ${lo.toString(16)} -> ${back}, back ${bits.map(w => w.toString(16))}`); }
}
console.log(`${roundTrip ? "! " : "  "}bits          ${samples.length - roundTrip} of ${samples.length} round trips exact`);
if (roundTrip) bad = true;
process.exit(bad ? 1 : 0);
