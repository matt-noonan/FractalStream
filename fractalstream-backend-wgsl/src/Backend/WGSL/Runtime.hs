-- | The WGSL a kernel starts with: its bindings, and the numbers it
-- computes with, in double-float or single precision.
--
-- Generated code computes with real numbers only through the functions
-- here (@radd@, @rmul@, ...), on the type @R@, so the same code is either
-- precision: in double-float, @R@ is a @vec2f@ (hi, lo: the number is
-- their sum, about 48 bits, with f32's range), after Dekker, Knuth and
-- the QD library; in single precision, an @f32@. Complex numbers are a
-- struct of two @R@s, integers @i32@s (wrapping, as the CPU's), truth
-- values @bool@s, and colors @vec3u@s (a byte a channel).
--
-- df64's @two_sum@ and @two_prod@ are exact only if the compiler neither
-- reassociates nor contracts them, and the browsers' compilers do (fast
-- math, on Metal at least: see WEB_PLAN.md, "The GPU spike"). So each step
-- goes through @g@, which launders it through its bits and an xor with 0
-- from the parameters (@params.zero@), which no rewrite can see through.
module Backend.WGSL.Runtime
  ( Precision(..)
  , runtime
  , arenaWords
  ) where

import Data.List (intercalate)
import GHC.Float (castFloatToWord32)
import Numeric (showHex)

-- | How many words of scratch space (for lists) each pixel has: 4 KB.
arenaWords :: Int
arenaWords = 1024

-- | The two kinds of real number a kernel can compute with.
data Precision = DoubleFloat | Single
  deriving (Eq, Show)

-- | The bindings and functions every kernel has.
runtime :: Precision -> String
runtime p = unlines [bindings, numbers p, common, lists p]

bindings :: String
bindings = unlines
  [ "// A block's parameters (80 bytes): its size, how many subsamples a"
  , "// pixel, 0 (for g), where the block is in the image, and (as pairs of"
  , "// f32s, hi and lo) the image's first pixel's point, the step between"
  , "// pixels, and the pixel size the code sees."
  , "struct Params {"
  , "  width: u32, height: u32, subsamples: u32, zero: u32,"
  , "  offX: i32, offY: i32, pad0: u32, pad1: u32,"
  , "  x0: vec2f, y0: vec2f, dx: vec2f, dy: vec2f, px: vec2f, py: vec2f,"
  , "}"
  , "@group(0) @binding(0) var<uniform> params: Params;"
  , "// the configuration's values (see Backend.WGSL.Code)"
  , "@group(0) @binding(1) var<storage, read> args: array<u32>;"
  , "// the block's pixels, 0xIIBBGGRR (II: the pixel's issues, bit 0 for"
  , "// running out of list space, bit 1 for a bad index)"
  , "@group(0) @binding(2) var<storage, read_write> out: array<u32>;"
  , "// the configuration's tables' elements, laid out as on the CPU"
  , "// (Language.Table), one after another"
  , "@group(0) @binding(3) var<storage, read> tables: array<u32>;"
  ]

numbers :: Precision -> String
numbers = \case
  DoubleFloat -> unlines $
    [ "alias R = vec2f;"
    , "fn g(x: f32) -> f32 { return bitcast<f32>(bitcast<u32>(x) ^ params.zero); }"
    , "fn two_sum(a: f32, b: f32) -> vec2f {"
    , "  let s = g(a + b);"
    , "  let bb = g(s - a);"
    , "  let t = g(s - bb);"
    , "  return vec2f(s, g(g(a - t) + g(b - bb)));"
    , "}"
    , "fn quick_two_sum(a: f32, b: f32) -> vec2f {"
    , "  let s = g(a + b);"
    , "  return vec2f(s, g(b - g(s - a)));"
    , "}"
    , "fn two_prod(a: f32, b: f32) -> vec2f {"
    , "  let p = g(a * b);"
    , "  return vec2f(p, g(fma(a, b, -p)));"
    , "}"
    , "fn rbits(hi: u32, lo: u32) -> R { return R(bitcast<f32>(hi), bitcast<f32>(lo)); }"
    , "fn rp(v: vec2f) -> R { return v; }"
    , "fn rf(a: R) -> f32 { return a.x + a.y; }"
    , "fn ri(i: i32) -> R { let h = f32(i); return R(h, f32(i - i32(h))); }"
    , "fn radd(a: R, b: R) -> R {"
    , "  var s = two_sum(a.x, b.x);"
    , "  let t = two_sum(a.y, b.y);"
    , "  s.y = g(s.y + t.x);"
    , "  s = quick_two_sum(s.x, s.y);"
    , "  s.y = g(s.y + t.y);"
    , "  return quick_two_sum(s.x, s.y);"
    , "}"
    , "fn rneg(a: R) -> R { return -a; }"
    , "fn rsub(a: R, b: R) -> R { return radd(a, -b); }"
    , "fn rmul(a: R, b: R) -> R {"
    , "  var p = two_prod(a.x, b.x);"
    , "  p.y = g(p.y + g(g(a.x * b.y) + g(a.y * b.x)));"
    , "  return quick_two_sum(p.x, p.y);"
    , "}"
    -- (by a divisor near f32's largest, both scaled down first: the GPU
    -- divides by multiplying by the reciprocal, which would flush to 0)
    , "fn rdiv(a: R, b: R) -> R {"
    , "  if (abs(b.x) > 1e37) { return rdiv0(rscale(a, -64), rscale(b, -64)); }"
    , "  return rdiv0(a, b);"
    , "}"
    , "fn rdiv0(a: R, b: R) -> R {"
    , "  let q1 = g(a.x / b.x);"
    , "  var rem = rsub(a, rmul(b, R(q1, 0.0)));"
    , "  let q2 = g(rem.x / b.x);"
    , "  rem = rsub(rem, rmul(b, R(q2, 0.0)));"
    , "  let q3 = g(rem.x / b.x);"
    , "  return radd(quick_two_sum(q1, q2), R(q3, 0.0));"
    , "}"
    , "fn rsqrt(a: R) -> R {"
    , "  if (fisnan(a.x) || a.x < 0.0) { return R(fnan(), 0.0); }"
    , "  if (fisinf(a.x)) { return a; }"
    , "  if (a.x == 0.0) { return R(0.0, 0.0); }"
    , "  let x = 1.0 / sqrt(a.x);"
    , "  let ax = g(a.x * x);"
    , "  let d = rsub(a, two_prod(ax, ax));"
    , "  return quick_two_sum(ax, g(d.x * x * 0.5));"
    , "}"
    , "fn rabs(a: R) -> R { return select(a, -a, a.x < 0.0); }"
    , "fn rlt(a: R, b: R) -> bool { return ordered(a.x, b.x) && (a.x < b.x || (a.x == b.x && a.y < b.y)); }"
    , "fn req(a: R, b: R) -> bool { return ordered(a.x, b.x) && a.x == b.x && a.y == b.y; }"
    , "fn rfloor(a: R) -> R {"
    , "  let h = floor(a.x);"
    , "  if (h != a.x) { return R(h, 0.0); }"
    , "  return quick_two_sum(h, floor(a.y));"
    , "}"
    , "fn rtoi(a: R) -> i32 {"
    , "  if (fisnan(a.x)) { return 0; }"
    , "  if (a.x >= 2147483648.0) { return 2147483647; }"
    , "  if (a.x < -2147483648.0) { return -2147483647 - 1; }"
    , "  return i32(a.x) + i32(a.y);"
    , "}"
    , "fn rscale(a: R, k: i32) -> R { return R(ldexp(a.x, k), ldexp(a.y, k)); }"
    , "fn rldexp(a: R, k: i32) -> R { return rscale(a, k); }"
    -- exp: e^a = 2^m e^r, r = a - m ln 2, and e^r as (1 + s)^512, s from
    -- the Taylor series of e^(r / 512) - 1
    ] ++ df64Functions
  Single -> unlines
    [ "alias R = f32;"
    , "fn rbits(hi: u32, lo: u32) -> R { return bitcast<f32>(hi) + bitcast<f32>(lo); }"
    , "fn rp(v: vec2f) -> R { return v.x + v.y; }"
    , "fn rf(a: R) -> f32 { return a; }"
    , "fn ri(i: i32) -> R { return f32(i); }"
    , "fn radd(a: R, b: R) -> R { return a + b; }"
    , "fn rneg(a: R) -> R { return -a; }"
    , "fn rsub(a: R, b: R) -> R { return a - b; }"
    , "fn rmul(a: R, b: R) -> R { return a * b; }"
    , "fn rdiv(a: R, b: R) -> R {"
    , "  if (abs(b) > 1e37) { return ldexp(a, -64) / ldexp(b, -64); }"
    , "  return a / b;"
    , "}"
    , "fn rsqrt(a: R) -> R {"
    , "  if (fisnan(a) || a < 0.0) { return fnan(); }"
    , "  if (fisinf(a)) { return a; }"
    , "  if (a == 0.0) { return 0.0; }"
    , "  return sqrt(a);"
    , "}"
    , "fn rabs(a: R) -> R { return abs(a); }"
    , "fn rlt(a: R, b: R) -> bool { return ordered(a, b) && a < b; }"
    , "fn req(a: R, b: R) -> bool { return ordered(a, b) && a == b; }"
    , "fn rfloor(a: R) -> R { return floor(a); }"
    , "fn rtoi(a: R) -> i32 {"
    , "  if (fisnan(a)) { return 0; }"
    , "  if (a >= 2147483648.0) { return 2147483647; }"
    , "  if (a < -2147483648.0) { return -2147483647 - 1; }"
    , "  return i32(a);"
    , "}"
    , "fn rldexp(a: R, k: i32) -> R { return ldexp(a, k); }"
    , "fn rexp(a: R) -> R { return exp(a); }"
    , "fn rlog(a: R) -> R { return log(a); }"
    , "fn rsin(a: R) -> R { return sin(a); }"
    , "fn rcos(a: R) -> R { return cos(a); }"
    , "fn rtan(a: R) -> R { return tan(a); }"
    , "fn ratan2(y: R, x: R) -> R { return atan2(y, x); }"
    -- (sinh and tanh by a series near 0, where WGSL's cancel)
    , "fn rsinh(a: R) -> R { if (abs(a) < 0.05) { let z = a * a; return a * (1.0 + z * (1.0 / 6.0 + z / 120.0)); } return sinh(a); }"
    , "fn rcosh(a: R) -> R { return cosh(a); }"
    , "fn rtanh(a: R) -> R { if (abs(a) < 0.05) { let z = a * a; return a * (1.0 - z * (1.0 / 3.0 - z * 2.0 / 15.0)); } return tanh(a); }"
    ]

-- | What both precisions share, written over R.
common :: String
common = unlines
  -- (from the parameters: as constants, the compiler rejects them)
  [ "fn fnan() -> f32 { return bitcast<f32>(0x7fc00000u | params.zero); }"
  , "fn finf() -> f32 { return bitcast<f32>(0x7f800000u | params.zero); }"
  , "fn fisnan(x: f32) -> bool { return (bitcast<u32>(x) & 0x7fffffffu) > 0x7f800000u; }"
  , "fn fisinf(x: f32) -> bool { return (bitcast<u32>(x) & 0x7fffffffu) == 0x7f800000u; }"
  -- (comparisons with NaN are false, as on the CPU. The browsers'
  -- compilers assume there are no infinities or NaNs (fast math), so the
  -- functions here that must handle them ask through their bits)
  , "fn ordered(a: f32, b: f32) -> bool { return !fisnan(a) && !fisnan(b); }"
  , "fn rnonzero(a: R) -> bool { return !req(a, ri(0)); }"
  , "fn rceil(a: R) -> R { return rneg(rfloor(rneg(a))); }"
  , "fn rtrunc(a: R) -> R { return select(rceil(a), rfloor(a), !rlt(a, ri(0))); }"
  -- (fmod, then nonnegative for a positive divisor, as the CPU's. fmod's
  -- remainder is exact, with a's sign, and less than |b|: the quotient,
  -- rounded, can be one off, which the remainder shows)
  , "fn rfmod(a: R, b: R) -> R {"
  , "  let bb = rabs(b);"
  , "  var m = rsub(a, rmul(bb, rtrunc(rdiv(a, bb))));"
  , "  if (!rlt(a, ri(0))) {"
  , "    if (rlt(m, ri(0))) { m = radd(m, bb); }"
  , "    if (!rlt(m, bb)) { m = rsub(m, bb); }"
  , "  } else {"
  , "    if (rlt(ri(0), m)) { m = rsub(m, bb); }"
  , "    if (!rlt(rneg(bb), m)) { m = radd(m, bb); }"
  , "  }"
  , "  return m;"
  , "}"
  , "fn rmod(a: R, b: R) -> R {"
  , "  let m = rfmod(a, b);"
  , "  return select(m, radd(m, b), rlt(m, ri(0)));"
  , "}"
  , "struct C { re: R, im: R }"
  , "fn cadd(a: C, b: C) -> C { return C(radd(a.re, b.re), radd(a.im, b.im)); }"
  , "fn csub(a: C, b: C) -> C { return C(rsub(a.re, b.re), rsub(a.im, b.im)); }"
  , "fn cneg(a: C) -> C { return C(rneg(a.re), rneg(a.im)); }"
  , "fn cconj(a: C) -> C { return C(a.re, rneg(a.im)); }"
  , "fn cmul(a: C, b: C) -> C {"
  , "  return C(rsub(rmul(a.re, b.re), rmul(a.im, b.im)), radd(rmul(a.re, b.im), rmul(a.im, b.re)));"
  , "}"
  -- (b scaled by a power of 2 near its size first, so that |b|^2 can't
  -- underflow or overflow, as it would in f32's range for |b| below 1e-19
  -- or above 1e19, where doubles' is fine)
  , "fn cdiv(a: C, b0: C) -> C {"
  , "  let m = rf(select(rabs(b0.re), rabs(b0.im), rlt(rabs(b0.re), rabs(b0.im))));"
  , "  var e = 0;"
  , "  if (m != 0.0 && !fisinf(m) && !fisnan(m)) { e = i32((bitcast<u32>(m) >> 23u) & 255u) - 127; }"
  , "  let b = C(rldexp(b0.re, -e), rldexp(b0.im, -e));"
  , "  let d = radd(rmul(b.re, b.re), rmul(b.im, b.im));"
  , "  return C(rldexp(rdiv(radd(rmul(a.re, b.re), rmul(a.im, b.im)), d), -e),"
  , "           rldexp(rdiv(rsub(rmul(a.im, b.re), rmul(a.re, b.im)), d), -e));"
  , "}"
  -- (hypot: scaled by a power of 2 near the larger part, so as not to
  -- overflow. Not divided by it: the GPU divides by multiplying by the
  -- reciprocal, which, of a number near f32's largest, is flushed to 0)
  , "fn cabs(a: C) -> R {"
  , "  let x = rabs(a.re);"
  , "  let y = rabs(a.im);"
  , "  if (fisinf(rf(x)) || fisinf(rf(y))) { return rp(vec2f(finf(), 0.0)); }"
  , "  if (fisnan(rf(x)) || fisnan(rf(y))) { return rp(vec2f(fnan(), 0.0)); }"
  , "  let m = select(x, y, rlt(x, y));"
  , "  if (!rnonzero(m)) { return ri(0); }"
  , "  let e = i32((bitcast<u32>(rf(m)) >> 23u) & 255u) - 127;"
  , "  let u = rldexp(x, -e);"
  , "  let v = rldexp(y, -e);"
  , "  return rldexp(rsqrt(radd(rmul(u, u), rmul(v, v))), e);"
  , "}"
  -- (the principal square root, as Data.Complex's)
  , "fn csqrt(z: C) -> C {"
  , "  if (!rnonzero(z.re) && !rnonzero(z.im)) { return C(ri(0), ri(0)); }"
  , "  let u = rsqrt(rmul(rdiv(ri(1), ri(2)), radd(cabs(z), rabs(z.re))));"
  , "  let v = rdiv(rabs(z.im), rmul(ri(2), u));"
  , "  let neg = rlt(z.re, ri(0));"
  , "  let re = select(u, v, neg);"
  , "  let im0 = select(v, u, neg);"
  , "  return C(re, select(im0, rneg(im0), rlt(z.im, ri(0))));"
  , "}"
  -- (C's pow, which WGSL's isn't for negative bases)
  , "fn rpow(x: R, y: R) -> R {"
  , "  if (!rnonzero(y)) { return ri(1); }"
  , "  if (!rnonzero(x)) { return select(ri(0), rp(vec2f(finf(), 0.0)), rlt(y, ri(0))); }"
  , "  if (rlt(x, ri(0))) {"
  , "    if (rne(rfloor(y), y)) { return rp(vec2f(fnan(), 0.0)); }"
  , "    let p = rexp(rmul(y, rlog(rneg(x))));"
  , "    let half = rmul(y, rdiv(ri(1), ri(2)));"
  , "    return select(p, rneg(p), rne(rfloor(half), half));"
  , "  }"
  , "  return rexp(rmul(y, rlog(x)));"
  , "}"
  , "fn rasin(a: R) -> R { return ratan2(a, rsqrt(rsub(ri(1), rmul(a, a)))); }"
  , "fn racos(a: R) -> R { return ratan2(rsqrt(rsub(ri(1), rmul(a, a))), a); }"
  , "fn ratan(a: R) -> R { return ratan2(a, ri(1)); }"
  , "fn rasinh(a: R) -> R {"
  , "  let b = rabs(a);"
  , "  let r = rlog(radd(b, rsqrt(radd(rmul(b, b), ri(1)))));"
  , "  return select(r, rneg(r), rlt(a, ri(0)));"
  , "}"
  , "fn racosh(a: R) -> R { return rlog(radd(a, rsqrt(rsub(rmul(a, a), ri(1))))); }"
  , "fn ratanh(a: R) -> R { return rmul(rdiv(ri(1), ri(2)), rlog(rdiv(radd(ri(1), a), rsub(ri(1), a)))); }"
  -- (complex functions, as the CPU's compute them)
  , "fn carg(z: C) -> R { return ratan2(z.im, z.re); }"
  , "fn cexp(z: C) -> C { let e = rexp(z.re); return C(rmul(e, rcos(z.im)), rmul(e, rsin(z.im))); }"
  , "fn clog(z: C) -> C { return C(rlog(cabs(z)), ratan2(z.im, z.re)); }"
  , "fn csin(z: C) -> C { return C(rmul(rsin(z.re), rcosh(z.im)), rmul(rcos(z.re), rsinh(z.im))); }"
  , "fn ccos(z: C) -> C { return C(rmul(rcos(z.re), rcosh(z.im)), rneg(rmul(rsin(z.re), rsinh(z.im)))); }"
  , "fn csinh(z: C) -> C { return C(rmul(rsinh(z.re), rcos(z.im)), rmul(rcosh(z.re), rsin(z.im))); }"
  , "fn ccosh(z: C) -> C { return C(rmul(rcosh(z.re), rcos(z.im)), rmul(rsinh(z.re), rsin(z.im))); }"
  -- (tan(x + iy) from tan x and tanh y; tanh(x + iy) from tanh x and tan y)
  , "fn tandiv(a: R, b: R, hyperbolic: bool) -> C {"
  , "  let ab = rmul(a, b);"
  , "  let a2b = rmul(a, ab);"
  , "  let ab2 = rmul(b, ab);"
  , "  let den = radd(ri(1), rmul(a, ab2));"
  , "  let re = select(rsub(a, ab2), radd(a, ab2), hyperbolic);"
  , "  let im = select(radd(b, a2b), rsub(b, a2b), hyperbolic);"
  , "  return C(rdiv(re, den), rdiv(im, den));"
  , "}"
  , "fn ctan(z: C) -> C { return tandiv(rtan(z.re), rtanh(z.im), false); }"
  , "fn ctanh(z: C) -> C { return tandiv(rtanh(z.re), rtan(z.im), true); }"
  -- (z^p as exp(p log z), but for z^0 = 1, and 0^p: 0, inf or NaN as
  -- Re p is positive, negative or zero)
  , "fn cpow(z: C, p: C) -> C {"
  , "  if (!rnonzero(p.re) && !rnonzero(p.im)) { return C(ri(1), ri(0)); }"
  , "  if (!rnonzero(z.re) && !rnonzero(z.im)) {"
  , "    if (rlt(ri(0), p.re)) { return C(ri(0), ri(0)); }"
  , "    if (rlt(p.re, ri(0))) { return C(rp(vec2f(finf(), 0.0)), ri(0)); }"
  , "    return C(rp(vec2f(fnan(), 0.0)), rp(vec2f(fnan(), 0.0)));"
  , "  }"
  , "  return cexp(cmul(p, clog(z)));"
  , "}"
  -- (|z| < r and l < |z|: squared, as the CPU compares them, when the
  -- squares can't overflow; else |z|, scaled)
  , "fn small(x: R) -> bool { return abs(rf(x)) < 1e18; }"
  , "fn cabslt(z: C, r: R) -> bool {"
  , "  if (small(z.re) && small(z.im) && small(r)) {"
  , "    return rlt(radd(rmul(z.re, z.re), rmul(z.im, z.im)), rmul(rabs(r), r));"
  , "  }"
  , "  return rlt(cabs(z), r);"
  , "}"
  , "fn cabsgt(z: C, l: R) -> bool {"
  , "  if (small(z.re) && small(z.im) && small(l)) {"
  , "    return rlt(rmul(rabs(l), l), radd(rmul(z.re, z.re), rmul(z.im, z.im)));"
  , "  }"
  , "  return rlt(l, cabs(z));"
  , "}"
  , "fn ceq(a: C, b: C) -> bool { return req(a.re, b.re) && req(a.im, b.im); }"
  -- (ordered and unequal: NaN is unequal to nothing, as the CPU's)
  , "fn rne(a: R, b: R) -> bool { return rlt(a, b) || rlt(b, a); }"
  , "fn cne(a: C, b: C) -> bool { return rne(a.re, b.re) || rne(a.im, b.im); }"
  -- (integers, as the CPU's: dividing by 0 gives 0, and the remainder
  -- the dividend)
  , "fn idiv(x: i32, y: i32) -> i32 {"
  , "  if (y == 0) { return 0; }"
  , "  if (y == -1) { return 0 - x; }"
  , "  return x / y;"
  , "}"
  , "fn imod(x: i32, y: i32) -> i32 {"
  , "  if (y == 0) { return x; }"
  , "  if (y == -1) { return 0; }"
  , "  return x % y;"
  , "}"
  , "fn ipow(b: i32, e: i32) -> i32 {"
  , "  if (e < 0) {"
  , "    if (b == 1) { return 1; }"
  , "    if (b == -1) { return select(1, -1, (e & 1) != 0); }"
  , "    return 0;"
  , "  }"
  , "  var r = 1;"
  , "  for (var k = 0; k < e; k++) { r = r * b; }"
  , "  return r;"
  , "}"
  , "fn iabs(x: i32) -> i32 { return select(x, 0 - x, x < 0); }"
  -- (a color channel in [0, 1] as a byte: clamped, NaN as 0, rounded)
  , "fn cbyte(c: R) -> u32 {"
  , "  let v = rf(c);"
  , "  if (fisnan(v)) { return 0u; }"
  , "  return u32(clamp(v, 0.0, 1.0) * 255.0 + 0.5);"
  , "}"
  -- (s c + (1 - s) c', truncated to a byte: saturating at 0, keeping the
  -- low byte, as the CPU's)
  , "fn cblend(s: R, a: vec3u, b: vec3u) -> vec3u {"
  , "  let t = rf(s);"
  , "  let v = t * vec3f(a) + (1.0 - t) * vec3f(b);"
  , "  return vec3u(cmix(v.x), cmix(v.y), cmix(v.z));"
  , "}"
  , "fn cmix(v: f32) -> u32 {"
  , "  if (fisnan(v) || v <= 0.0) { return 0u; }"
  , "  if (v >= 4294967295.0) { return 255u; }"
  , "  return u32(v) & 255u;"
  , "}"
  ]

-- | Lists and tables.
--
-- A list is a word address: 0 for the empty list, else its block's
-- first word, its length (its top bit: shared, see @lshare@), followed
-- by its elements (a truth value,
-- integer, color or list in one word; a real number in two, as @rbits@
-- reads them; a complex number in four; a pair its parts'). A list made
-- by the kernel is in the pixel's @arena@ (bump-allocated from word 1,
-- afresh for each subsample, never changed once made); a configuration's
-- is in @args@, with the address's top bit set. Running out of arena sets
-- @overflow@, and gives the empty list; indexing out of range sets
-- @badIndex@: either makes the pixel magenta, as on the CPU.
--
-- A table is its descriptor's index in @args@: its elements' first word in
-- @tables@, its number of dimensions, its origin's x and y, a cell's width
-- and height (each as two words, as @rbits@ reads them), and its size
-- along each dimension. Its elements are as on the CPU: reals as f64s,
-- decoded by @rf64@.
lists :: Precision -> String
lists p = unlines $
  [ "var<private> arena: array<u32, " ++ show arenaWords ++ ">;"
  , "var<private> bump: u32;"
  , "var<private> overflow: bool;"
  , "var<private> badIndex: bool;"
  , "fn lw(a: u32) -> u32 {"
  , "  if (a >= 0x80000000u) { return args[a - 0x80000000u]; }"
  , "  return arena[min(a, " ++ show (arenaWords - 1) ++ "u)];"
  , "}"
  , "fn sw(a: u32, v: u32) { if (a > 0u && a < " ++ show arenaWords ++ "u) { arena[a] = v; } }"
  , "fn alloc(n: u32) -> u32 {"
  , "  if (n > " ++ show arenaWords ++ "u - bump) { overflow = true; return 0u; }"
  , "  let a = bump;"
  , "  bump += n;"
  , "  return a;"
  , "}"
  -- (a block's first word is its length, its top bit set once another
  -- variable or list may hold it: so it's extended in place only while
  -- it's unshared, and at the top of the arena; see Backend.WGSL.Data)
  , "fn llen(h: u32) -> i32 { return i32(lw(h) & 0x7fffffffu); }"
  , "fn lshare(h: u32) { if (h > 0u && h < " ++ show arenaWords ++ "u) { arena[h] = arena[h] | 0x80000000u; } }"
  , "fn lgrowable(h: u32, w: u32) -> bool {"
  , "  return h > 0u && h < " ++ show arenaWords ++ "u && (arena[h] & 0x80000000u) == 0u && h + 1u + arena[h] * w == bump;"
  , "}"
  , "fn cpack(c: vec3u) -> u32 { return c.x | (c.y << 8u) | (c.z << 16u); }"
  , "fn cunpack(w: u32) -> vec3u { return vec3u(w & 255u, (w >> 8u) & 255u, (w >> 16u) & 255u); }"
  , "fn tbyte(b: u32) -> u32 { return (tables[b >> 2u] >> ((b & 3u) * 8u)) & 255u; }"
  -- an f64's bits (low word first) as R: its top 24 bits of significand,
  -- then the rest (rounded to 24 more)
  , "fn rf64(lo: u32, hi: u32) -> R {"
  , "  let e = i32((hi >> 20u) & 0x7ffu);"
  , "  let s = select(1.0, -1.0, (hi >> 31u) != 0u);"
  , "  if (e == 0) { return ri(0); }"
  , "  if (e == 0x7ff) { return rp(vec2f(select(s * finf(), fnan(), ((hi & 0xfffffu) | lo) != 0u), 0.0)); }"
  , "  let top = f32(0x800000u | ((hi & 0xfffffu) << 3u) | (lo >> 29u));"
  , "  let rest = f32(lo & 0x1fffffffu);"
  , "  return rpn(vec2f(s * ldexp(top, e - 1023 - 23), s * ldexp(rest, e - 1023 - 52)));"
  , "}"
  ] ++ case p of
    DoubleFloat ->
      [ "fn rpn(v: vec2f) -> R { return quick_two_sum(v.x, v.y); }"
      , "fn rw0(r: R) -> u32 { return bitcast<u32>(r.x); }"
      , "fn rw1(r: R) -> u32 { return bitcast<u32>(r.y); }" ]
    Single ->
      [ "fn rpn(v: vec2f) -> R { return v.x + v.y; }"
      , "fn rw0(r: R) -> u32 { return bitcast<u32>(r); }"
      , "fn rw1(r: R) -> u32 { return 0u; }" ]

-- | df64's exponential, logarithm, sine and cosine, arctangent and
-- hyperbolic functions: each reduces its argument (exactly, by constants
-- in three parts, with two_prod), then sums a polynomial by Horner's rule,
-- with df64 coefficients (no divisions), to about 1e-15 (see
-- dev-tools/gpu/df64-check.mjs).
df64Functions :: [String]
df64Functions =
  [ -- a - k c, for c in three parts: k p1 and k p2 exactly (two_prod)
    "fn rreduce(a: R, k: f32, p: vec3f) -> R {"
  , "  return rsub(rsub(rsub(a, two_prod(k, p.x)), two_prod(k, p.y)), R(g(k * p.z), 0.0));"
  , "}"
  , "const LN2 = " ++ dfc ln2 ++ ";"
  , "const LN2_3 = " ++ parts3 ln2 ++ ";"
  , "const PI = " ++ dfc piR ++ ";"
  , "const PIO2 = " ++ dfc (piR / 2) ++ ";"
  , "const PIO2_3 = " ++ parts3 (piR / 2) ++ ";"
  -- exp: e^a = 2^m e^r, r = a - m ln 2; e^(r/16) - 1 by its series, then
  -- squared back up (1 + s)^2 - 1 = 2 s + s^2, four times
  , "fn rexp(a: R) -> R {"
  , "  if (fisnan(a.x)) { return R(fnan(), 0.0); }"
  , "  if (a.x > 88.8) { return R(finf(), 0.0); }"
  , "  if (a.x < -104.0) { return R(0.0, 0.0); }"
  , "  if (a.x == 0.0) { return R(1.0, 0.0); }"
  , "  let m = round(a.x / LN2.x);"
  , "  let r = rscale(rreduce(a, m, LN2_3), -4);"
  , "  var s = rmul(r, " ++ hornerMixed "r" 0.0217 [ 1 / fact k | k <- [1 .. 8] ] ++ ");"
  , "  for (var k = 0; k < 4; k++) { s = radd(rscale(s, 1), rmul(s, s)); }"
  , "  return rscale(radd(s, R(1.0, 0.0)), i32(m));"
  , "}"
  -- log: a = m 2^e, m in [1/sqrt 2, sqrt 2]; log m = 2 atanh(s),
  -- s = (m - 1)/(m + 1), by its series in s^2
  , "fn rlog(a: R) -> R {"
  , "  if (a.x == 0.0) { return R(-finf(), 0.0); }"
  , "  if (a.x < 0.0 || fisnan(a.x)) { return R(fnan(), 0.0); }"
  , "  if (fisinf(a.x)) { return a; }"
  , "  var e = i32((bitcast<u32>(a.x) >> 23u) & 255u) - 127;"
  , "  var m = rscale(a, -e);"
  , "  if (m.x > 1.4142135) { m = rscale(m, -1); e += 1; }"
  , "  let s = rdiv(rsub(m, R(1.0, 0.0)), radd(m, R(1.0, 0.0)));"
  , "  let z = rmul(s, s);"
  , "  let lm = rmul(rscale(s, 1), " ++ hornerMixed "z" 0.0295 [ 1 / fromIntegral (2 * k + 1) | k <- [0 .. 11 :: Int] ] ++ ");"
  , "  let f = f32(e);"
  , "  return radd(radd(radd(two_prod(f, LN2_3.x), two_prod(f, LN2_3.y)), R(g(f * LN2_3.z), 0.0)), lm);"
  , "}"
  -- sin and cos: a = k pi/2 + r, |r| <= pi/4, and their series in r^2,
  -- by the quadrant
  , "fn rsincos(a: R) -> vec4f {"
  , "  let k = round(a.x / PIO2.x);"
  , "  let r = rreduce(a, k, PIO2_3);"
  , "  let z = rmul(r, r);"
  , "  let sn = rmul(r, " ++ hornerMixed "z" 0.617 [ (-1) ^ n / fact (2 * n + 1) | n <- [0 .. 8 :: Int] ] ++ ");"
  , "  let cs = " ++ hornerMixed "z" 0.617 [ (-1) ^ n / fact (2 * n) | n <- [0 .. 9 :: Int] ] ++ ";"
  , "  let q = i32(k - 4.0 * floor(k / 4.0));"
  , "  if (q == 0) { return vec4f(sn, cs); }"
  , "  if (q == 1) { return vec4f(cs, rneg(sn)); }"
  , "  if (q == 2) { return vec4f(rneg(sn), rneg(cs)); }"
  , "  return vec4f(rneg(cs), sn);"
  , "}"
  , "fn rsin(a: R) -> R { if (fisnan(a.x) || fisinf(a.x)) { return R(fnan(), 0.0); } return rsincos(a).xy; }"
  , "fn rcos(a: R) -> R { if (fisnan(a.x) || fisinf(a.x)) { return R(fnan(), 0.0); } return rsincos(a).zw; }"
  , "fn rtan(a: R) -> R { let sc = rsincos(a); return rdiv(sc.xy, sc.zw); }"
  -- atan2: t = min/max of |y| and |x|, in [0, 1]; atan t = atan c +
  -- atan u, c = k/8 nearest t (atan c from the table), u = (t - c)/(1 + t c),
  -- |u| <= 1/16, by its series; then by the octant
  , "fn ratan2(y: R, x: R) -> R {"
  , "  if (x.x == 0.0 && y.x == 0.0) { return R(0.0, 0.0); }"
  , "  if (fisnan(x.x) || fisnan(y.x)) { return R(fnan(), 0.0); }"
  , "  let ax = rabs(x);"
  , "  let ay = rabs(y);"
  , "  let swap = rlt(ax, ay);"
  , "  let t = rdiv(select(ay, ax, swap), select(ax, ay, swap));"
  , "  let k = round(t.x * 8.0);"
  , "  let c = R(k * 0.125, 0.0);"
  , "  let u = rdiv(rsub(t, c), radd(R(1.0, 0.0), rmul(t, c)));"
  , "  let z = rmul(u, u);"
  , "  var table = array<vec2f, 9>(" ++ intercalate ", " (map dfc atanTable) ++ ");"
  , "  var r = radd(table[clamp(i32(k), 0, 8)], rmul(u, " ++ hornerMixed "z" 0.0042 [ (-1) ^ k / fromIntegral (2 * k + 1) | k <- [0 .. 7 :: Int] ] ++ "));"
  , "  if (swap) { r = rsub(PIO2, r); }"
  , "  if (rlt(x, R(0.0, 0.0))) { r = rsub(PI, r); }"
  , "  if (rlt(y, R(0.0, 0.0))) { r = rneg(r); }"
  , "  return r;"
  , "}"
  -- (the hyperbolic functions: by a series near 0, where e^a - e^-a
  -- would cancel)
  , "fn rsinhs(a: R) -> R {"
  , "  let z = rmul(a, a);"
  , "  return rmul(a, " ++ hornerMixed "z" 0.0025 [ 1 / fact (2 * n + 1) | n <- [0 .. 5 :: Int] ] ++ ");"
  , "}"
  , "fn rsinh(a: R) -> R {"
  , "  if (abs(a.x) < 0.05) { return rsinhs(a); }"
  , "  let e = rexp(a);"
  , "  return rscale(rsub(e, rdiv(R(1.0, 0.0), e)), -1);"
  , "}"
  , "fn rcosh(a: R) -> R { let e = rexp(a); return rscale(radd(e, rdiv(R(1.0, 0.0), e)), -1); }"
  , "fn rtanh(a: R) -> R {"
  , "  if (a.x > 20.0) { return R(1.0, 0.0); }"
  , "  if (a.x < -20.0) { return R(-1.0, 0.0); }"
  , "  if (abs(a.x) < 0.05) { let s = rsinhs(a); return rdiv(s, rsqrt(radd(R(1.0, 0.0), rmul(s, s)))); }"
  , "  let e2 = rexp(rscale(a, 1));"
  , "  return rdiv(rsub(e2, R(1.0, 0.0)), radd(e2, R(1.0, 0.0)));"
  , "}"
  ]
  where
    fact :: Int -> Rational
    fact n = fromIntegral (product [1 .. n])

-- | A polynomial in z (WGSL, an R), its coefficients from the constant
-- term up, by Horner's rule, for |z| at most @zmax@: the terms after the first whose tail
-- (all the terms from it on) is below 1e-8 are summed in f32 (on z's high
-- part), where f32's error, under 1e-15, is less than df64's own; the rest
-- in df64.
hornerMixed :: String -> Double -> [Rational] -> String
hornerMixed z zmax cs =
  let tails = [ sum [ abs (fromRational c) * zmax ^ j | (j, c) <- drop k (zip [0 :: Int ..] cs) ] | k <- [0 .. length cs] ]
      k0 = max 1 (length (takeWhile (>= 1e-8) tails))
      (front, tailCs) = splitAt k0 cs
      tailExpr
        | null tailCs = Nothing
        | otherwise = Just (foldr (\c acc -> "(" ++ f32Lit c ++ " + " ++ z ++ ".x * " ++ acc ++ ")")
                                   (f32Lit (last tailCs)) (init tailCs))
      -- (the last df64 term picks the f32 tail up: c + z tail)
      start = case tailExpr of
        Nothing -> dfc (last front)
        Just t -> "radd(" ++ dfc (last front) ++ ", rmul(" ++ z ++ ", R(" ++ t ++ ", 0.0)))"
  in foldr (\c acc -> "radd(" ++ dfc c ++ ", rmul(" ++ z ++ ", " ++ acc ++ "))") start (init front)

f32Lit :: Rational -> String
f32Lit c = "bitcast<f32>(" ++ f32Bits (fromRational c) ++ ")"

-- | A constant as df64: the f32 nearest it, and the f32 nearest the rest.
dfc :: Rational -> String
dfc r = let hi = fromRational r :: Float
            lo = fromRational (r - toRational hi) :: Float
        in "R(bitcast<f32>(" ++ f32Bits hi ++ "), bitcast<f32>(" ++ f32Bits lo ++ "))"

-- | A constant in three f32 parts (for 'rreduce').
parts3 :: Rational -> String
parts3 r = let p1 = fromRational r :: Float
               p2 = fromRational (r - toRational p1) :: Float
               p3 = fromRational (r - toRational p1 - toRational p2) :: Float
           in "vec3f(bitcast<f32>(" ++ f32Bits p1 ++ "), bitcast<f32>(" ++ f32Bits p2 ++ "), bitcast<f32>(" ++ f32Bits p3 ++ "))"

f32Bits :: Float -> String
f32Bits x = "0x" ++ showHex (castFloatToWord32 x) "u"

-- | A decimal number, exactly.
decimal :: String -> Rational
decimal s = case break (== '.') s of
  (whole, '.' : frac) -> fromInteger (read (whole ++ frac)) / 10 ^ length frac
  _ -> fromInteger (read s)

-- (to 40 digits: pi, ln 2, and atan (k/8), k = 0 .. 8)
piR, ln2 :: Rational
piR = decimal "3.1415926535897932384626433832795028841972"
ln2 = decimal "0.6931471805599453094172321214581765680755"

atanTable :: [Rational]
atanTable = 0 : map decimal
  [ "0.1243549945467614350313548491638710255732", "0.2449786631268641541720824812112758109141"
  , "0.3587706702705722203959200639264604997770", "0.4636476090008061162142562314612144020285"
  , "0.5585993153435624359715082164016612703464", "0.6435011087932843868028092287173226380415"
  , "0.7188299996216245054170141515259046539514" ] ++ [piR / 4]
