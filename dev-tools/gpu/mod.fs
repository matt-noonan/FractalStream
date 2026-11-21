# Euclidean // and mod, and round, on every backend (check by hand: every
# pixel green, with fsrun's -b cpu, gpu-df64 and gpu-f32, the wasm build,
# and dev-tools/web/check-gpu.mjs). mod(a, b) is in [0, |b|), with
# a = b (a // b) + mod(a, b), and dividing by 0 gives 0 both ways; round
# takes a half away from zero. The arguments come from z, so that nothing
# is worked out before the render.
format: 1
script: |
  @viewer "Euclidean mod" var=z center=0 pixelSize=1/20
  # (0 at every pixel, but not known to be before the render)
  s := if re z > 1000000 then 1 else 0
  ok := true
  # integers, of both signs, from the pixel
  a := round(4 re z)
  b := round(2 im z)
  q := a // b
  r := mod(a, b)
  ok <- ok and (if b = 0 then q = 0 and r = 0 else 0 <= r and r < |b| and b q + r = a)
  # some by hand
  ok <- ok and mod(-7 + s, 3) = 2 and mod(7 + s, -3) = 1 and mod(-7 + s, -3) = 2
  ok <- ok and (-7 + s) // 3 = -3 and (7 + s) // -3 = -2 and (-7 + s) // -3 = 3
  ok <- ok and mod(7 + s, 0) = 0 and (7 + s) // 0 = 0
  ok <- ok and mod(5 + s, -1) = 0 and (5 + s) // -1 = -5
  ok <- ok and mod(-1 + s, -2147483647 - 1) = 2147483647
  # reals, from the pixel
  x := 7 re z
  y := 3 im z
  m := mod(x, y)
  k := (x - m) / y
  ok <- ok and (if |y| < 0.01 then true else 0 <= m and m < |y| and |k - floor(k + 0.5)| < 0.001)
  # and by hand: a negative divisor, a remainder that can round to |b| (0
  # in doubles; just under 3 in double-float, which can hold 3 - 1/10^20),
  # and 0
  ok <- ok and |mod(-1 + s, -3.0) - 2| < 0.000001 and |mod(7.5 + s, -2.0) - 1.5| < 0.000001
  tiny := mod(s - 1 / 10^20, 3.0)
  ok <- ok and 0 <= tiny and tiny < 3 and mod(2.5 + s, 0.0) = 0
  # round: the nearest, a half away from zero
  ok <- ok and round(2.5 + s) = 3 and round(-2.5 + s) = -3 and round(0.5 + s) = 1
  ok <- ok and round(-0.5 + s) = -1 and round(1.5 + s) = 2 and round(2.4999 + s) = 2
  ok <- ok and round(2.9999 + s) = 3 and round(-2.9999 + s) = -3 and round(s - 0.4) = 0
  # and from the pixel: within a half
  h := round(7.3 re z)
  ok <- ok and |7.3 re z - h| <= 0.5
  color <- if ok then green else red
