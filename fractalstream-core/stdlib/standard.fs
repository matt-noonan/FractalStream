# FractalStream's standard library.
#
# These definitions are part of every project, as if they were in its
# @common stanza: each viewer and tool gets the ones it uses. A project
# can define any of these names for itself, and then its own definition is
# the one it gets, wherever it defines it. The definitions here always use
# each other, never a project's definitions of the same names.
#
# Definitions here are functions, either `f(x) := ...` on one line, or
# `function f(x):` with a body. As in a script, a definition can only use
# the ones before it, and a function with a body can only be called as the
# whole right-hand side of an assignment (`c := f(x)`); configuration
# values can't call one at all.
#
# Be cautious about changing what a definition here does: projects that
# already use it would change too. Adding new definitions is safe. Never
# turn a `f(x) := ...` function into a function with a body, which would
# break every project that calls it inside an expression. (The other way
# around is safe.)
#
# (Names starting with `internal_` stand for values a script can't name,
# such as how the viewer shows its points. They're only for this file.)

# ---------------------------------------------------------------------------
# Other names for built-in functions

asin(x) := arcsin(x)
acos(x) := arccos(x)
atan(x) := arctan(x)
asinh(x) := arcsinh(x)
acosh(x) := arccosh(x)
atanh(x) := arctanh(x)

Re(z) := re(z)
Im(z) := im(z)
Arg(z) := arg(z)
Conj(z) := conj(z)
bar(z) := conj(z)
Bar(z) := conj(z)

ceil(x) := ceiling(x)

# ---------------------------------------------------------------------------
# Colors

# A color from its red, green, and blue components, each from 0 to 255.
rgb255(r, g, b) := rgb(r / 255, g / 255, b / 255)

# A darker or lighter shade of a color.
dark(c) := blend(0.333, c, black)
light(c) := blend(0.333, c, white)

# Back and forth between two colors: c1 at whole numbers, c2 halfway
# between them.
cycle_at(s, c1, c2) := blend((if s < 0.5 then 2 s else 2 - 2 s), c1, c2)
cycle(t, c1, c2) := cycle_at(mod(t, 1.0), c1, c2)

# A color wheel through eight colors: c1 at whole numbers, blending into c2
# at 1/8, c3 at 2/8, and so on, then back to c1. Its helpers take u from
# 0 to 1, and the part of the wheel from lo to lo + 1/4 or 1/8.
wheel2_at(u, lo, c1, c2, c3) := if u < lo + 0.125 then blend(8 (u - lo), c1, c2) else blend(8 (u - (lo + 0.125)), c2, c3)
wheel4_at(u, lo, c1, c2, c3, c4, c5) := if u < lo + 0.25 then wheel2_at(u, lo, c1, c2, c3) else wheel2_at(u, lo + 0.25, c3, c4, c5)
wheel8_at(u, c1, c2, c3, c4, c5, c6, c7, c8) := if u < 0.5 then wheel4_at(u, 0.0, c1, c2, c3, c4, c5) else wheel4_at(u, 0.5, c5, c6, c7, c8, c1)
wheel8(t, c1, c2, c3, c4, c5, c6, c7, c8) := wheel8_at(mod(t, 1.0), c1, c2, c3, c4, c5, c6, c7, c8)

# Color wheels, each with period 1.
rainbow(t) := wheel8(t, red, orange, yellow, green, rgb255(0, 128, 128), blue, purple, violet)

# Sampled from 8 points in the Arenberg phase wheel
# (https://eprovst.github.io/DomainColoring.jl/stable/arenberg/#The-Arenberg-Phase-Wheel)
# and converted to sRGB using D50 at 2 degrees
arenberg(t) := wheel8(t, rgb255(255, 80, 81), rgb255(255, 174, 0), rgb255(152, 190, 0), rgb255(0, 183, 74), rgb255(0, 237, 232), rgb255(0, 172, 255), rgb255(184, 161, 255), rgb255(255, 134, 226))

# Cyclic color maps from https://s-ink.org/scientific-colour-maps
romao(t) := wheel8(t, rgb255(201, 218, 169), rgb255(196, 163, 72), rgb255(164, 103, 43), rgb255(137, 64, 53), rgb255(116, 57, 88), rgb255(93, 84, 140), rgb255(81, 135, 185), rgb255(136, 197, 201))
bamo(t) := wheel8(t, rgb255(208, 200, 189), rgb255(212, 162, 196), rgb255(178, 110, 160), rgb255(118, 59, 102), rgb255(78, 45, 63), rgb255(79, 69, 59), rgb255(105, 110, 75), rgb255(177, 186, 150))
broco(t) := wheel8(t, rgb255(201, 203, 179), rgb255(139, 162, 190), rgb255(89, 115, 155), rgb255(57, 65, 100), rgb255(57, 46, 55), rgb255(69, 63, 41), rgb255(109, 105, 60), rgb255(166, 165, 118))
corko(t) := wheel8(t, rgb255(165, 196, 173), rgb255(134, 157, 185), rgb255(86, 111, 148), rgb255(64, 68, 93), rgb255(64, 62, 57), rgb255(69, 79, 43), rgb255(79, 115, 58), rgb255(117, 164, 111))
viko(t) := wheel8(t, rgb255(210, 179, 161), rgb255(119, 157, 187), rgb255(61, 104, 151), rgb255(60, 53, 103), rgb255(80, 25, 59), rgb255(105, 24, 34), rgb255(150, 62, 35), rgb255(203, 135, 99))

# Gradients there and back again, through five colors: c1 .. c5 .. c2
ice(t) := wheel8(t, rgb255(30, 203, 255), rgb255(95, 216, 251), rgb255(137, 227, 249), rgb255(173, 239, 249), rgb255(207, 249, 252), rgb255(173, 239, 249), rgb255(137, 227, 249), rgb255(95, 216, 251))
fire(t) := wheel8(t, rgb255(255, 61, 61), rgb255(255, 123, 57), rgb255(255, 171, 71), rgb255(255, 212, 103), rgb255(255, 250, 148), rgb255(255, 212, 103), rgb255(255, 171, 71), rgb255(255, 123, 57))
wheat(t) := wheel8(t, rgb255(0, 179, 255), rgb255(102, 175, 229), rgb255(138, 172, 203), rgb255(243, 215, 164), rgb255(237, 242, 0), rgb255(243, 215, 164), rgb255(138, 172, 203), rgb255(102, 175, 229))
rose(t) := wheel8(t, rgb255(255, 0, 153), rgb255(255, 88, 98), rgb255(247, 146, 65), rgb255(213, 191, 77), rgb255(179, 226, 131), rgb255(213, 191, 77), rgb255(247, 146, 65), rgb255(255, 88, 98))
forest(t) := wheel8(t, rgb255(12, 73, 0), rgb255(66, 87, 18), rgb255(103, 100, 44), rgb255(134, 114, 74), rgb255(159, 131, 107), rgb255(134, 114, 74), rgb255(103, 100, 44), rgb255(66, 87, 18))
ocean(t) := wheel8(t, rgb255(10, 0, 73), rgb255(0, 52, 106), rgb255(4, 94, 125), rgb255(84, 135, 140), rgb255(152, 173, 167), rgb255(84, 135, 140), rgb255(4, 94, 125), rgb255(0, 52, 106))
winter(t) := wheel8(t, rgb255(247, 250, 255), rgb255(195, 203, 223), rgb255(148, 157, 192), rgb255(107, 113, 160), rgb255(70, 70, 128), rgb255(107, 113, 160), rgb255(148, 157, 192), rgb255(195, 203, 223))
spring(t) := wheel8(t, rgb255(103, 223, 130), rgb255(136, 207, 87), rgb255(164, 188, 49), rgb255(189, 167, 14), rgb255(211, 142, 8), rgb255(189, 167, 14), rgb255(164, 188, 49), rgb255(136, 207, 87))
summer(t) := wheel8(t, rgb255(48, 92, 206), rgb255(0, 135, 180), rgb255(0, 161, 177), rgb255(0, 193, 149), rgb255(35, 227, 0), rgb255(0, 193, 149), rgb255(0, 161, 177), rgb255(0, 135, 180))
fall(t) := wheel8(t, rgb255(128, 55, 3), rgb255(158, 99, 26), rgb255(186, 143, 55), rgb255(212, 189, 90), rgb255(235, 236, 131), rgb255(212, 189, 90), rgb255(186, 143, 55), rgb255(158, 99, 26))

# ---------------------------------------------------------------------------
# The view

# In the hyperbolic plane, the screen-to-model map is (a s + b)/(c s + d),
# of determinant 1, and it stretches a pixel at the screen point s that
# shows p by |a - c p|².
mobius_stretch(a, c, p) := |a - c p|^2
hyperbolic_stretch_at(p) := mobius_stretch(internal_view_a_re + internal_view_a_im i, internal_view_c_re + internal_view_c_im i, p)

# How much the view stretches a screen pixel at the point p: not at all on
# the plane; on the sphere, by the stereographic projection at p (for a
# pixel facing the viewer: near the globe's rim, pixels are longer one
# way).
view_stretch_at(p) := if internal_view_kind = 2 then hyperbolic_stretch_at(p) else if internal_view_kind = 1 then (1 + |p|^2) / 2 else 1.0

# The width, in the model, of a screen pixel at the point p, so that a tool
# can size a marker at p to look the same anywhere in the view.
pixel_size_at(p) := internal_view_pixel_size view_stretch_at(p)

# ---------------------------------------------------------------------------
# Iterating a function

# f applied N times, starting from x: compose(f, 3, x) is f(f(f(x))).
function compose(f, N, x):
  repeat N times:
    x <- f(x)
  compose <- x

# ---------------------------------------------------------------------------
# Newton's method

function full_newton_root(f, x, N, stop):
  repeat up to N times:
    y := f(x)
    x <- x - y / (ⅆy/ⅆx)
  until stop(x, previous x)
  full_newton_root <- not stuck, x

function newton_root(f, x):
  newton_root <- full_newton_root(f, x, max_iters, (x,y) -> x = y)

# ---------------------------------------------------------------------------
# Schwarzian derivative

function S(f, x):
  y := f(x)
  S <- diff(y, x, 3) / diff(y, x) - (3/2) (diff(y, x, 2) / diff(y, x))^2

# ---------------------------------------------------------------------------
# min / max / clamp

min(x, y) := if x < y then x else y

max(x, y) := if x > y then x else y

clamp(x, lo, hi) := if x < lo then lo else if x > hi then hi else x

# ---------------------------------------------------------------------------
# Floyd's tortoise and hare algorithm

function find_cycle(f, z, warmup, max_period, same, χ):

  find_cycle <- false, 0, 0

  iterate z -> f(z) while χ(z) up to warmup times

  if χ(z):
    tortoise := f(z)
    hare := f(tortoise)

    # The tortoise and the hare meet after a multiple of the period that
    # is at least the steps before the cycle starts: so a cycle whose
    # period and lead-in (after the warmup) both fit in max_period steps
    # is found, and the two loops after this one need no more steps.
    while χ(tortoise) and χ(hare) and not same(tortoise, hare) up to max_period times:
      tortoise <- f(tortoise)
      hare <- f(f(hare))

    # (only if they met: if the loop ran out of steps first, there's no
    # cycle to find)
    if χ(tortoise) and χ(hare) and same(tortoise, hare):
      # where the cycle starts
      tortoise <- z
      while not same(tortoise, hare) up to max_period times:
        tortoise <- f(tortoise)
        hare <- f(hare)

      # its period
      λ := 1
      hare <- f(tortoise)
      while not same(tortoise, hare) up to max_period times:
        hare, λ <- f(hare), λ + 1

      if same(tortoise, hare):
        find_cycle <- true, hare, λ

# ---------------------------------------------------------------------------
# Integrals, residue, winding number

function riemann_sum(γ, f, N):
  riemann_sum <- 0
  t, dt := 0, 1/N
  γt, next_γt := γ(0), γ(dt)
  repeat N times:
    riemann_sum <- riemann_sum + f(γt) (next_γt - γt)
    t <- t + dt
    γt, next_γt <- next_γt, γ(t)

function residue(f, z, r, N):
  residue <- riemann_sum(t -> z + r e^{2 π i t}, f, N) / (2 π i)

function winding_number(γ, z0, N):
  winding_number <- round (im riemann_sum(γ, z -> 1 / (z - z0), N) / (2 π))

# ---------------------------------------------------------------------------
# Coloring the plane

domain_color(z) ≔ hsl((arg(z) / (2 π)) + 1/3, 1, |z|² / (1 + |z|²))
