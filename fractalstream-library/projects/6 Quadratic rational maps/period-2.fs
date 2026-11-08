script: |
  # Quadratic rational maps with a periodic critical point: period 2.
  #
  # A quadratic rational map has two critical points, and one of them can be
  # put on a cycle of period n.  Up to conjugacy such maps form a curve in
  # the two-dimensional moduli space of quadratic rational maps -- Milnor's
  # Per_n(0) -- so each n gives a one-parameter family.  This project draws
  # Per_2(0).
  #
  # Parametrization.  Dynamo's normal form is
  #
  #     f(z) = (z^2 + c)/(1 - z^2),
  #
  # whose critical points are 0 and oo.  oo <-> -1 is the critical 2-cycle
  # and 0 is the free critical point, so the parameter plane reports the fate
  # of the orbit of 0.  Milnor's coordinate for the same curve is
  # f_a(z) = a/z + 1/z^2, drawn in the -a^3 plane; M(z) = -2/(a z) - 1
  # conjugates it to the form above with c = 8/a^3 - 1.
  #
  # The third window draws the curve in Milnor's coordinate, inverted:
  # f(z) = 1/(b z) + 1/z^2 with b = 1/a, free critical point -2b.  The family
  # is unchanged by a -> w a for w a cube root of unity (conjugate by
  # z -> w^2 z), so only a^3 is a modulus.  In the b-plane that symmetry is a
  # rotation by 120 degrees about the origin, with the three copies of the
  # c-plane around it at |b| about 0.5; the c-plane is the quotient, so it
  # shows one copy and no symmetry.  The b-plane uses an escape radius of 1e4
  # rather than 10: b = 0 is the degenerate limit a = oo, and at radius 10
  # every parameter within 1/(2 sqrt 10) of it escapes at the first step, in
  # one flat colour -- a disc in the middle of the picture.
  #
  # What the windows show
  #
  #  * Black is every point whose free critical point does not reach a pole
  #    within the iteration limit: in the parameter plane the Mandelbrot-like
  #    set, in the dynamical plane the filled Julia set.
  #  * Everything else is coloured by how long the escape took, one rainbow
  #    band per "Color period" iterations.
  #  * "Colour components by period" (Coloring tab) paints the components
  #    whose orbit lands on some other attracting cycle in a pale colour of
  #    its period instead of black.  The test looks for periods up to 64;
  #    longer or very weakly attracting cycles stay black.
  #  * Clicking the parameter plane picks the parameter and the Julia window
  #    follows; "Trace orbit" draws the orbit of a point of a Julia set.
  # References
  #
  #  * J. Milnor, "Geometry and dynamics of quadratic rational maps",
  #    Experiment. Math. 2 (1993) 37-83; doi:10.1080/10586458.1993.10504267.
  #    The curves Per_n(0) and the moduli space of quadratic rational maps.
  #  * J. Milnor, "Remarks on quadratic rational maps", arXiv:math/9209221.
  #    Section 9 draws Per_2(0) in the -a^3 plane (Figure 7) and Per_3(0) in
  #    the log c plane (Figure 9), and describes Per_4(0) by the cross-ratio
  #    of its critical cycle.
  #  * Dynamo, Danny Stoll's program, https://github.com/DannyStoll1/dynamo
  #    (GPL-3): the normal forms used here are the ones in its
  #    crates/profiles/src/rational_maps/quad_rat_per_{2,3,4,5}.rs.  Only the
  #    formulas are taken from it, restated in FractalStream's language.
  #  * D. Gage and D. Jackson, "Computer generated images for quadratic
  #    rational maps with a periodic critical point", arXiv:1009.3478: the
  #    convention that the parameters whose free critical point is not
  #    attracted to the critical cycle are black, and a proof that for n = 5
  #    the parametrizing curve has geometric genus 1.

  @config "Parameters"
  c : C as "c" := 0.3 + 0.6i
  b : C as "b = 1/a (Milnor's plane)" := 0.45 + 0.25i
  esc : R as "Escape radius (1/a plane)" := 10000

  @config "Coloring"
  period : R as "Color period (iterations)" := 10
  iters : Z as "Iteration limit" := 500
  periods : Boolean as "Colour components by period" := false

  @common
  f(z) := (z^2 + c)/(1 - z^2)

  # Milnor writes this family as a/z + 1/z^2; in b = 1/a it is 1/(b z) +
  # 1/z^2, whose free critical point is -2b.  The family is invariant under
  # a -> w a for w a cube root of unity, so the b-plane has a three-fold
  # symmetry about the origin, a point of view on the same curve that the
  # c-plane quotients away (see results/quadratic-rational-per-n.md).
  g(z) := 1/(b z) + 1/z^2

  # Black is everything whose free critical point does not reach the pole:
  # in the parameter plane that is the Mandelbrot-like set, in the dynamical
  # plane it is the filled Julia set.  The rest is coloured by how long the
  # orbit took to be thrown out.  With "Colour components by period" ticked,
  # the components that land on some other attracting cycle are painted in a
  # pale colour of its period instead.
  define shadeP:
    if z escaped:
      color <- rainbow(iterations / period)
    else if periods:
      w := z
      repeat up to 64 times:
        w <- f(w)
      until w = z
      color <- if stuck then black else blend(0.5, rainbow(iterations / 8), white)
    else:
      color <- black

  # The same curve in Milnor's coordinate, inverted.  The escape radius is
  # 1e4 rather than 10 because b = 0 is the degenerate limit a = oo: at a
  # radius of 10 every parameter within 1/(2 sqrt 10) of it escapes at the
  # first step in one flat colour, a disc in the middle of the picture.
  define shadeM:
    if z escaped:
      color <- rainbow(iterations / period)
    else if periods:
      w := z
      repeat up to 64 times:
        w <- g(w)
      until w = z
      color <- if stuck then black else blend(0.5, rainbow(iterations / 8), white)
    else:
      color <- black

  @viewer "Parameter plane" var=c center=0.1 pixelSize=1/150 limit=iters
  z := 0
  iterate z -> f(z) until z escapes
  run shadeP

  @viewer "Julia set" center=0 pixelSize=1/150 limit=iters
  iterate z -> f(z) until z escapes
  run shadeP

  @viewer "Plane in Milnor's 1/a (Z/3)" var=b center=0 pixelSize=1/300 limit=iters escape=esc
  z := -2 b
  iterate z -> g(z) until z escapes
  run shadeM

  # Clicking the plane picks c (it writes the clicked point into the config
  # variable, and the Julia window follows).
  @tool "Pick c" for="Parameter plane" shortcut=p
  on click-or-drag:
    pass

  @tool overlay for="Parameter plane"
  on refresh:
    erase
    use white for stroke
    draw circle at c with radius 3 pixel_size

  @tool "Trace orbit" for="Julia set" shortcut=t
  on click at w:
    erase
    use yellow for stroke
    prev := w
    repeat up to 200 times:
      draw point at w
      prev <- w
      w <- f(w)
      draw line from prev to w
    until w escapes
  on deactivated:
    erase
metadata:
  title: 'Period 2: parameter plane, Julia set, and Milnor''s 1/a'
  description: |
    Period 2 case of the curves Per_n(0): quadratic rational maps one of
    whose critical points is periodic of period 2, in Dynamo's normal form
    f(z) = (z^2 + c)/(1 - z^2).  One window is the parameter plane, one the
    Julia set of the current parameter, and one the same curve in Milnor's
    coordinate z -> a/z + 1/z^2, drawn in the inverted plane b = 1/a, where
    the family's invariance under a -> w a (w a cube root of unity) shows as
    a three-fold symmetry.
  preview-commands: |
    capture viewer "Parameter plane" as preview
  preview: |
    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
