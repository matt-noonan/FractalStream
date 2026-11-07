script: |
  # Quadratic rational maps with a periodic critical point: period 4.
  #
  # A quadratic rational map has two critical points, and one of them can be
  # put on a cycle of period n.  Up to conjugacy such maps form a curve in
  # the two-dimensional moduli space of quadratic rational maps -- Milnor's
  # Per_n(0) -- so each n gives a one-parameter family.  This project draws
  # Per_4(0).
  #
  # Parametrization.  Dynamo's normal form is
  #
  #     f(z) = (z - c)(z(c - 1) - 2c + 1)/(z^2 (c - 1)),
  #
  # whose critical cycle is 0 -> oo -> 1 -> c -> 0.  Here 0 is on the cycle
  # (it is the double pole), so the free critical point is the other one,
  # 2(2c^2 - c)/(c^2 + c - 1), and that is what the parameter plane follows.
  # Milnor describes the same curve by the cross-ratio of the critical cycle.
  # The family is a channel -- much of the plane is captured at the first
  # step -- so the window is centred on a stretch of the hyperbolic
  # components rather than on the origin.
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
  c : C as "c" := -0.45 + 0.2i

  @config "Coloring"
  period : R as "Color period (iterations)" := 10
  iters : Z as "Iteration limit" := 500
  periods : Boolean as "Colour components by period" := false

  @common
  f(z) := (z - c)*(z*(c - 1) - 2*c + 1)/(z^2*(c - 1))

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

  @viewer "Parameter plane" var=c center=-0.8 pixelSize=1/300 limit=iters
  z := 2*(2 c^2 - c)/(c^2 + c - 1)
  iterate z -> f(z) until z escapes
  run shadeP

  @viewer "Julia set" center=0 pixelSize=1/150 limit=iters
  iterate z -> f(z) until z escapes
  run shadeP

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
  title: 'Period 4: parameter plane and Julia set'
  description: |
    Period 4 case of the curves Per_n(0): quadratic rational maps one of
    whose critical points is periodic of period 4, in Dynamo's normal form
    f(z) = (z - c)(z(c - 1) - 2c + 1)/(z^2 (c - 1)).  The critical cycle is
    0 -> oo -> 1 -> c -> 0; 0 is on it (the double pole), so the free
    critical point is 2(2c^2 - c)/(c^2 + c - 1).  One window is the parameter
    plane, the other the Julia set of the current parameter.
  preview-commands: |
    capture viewer "Parameter plane" as preview
  preview: |
    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
    0/n/pSfzUJ03fwgAAAAASUVORK5CYII=
