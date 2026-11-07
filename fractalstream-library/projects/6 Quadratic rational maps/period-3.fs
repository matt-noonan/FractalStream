script: |
  # Quadratic rational maps with a periodic critical point: period 3.
  #
  # A quadratic rational map has two critical points, and one of them can be
  # put on a cycle of period n.  Up to conjugacy such maps form a curve in
  # the two-dimensional moduli space of quadratic rational maps -- Milnor's
  # Per_n(0) -- so each n gives a one-parameter family.  This project draws
  # Per_3(0).
  #
  # Parametrization.  Dynamo's normal form is
  #
  #     f(z) = (z^2 + c^3 - c - 1)/(z^2 - c^2),
  #
  # whose critical cycle is oo -> 1 -> -c -> oo; the free critical point is 0.
  # Milnor draws the same curve in the log c plane, for the form
  # z -> 1 - (1+c)/z + c/z^2.
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
  c : C as "c" := 0.45 + 1.0i

  @config "Coloring"
  period : R as "Color period (iterations)" := 10
  iters : Z as "Iteration limit" := 500
  periods : Boolean as "Colour components by period" := false

  @common
  f(z) := (z^2 + c^3 - c - 1)/(z^2 - c^2)

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

  @viewer "Parameter plane" var=c center=0.45 pixelSize=1/150 limit=iters
  z := 0
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
  title: 'Period 3: parameter plane and Julia set'
  description: |
    Period 3 case of the curves Per_n(0): quadratic rational maps one of
    whose critical points is periodic of period 3, in Dynamo's normal form
    f(z) = (z^2 + c^3 - c - 1)/(z^2 - c^2).  The critical cycle is
    oo -> 1 -> -c -> oo and the free critical point is 0.  One window is the
    parameter plane, the other the Julia set of the current parameter.
  preview-commands: |
    capture viewer "Parameter plane" as preview
  preview: |
    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
