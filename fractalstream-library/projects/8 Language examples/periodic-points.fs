format: 1
script: |
  # The points of period n of z² + C are the solutions of fⁿ(z) = z. Here
  # `compose(f, n, w)` is f applied n times, and the standard library's
  # `newton_root` runs Newton's method on fⁿ(w) - w from each pixel, which
  # needs the derivative of fⁿ: `newton_root` takes it with `diff`, through
  # `compose`'s loop. Each pixel is colored by the root it finds (a color
  # of its own for each, by `autocolor`), so the picture is Newton's
  # basins for the period-n points.
  #
  # `f ∘ f` is f composed with itself, a function you can call: the
  # second viewer shows (f ∘ f)(z) - z, whose zeros (where all the colors
  # meet) are the points of period 1 and 2.
  #
  # Click a point with the "Cycle" tool to run Newton's method from there
  # and draw the cycle it finds, with its multiplier (fⁿ)'.

  @config "Parameters"
  C : C as "f(z) = z² +" := -0.12 + 0.75i
  n : Z as "Period n =" := 3

  @common
  f(z) := z^2 + C
  # f applied n times
  function fn(w):
    fn <- compose(f, n, w)

  @viewer "Newton's method for fⁿ(z) = z"
  setup:
    pixel size: 1/150
  (found, root) := newton_root(w -> fn(w) - w, z)
  color <- if found then autocolor(0, root) else black

  @viewer "(f ∘ f)(z) - z"
  setup:
    pixel size: 1/200
  color <- domain_color((f ∘ f)(z) - z)

  @tool "Cycle" for "Newton's method for fⁿ(z) = z" with shortcut c
  text "Click to find a point of period n (or dividing n) by Newton's method from there, and draw its cycle."
  multiplier : C as "Multiplier (fⁿ)' =" := 0
  on click:
    erase
    (found, w) := newton_root(u -> fn(u) - u, click)
    if found:
      multiplier <- fn'(w)
      use white for stroke
      use white for fill
      repeat n times:
        draw filled circle at w with radius 4 pixels
        w <- f(w)
        draw line from previous w to w
  on deactivated:
    erase
metadata:
  title: Periodic points by Newton's method
  description: |
    Newton's method finds the points of period n of z² + C, as the roots of
    fⁿ(z) - z, where fⁿ is `compose(f, n, w)` and `newton_root` takes its
    derivative automatically; each pixel is colored by the root it finds.
    A second viewer draws (f ∘ f)(z) - z, using the composition `f ∘ f` as
    a function of its own. The "Cycle" tool draws the cycle through a
    point, with its multiplier.
  preview-commands: |
    select viewer "Newton's method for fⁿ(z) = z"
    capture viewer "Newton's method for fⁿ(z) = z" as preview
  preview: |
    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
autocolor:
  '0':
    tolerance: 1.0e-3
    locked: false
    next-hue: 3.0572738853503187
    entries:
    - point: -1.2480877405 + 0.665447491i
      color: rgb(1.000, 0.878, 0.000)
    - point: -0.6680173145 + 0.5698205046i
      color: rgb(1.000, 0.000, 0.000)
    - point: -0.2766277683 + 0.4828568013i
      color: rgb(0.000, 1.000, 0.294)
    - point: -0.1201252807 + 0.7499649323i
      color: rgb(0.000, 0.827, 1.000)
    - point: 0.0015517258 - 0.011299927i
      color: rgb(0.584, 0.000, 1.000)
    - point: 0.0397759656 - 1.0628592889i
      color: rgb(0.051, 0.000, 1.000)
    - point: 0.9949026446 - 0.9110737109i
      color: rgb(0.243, 1.000, 0.000)
    - point: 1.2766277683 - 0.4828568012i
      color: rgb(1.000, 0.000, 0.533)
