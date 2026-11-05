script: |
  @common

  f(z) := z^2 + C

  # one step of f, followed by a Dehn twist in the annulus around
  # twist_center with radii r0 and r1
  define twist_step:
    z ⭠ f(z)
    d ≔ |z - twist_center|
    if d > r0 and d < r1:
      theta ≔ 2π (d - r0) / (r1 - r0)
      z ⭠ e^(i theta) (z - twist_center) + twist_center

  # show the twist's center and annulus
  define draw_twist:
    use red for stroke
    draw point at twist_center
    use white for stroke
    draw circle at twist_center with radius r0
    draw circle at twist_center with radius r1

  @config "Configuration"
  C : C as "Selected value of C:" := -0.1226 + 0.7449i
  drag_target : Z as "Dehn tool drag target: " := 0

  @config "Dehn twist"
  twist_center : C as "Center of twist:" := -0.267 + 0.478i
  row:
    r0 : R as "Inner radius" := 0.2
    r1 : R as "Outer radius" := 0.5

  @viewer "Parameter space" var=C colorizer="Binary decomposition"
  z ≔ 0
  until z escapes:
    run twist_step

  @viewer "Twisted dynamical plane" colorizer="Binary decomposition"
  until z escapes:
    run twist_step

  @tool "Trace" for="Twisted dynamical plane" shortcut=t
  on deactivated:
    erase
  on click-or-drag:
    prevZ ≔ z
    erase
    until z escapes up to 100 times:
      prevZ ⭠ z
      draw point at z
      run twist_step
      draw line from z to prevZ

  @tool "Dehn twist" for="Twisted dynamical plane" shortcut=d
  on activated:
    run draw_twist
  on deactivated:
    erase
  on drag-finished:
    drag_target ⭠ 0
  on drag from z0 to z:
    eps ≔ 5 pixel_size
    if drag_target = 0:
      if |z0 - twist_center| < eps:
        drag_target ⭠ 1
      else if |(|z0 - twist_center|) - r0| < eps:
        drag_target ⭠ 2
      else if |(|z0 - twist_center|) - r1| < eps:
        drag_target ⭠ 3

    if drag_target = 1:
      twist_center ⭠ z
    if drag_target = 2:
      r0 ⭠ |z - twist_center|
    if drag_target = 3:
      r1 ⭠ |z - twist_center|
    if drag_target > 0:
      erase
      run draw_twist
metadata:
  title: A twisted Mandelbrot set
  description: |
    What happens to the Mandelbrot and Julia sets of z ↦ z² + C when each
    step is followed by a twist in an annulus? Choose the annulus in the
    configuration window, or drag it with the Dehn twist tool (d), and watch
    both planes deform.
  preview-commands: |
    capture viewer "Parameter space" as preview
  preview: |
    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
    po9BtzFyDP1fpIRZ/nJaLQQAAAAASUVORK5CYII=
wx-properties:
  viewers:
    Twisted dynamical plane:
      x: 620
      'y': 104
