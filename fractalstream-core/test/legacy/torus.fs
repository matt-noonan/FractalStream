# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue
  out_of_bounds_color : Color as "Out of Bounds" := red
  lower_basin_color : Color as "Lower Basin" := green
  upper_basin_color : Color as "Upper Basin" := orange

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  define script(limit):
    flag_color <- out_of_bounds_color
    if y < 1 and y > 0 and x < pi and x > -pi:
      repeat up to limit times:
        x, y <- 2 * arctan(sin(2 * x) / (cos(2 * x) + y^2)), (cos(2 * x) + 1) / (cos(2 * x) + y^2 / 2 + 1 / (2 * y^2))
        # (reduce x mod pi: the remainder, with x's sign)
        x <- x - pi * (if x < 0 then ceil(x / pi) else floor(x / pi))
      until y = 0 or y = 1
      if y = 0:
        flag_color <- lower_basin_color
      else:
        flag_color <- upper_basin_color

  @viewer "Dynamical plane" xvar=x yvar=y
  flag_color := default_exit_condition_color
  run script(max_iters)
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @tool "Trace" for="Dynamical plane" shortcut=t
  trace_color : Color as "Trace color" := white

  on deactivated:
    erase

  on click-or-drag at (x, y):
    erase
    use trace_color for line
    # (the script sets these for coloring, which the tool doesn't use)
    flag_color := default_exit_condition_color
    draw point at (x, y)
    # each step runs the script once more, with each loop making at most one pass
    repeat up to 100 times:
      previous_x, previous_y := x, y
      run script(1)
      draw point at (x, y)
      draw line from (previous_x, previous_y) to (x, y)
    until (x, y) escapes
metadata:
  title: "torus"
  description: Translated from a script for the original FractalStream.

# The original script:
#   {real}
#   [Out of Bounds].
#   if y < 1 and y > 0 and x < π and x > -π then:
#   do
#   set (x,y) to (2*arctan(sin(2*x) / (cos(2*x) + y^2)), (cos(2*x) + 1) /
#   (cos(2*x) + y^2 / 2 + 1 / (2*y^2))).
#   reduce x mod π.
#   until y = 0 or y = 1.
#   if y = 0 then [Lower Basin]. else [Upper Basin].
#   end.
