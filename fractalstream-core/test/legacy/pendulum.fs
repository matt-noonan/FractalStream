# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  a : R as "a =" := 0
  b : R as "b =" := 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  define script(limit):
    u := 0
    v := 0
    t := 0
    h := 2 * pi / 10
    repeat up to limit times:
      u, v <- x, y
      t <- 0
      for each k in range(1, 10):
        x <- x + h * y
        y <- y + h * (cos(t) - a * y - b * sin(x))
        t <- t + h
    until (u, v) = (x, y)

  @viewer "Parameter plane" xvar=a yvar=b
  x, y := 0, 0
  flag_color := default_exit_condition_color
  run script(max_iters)
  color <- if stuck then black else shade(flag_color, iterations / 20)

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
  title: "pendulum"
  description: Translated from a script for the original FractalStream.

# The original script:
#   {real}
#
#   set h to 2*π / 10.
#
#   do
#   	set (u,v) to (x,y).
#   	set t to 0.
#   	repeat 10 times:
#   		set x to x + h*y.
#   		set y to y + h*(cos(t) - a*y - b*sin(x)).
#   		set t to t + h.
#   	end.
#   until (u,v) = (x,y).
