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

  @viewer "Parameter plane" xvar=a yvar=b
  x, y := 0, 0
  flag_color := default_exit_condition_color
  u := 0
  v := 0
  t := 0
  h := 2 * pi / 10
  repeat:
    u, v <- x, y
    t <- 0
    for each k in range(1, 10):
      x <- x + h * y
      y <- y + h * (cos(t) - a * y - b * sin(x))
      t <- t + h
  until (u, v) = (x, y)
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @viewer "Dynamical plane" xvar=x yvar=y
  flag_color := default_exit_condition_color
  u := 0
  v := 0
  t := 0
  h := 2 * pi / 10
  repeat:
    u, v <- x, y
    t <- 0
    for each k in range(1, 10):
      x <- x + h * y
      y <- y + h * (cos(t) - a * y - b * sin(x))
      t <- t + h
  until (u, v) = (x, y)
  color <- if stuck then black else shade(flag_color, iterations / 20)
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
