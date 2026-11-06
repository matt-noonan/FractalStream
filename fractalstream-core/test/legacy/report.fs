# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  x := z
  iterate x -> x - (x^3 - 1) / (3 * x^2) until x stops
  color <- if stuck then black else shade(flag_color, iterations / 20)
metadata:
  title: "report"
  description: Translated from a script for the original FractalStream.

# The original script:
#   set x to z.
#   iterate x - (x^3 - 1) / (3 * x^2) on x until x stops.
#   report x.
