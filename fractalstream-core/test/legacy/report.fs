# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  x := z
  iterate x -> x - (x^3 - 1) / (3 * x^2) until x stops
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)
metadata:
  title: "report"
  description: Translated from a script for the original FractalStream.

# The original script:
#   set x to z.
#   iterate x - (x^3 - 1) / (3 * x^2) on x until x stops.
#   report x.
