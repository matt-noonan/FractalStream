# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z - (z^3 - 1) / (3 * z^2) until z stops
  color <- if stuck then black else shade(flag_color, iterations / 20)
metadata:
  title: "newton-stops"
  description: Translated from a script for the original FractalStream.

# The original script:
#   iterate z - (z^3 - 1) / (3 * z^2) until z stops.
