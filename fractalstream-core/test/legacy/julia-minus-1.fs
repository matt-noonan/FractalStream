# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z^2 - 1 until z escapes
  color <- if stuck then black else shade(flag_color, iterations / 20)
metadata:
  title: "julia-minus-1"
  description: Translated from a script for the original FractalStream.

# The original script:
#   iterate z^2 - 1 until z escapes.
