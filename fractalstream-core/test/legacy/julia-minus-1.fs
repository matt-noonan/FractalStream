# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z^2 - 1 until z escapes
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)
metadata:
  title: "julia-minus-1"
  description: Translated from a script for the original FractalStream.

# The original script:
#   iterate z^2 - 1 until z escapes.
