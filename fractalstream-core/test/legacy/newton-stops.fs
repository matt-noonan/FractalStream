# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z - (z^3 - 1) / (3 * z^2) until z stops
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)
metadata:
  title: "newton-stops"
  description: Translated from a script for the original FractalStream.

# The original script:
#   iterate z - (z^3 - 1) / (3 * z^2) until z stops.
