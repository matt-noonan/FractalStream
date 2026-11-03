# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  p : C as "p =" <- -0.125 + 0.75i

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z^2 + p until z escapes
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)
metadata:
  title: "user-parameter"
  description: Translated from a script for the original FractalStream.

# The original script:
#   default p to -0.125 + 0.75i.
#   iterate z^2 + p until z escapes.
