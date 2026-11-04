# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  p : C as "p =" <- -0.125 + 0.75i

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @common
  let shade(c, t) = if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z^2 + p until z escapes
  color <- if stuck then black else shade(flag_color, iterations / 20)
metadata:
  title: "user-parameter"
  description: Translated from a script for the original FractalStream.

# The original script:
#   default p to -0.125 + 0.75i.
#   iterate z^2 + p until z escapes.
