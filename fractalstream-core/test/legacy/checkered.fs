# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue
  checkered_color : Color as "Checkered" := red

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  # Checkers on the exterior approximate external rays.
  iterate z -> z^2 - 1 until z escapes
  if im(z) > 0:
    flag_color <- checkered_color
  color <- if stuck then black else shade(flag_color, iterations / 20)
metadata:
  title: "checkered"
  description: Translated from a script for the original FractalStream.

# The original script:
#   (Checkers on the exterior approximate external rays.)
#   iterate z^2 - 1 until z escapes.
#   if im(z) > 0 then [Checkered | red].
