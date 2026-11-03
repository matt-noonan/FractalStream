# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue
  checkered_color : Color as "Checkered" <- red

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  # Checkers on the exterior approximate external rays.
  iterate z -> z^2 - 1 until z escapes
  if im(z) > 0:
    flag_color <- checkered_color
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)
metadata:
  title: "checkered"
  description: Translated from a script for the original FractalStream.

# The original script:
#   (Checkers on the exterior approximate external rays.)
#   iterate z^2 - 1 until z escapes.
#   if im(z) > 0 then [Checkered | red].
