# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" <- 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  repeat:
    z <- z^2 + C
  until z vanishes
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z^2 + C until z escapes
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @tool "Dynamics" for="Parameter plane" shortcut=d
  on click-or-drag:
    pass
metadata:
  title: "component-centers"
  description: Translated from a script for the original FractalStream.

# The original script:
#   par iterate z^2 + c until z vanishes.
#   dyn iterate z^2 + c until z escapes.
