# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" <- 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  iterate z -> z - (exp(z) - C) / exp(z) until z stops
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z - (exp(z) - C) / exp(z) until z stops
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @tool "Dynamics" for="Parameter plane" shortcut=d
  on click-or-drag:
    pass
metadata:
  title: "exp-newton"
  description: Translated from a script for the original FractalStream.

# The original script:
#   iterate z - (exp(z) - c)/exp(z) until z stops.
