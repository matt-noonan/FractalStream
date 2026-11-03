# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" <- 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @common
  let shade(c, t) = if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  iterate z -> z^2 + C until z escapes
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z^2 + C until z escapes
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @tool "Dynamics" for="Parameter plane" shortcut=d
  on click-or-drag:
    pass
metadata:
  title: "mandelbrot"
  description: Translated from a script for the original FractalStream.

# The original script:
#   iterate z^2 + c until z escapes.
