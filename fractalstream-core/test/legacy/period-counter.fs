# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" <- 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  iterate z -> z^2 + C until z escapes
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z^2 + C until z escapes
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @tool "Dynamics" for="Parameter plane" shortcut=d
  on click-or-drag:
    pass

  @tool "Period Counter" for="Parameter plane"
  period_counter : Z as "Period Counter" <- 0
  on click at clicked:
    z := 0
    z <- 0
    for each k in range(1, 1000):
      z <- z^2 + clicked
    w := z
    p := 0
    repeat:
      z <- z^2 + clicked
      p <- p + 1
    until z = w
    period_counter <- round(p)

  @tool "Period Counter" for="Dynamical plane"
  period_counter : Z as "Period Counter" <- 0
  on click at clicked:
    z := clicked
    z <- 0
    for each k in range(1, 1000):
      z <- z^2 + C
    w := z
    p := 0
    repeat:
      z <- z^2 + C
      p <- p + 1
    until z = w
    period_counter <- round(p)
metadata:
  title: "period-counter"
  description: Translated from a script for the original FractalStream.

# The original script:
#   probe integer "Period Counter":
#   set z to 0.
#   repeat 1000 times:
#   set z to z^2 + c.
#   end.
#   set w to z.
#   set p to 0.
#   do
#   set z to z^2 + c.
#   set p to p + 1.
#   until z = w.
#   report p.
#   end.
#   iterate z^2 + c until z escapes.
