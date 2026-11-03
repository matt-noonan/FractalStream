# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" <- 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue
  period_3_cycle_color : Color as "Period 3 Cycle" <- red

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  last := 0
  b := 0
  a := 0
  repeat:
    last <- b
    b <- a
    a <- z
    z <- z^2 + C
  until z escapes or z = last
  if z = last:
    flag_color <- period_3_cycle_color
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  last := 0
  b := 0
  a := 0
  repeat:
    last <- b
    b <- a
    a <- z
    z <- z^2 + C
  until z escapes or z = last
  if z = last:
    flag_color <- period_3_cycle_color
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @tool "Dynamics" for="Parameter plane" shortcut=d
  on click-or-drag:
    pass
metadata:
  title: "period-3"
  description: Translated from a script for the original FractalStream.

# The original script:
#   do
#   set last to b.
#   set b to a.
#   set a to z.
#   set z to z^2 + c.
#   until z escapes or z = last.
#   if z = last then [Period 3 Cycle].
