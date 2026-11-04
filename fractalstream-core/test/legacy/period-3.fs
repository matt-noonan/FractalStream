# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" <- 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue
  period_3_cycle_color : Color as "Period 3 Cycle" <- red

  @common
  let shade(c, t) = if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

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
  color <- if stuck then black else shade(flag_color, iterations / 20)

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
  color <- if stuck then black else shade(flag_color, iterations / 20)
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
