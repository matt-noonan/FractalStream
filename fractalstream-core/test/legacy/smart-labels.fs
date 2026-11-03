# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" <- 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue
  purple_color : Color as "Purple" <- purple
  white_color : Color as "White" <- white
  orange_color : Color as "Orange" <- orange

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  a := -1 / 2 + C
  b := -1 / 2 - C
  iterate z -> z - (z - 1) * (z - a) * (z - b) / ((z - a) * (z - b) + (z - 1) * (z - b) + (z - 1) * (z - a)) until z stops
  if z = 1:
    flag_color <- purple_color
  if z = a:
    flag_color <- white_color
  if z = b:
    flag_color <- orange_color
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  a := -1 / 2 + C
  b := -1 / 2 - C
  iterate z -> z - (z - 1) * (z - a) * (z - b) / ((z - a) * (z - b) + (z - 1) * (z - b) + (z - 1) * (z - a)) until z stops
  if z = 1:
    flag_color <- purple_color
  if z = a:
    flag_color <- white_color
  if z = b:
    flag_color <- orange_color
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @tool "Dynamics" for="Parameter plane" shortcut=d
  on click-or-drag:
    pass
metadata:
  title: "smart-labels"
  description: Translated from a script for the original FractalStream.

# The original script:
#   set a to -1/2 + c.
#   set b to -1/2 -c.
#   iterate z - (z-1)*(z-a)*(z-b) / ((z-a)*(z-b) + (z-1)*(z-b) + (z-1)*(z-a)) until z
#   stops.
#   if z = 1 then [Purple].
#   if z = a then [White].
#   if z = b then [Orange].
