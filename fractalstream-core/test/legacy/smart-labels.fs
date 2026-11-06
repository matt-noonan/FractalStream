# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" := 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue
  purple_color : Color as "Purple" := purple
  white_color : Color as "White" := white
  orange_color : Color as "Orange" := orange

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

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
  color <- if stuck then black else shade(flag_color, iterations / 20)

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
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @tool "Trace" for="Dynamical plane" shortcut=t
  trace_color : Color as "Trace color" := white

  on deactivated:
    erase

  on click-or-drag at z:
    erase
    use trace_color for line
    draw point at z
    # each step runs the script once more, with each loop making at most one pass
    repeat up to 100 times:
      previous := z
      a := -1 / 2 + C
      b := -1 / 2 - C
      iterate z -> z - (z - 1) * (z - a) * (z - b) / ((z - a) * (z - b) + (z - 1) * (z - b) + (z - 1) * (z - a)) until z stops up to 1 times
      draw point at z
      draw line from previous to z
    until z escapes
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
