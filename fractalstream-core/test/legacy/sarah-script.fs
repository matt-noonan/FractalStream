# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" := 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue
  ba_color : Color as "BA" := red
  aa_color : Color as "AA" := green
  bb_color : Color as "BB" := orange
  ab_color : Color as "AB" := purple
  dynamics_color : Color as "Dynamics" := yellow

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  a := 0
  b := a
  a <- z
  z <- z^2 + C
  # NOTE: the original script has this here, with no `do` before it to end:
  #   until z escapes or z vanishes.
  if z vanishes:
    if |a - C| < |-a - C|:
      flag_color <- ba_color
      if |b - C| < |-b - C|:
        flag_color <- aa_color
    if |-a - C| < |a - C|:
      flag_color <- bb_color
      if |b - C| < |-b - C|:
        flag_color <- ab_color
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  flag_color <- dynamics_color
  iterate z -> z^2 + C until z escapes
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
      iterate z -> z^2 + C until z escapes up to 1 times
      draw point at z
      draw line from previous to z
    until z escapes
metadata:
  title: "sarah-script"
  description: Translated from a script for the original FractalStream.

# The original script:
#   dyn [Dynamics]
#   dyn iterate z^2 + c until z escapes.
#   dyn report z.
#   par block
#   set b to a.
#   set a to z.
#   set z to z^2 + c.
#   until z escapes or z vanishes.
#   report z.
#   if z vanishes then
#   if (a-c) < (-a-c) then
#   [BA]
#   if (b-c) < (-b-c) then [AA] end.
#   end.
#   if (-a-c) < (a-c) then
#   [BB]
#   if (b-c) < (-b-c) then [AB] end.
#   end.
#   end.
#   end.
