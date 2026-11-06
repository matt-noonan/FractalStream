# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue
  z_is_1_color : Color as "z is 1" := red
  z_is_omega_color : Color as "z is omega" := green
  z_is_omegabar_color : Color as "z is omegabar" := orange

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z - (z^3 - 1) / (3 * z^2) until z stops
  if z = 1:
    flag_color <- z_is_1_color
  if z = (-1 + sqrt(3) * i) / 2:
    flag_color <- z_is_omega_color
  if z = (-1 - sqrt(3) * i) / 2:
    flag_color <- z_is_omegabar_color
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
      iterate z -> z - (z^3 - 1) / (3 * z^2) until z stops up to 1 times
      draw point at z
      draw line from previous to z
    until z escapes
metadata:
  title: "newton-flags"
  description: Translated from a script for the original FractalStream.

# The original script:
#   iterate z - (z^3 - 1) / (3 * z^2) until z stops.
#   if z = 1 then [z is 1].
#   if z = (-1 + sqrt(3) * i) / 2 then [z is omega].
#   if z = (-1 - sqrt(3) * i) / 2 then [z is omegabar].
