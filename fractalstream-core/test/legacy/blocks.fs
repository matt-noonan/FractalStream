# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  a := 0
  b := 0
  if re(z) < 1:
    a <- 2
    b <- 3
  else:
    a <- -2
    b <- 8
  iterate z -> z^2 + a / b until z escapes
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
      a := 0
      b := 0
      if re(z) < 1:
        a <- 2
        b <- 3
      else:
        a <- -2
        b <- 8
      iterate z -> z^2 + a / b until z escapes up to 1 times
      draw point at z
      draw line from previous to z
    until z escapes
metadata:
  title: "blocks"
  description: Translated from a script for the original FractalStream.

# The original script:
#   if re(z) < 1 then:
#       set a to 2.
#       set b to 3.
#   end.
#   else:
#       set a to -2.
#       set b to 8.
#   end.
#   iterate z^2 + a/b until z escapes.
