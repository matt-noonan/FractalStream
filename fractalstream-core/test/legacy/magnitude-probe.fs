# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue
  disc_color : Color as "Disc" := red

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  define script(limit):
    if |z| < 1:
      flag_color <- disc_color

  define probe_magnitude:
    r := sqrt(z * bar(z))
    magnitude <- re(r)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  run script(max_iters)
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @tool "Magnitude" for="Dynamical plane"
  magnitude : R as "Magnitude" := 0
  on click at clicked:
    z := clicked
    run probe_magnitude

  @tool "Trace" for="Dynamical plane" shortcut=t
  trace_color : Color as "Trace color" := white

  on deactivated:
    erase

  on click-or-drag at z:
    erase
    use trace_color for line
    # (the script sets these for coloring, which the tool doesn't use)
    flag_color := default_exit_condition_color
    draw point at z
    # each step runs the script once more, with each loop making at most one pass
    repeat up to 100 times:
      previous := z
      run script(1)
      draw point at z
      draw line from previous to z
    until z escapes
metadata:
  title: "magnitude-probe"
  description: Translated from a script for the original FractalStream.

# The original script:
#   if z < 1 then [Disc].
#   probe real "Magnitude":
#     set r to sqrt(z*bar(z)).
#     report r.
#   end.
