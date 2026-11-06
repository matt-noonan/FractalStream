# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" := 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  define script(limit):
    iterate z -> z^2 + C until z escapes up to limit times

  define probe_period_counter(C):
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

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  run script(max_iters)
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  run script(max_iters)
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @tool "Period Counter" for="Parameter plane"
  period_counter : Z as "Period Counter" := 0
  on click at clicked:
    z := 0
    run probe_period_counter(clicked)

  @tool "Period Counter" for="Dynamical plane"
  period_counter : Z as "Period Counter" := 0
  on click at clicked:
    z := clicked
    run probe_period_counter(C)

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
