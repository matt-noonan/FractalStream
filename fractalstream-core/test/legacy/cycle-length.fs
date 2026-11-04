# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" <- 0

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue

  @common
  let shade(c, t) = if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  iterate z -> z^2 + C until z escapes
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iterate z -> z^2 + C until z escapes
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @tool "Cycle Length" for="Parameter plane"
  cycle_length : Z as "Cycle Length" <- 0
  on click at clicked:
    z := 0
    for each k in range(1, 500):
      z <- z^2 + clicked
    count := 0
    savedZ := z
    repeat:
      count <- count + 1
      z <- z^2 + clicked
    until z = savedZ
    cycle_length <- round(count)

  @tool "Cycle Length" for="Dynamical plane"
  cycle_length : Z as "Cycle Length" <- 0
  on click at clicked:
    z := clicked
    for each k in range(1, 500):
      z <- z^2 + C
    count := 0
    savedZ := z
    repeat:
      count <- count + 1
      z <- z^2 + C
    until z = savedZ
    cycle_length <- round(count)
metadata:
  title: "cycle-length"
  description: Translated from a script for the original FractalStream.

# The original script:
#   probe integer "Cycle Length":
#     repeat 500 times set z to z^2 + c.
#     set count to 0.
#     set savedZ to z.
#     do
#       set count to count + 1.
#       set z to z^2 + c.
#     until z = savedZ.
#     report count.
#   end.
#   iterate z^2 + c until z escapes.
