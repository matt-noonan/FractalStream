# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue
  disc_color : Color as "Disc" <- red

  @common
  let shade(c, t) = if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  if |z| < 1:
    flag_color <- disc_color
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @tool "Magnitude" for="Dynamical plane"
  magnitude : R as "Magnitude" <- 0
  on click at clicked:
    z := clicked
    r := sqrt(z * bar(z))
    magnitude <- re(r)
metadata:
  title: "magnitude-probe"
  description: Translated from a script for the original FractalStream.

# The original script:
#   if z < 1 then [Disc].
#   probe real "Magnitude":
#     set r to sqrt(z*bar(z)).
#     report r.
#   end.
