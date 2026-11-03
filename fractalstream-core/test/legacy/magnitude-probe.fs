# Translated from a script for the original FractalStream.
script: |
  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue
  disc_color : Color as "Disc" <- red

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  if |z| < 1:
    flag_color <- disc_color
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

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
