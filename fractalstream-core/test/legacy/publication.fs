# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" := 0
  fatness : R as "fatness =" := 1 / 10

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue
  black_color : Color as "black" := black
  white_color : Color as "white" := white

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  iters := 0
  gave_up := false
  dxdc := 0
  x := 0
  der := 1
  repeat:
    dxdc <- 1 + 2 * x * dxdc
    x <- x^2 + C
    der <- der * 2 * x
  until |x / dxdc| < pixel_size * fatness / 2 or x escapes or der vanishes
  iters <- iterations
  gave_up <- stuck
  if |x / dxdc| < pixel_size * fatness / 2:
    flag_color <- black_color
  else:
    flag_color <- white_color
  iters <- 0
  gave_up <- false
  color <- if gave_up then black else shade(flag_color, iters / 20)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  iters := 0
  gave_up := false
  d := 1
  w := z
  repeat:
    d <- 2 * z * d
    z <- z^2 + C
  until z escapes or |z / d - w / d| < pixel_size * fatness or d vanishes
  iters <- iterations
  gave_up <- stuck
  if |z / d - w / d| < pixel_size * fatness / 2:
    flag_color <- black_color
  else:
    flag_color <- white_color
  iters <- 0
  gave_up <- false
  color <- if gave_up then black else shade(flag_color, iters / 20)
metadata:
  title: "publication"
  description: Translated from a script for the original FractalStream.

# The original script:
#   default fatness to 1/10.
#   par block
#   set x to 0.
#   set der to 1.
#   do
#   set dxdc to 1+2*x*dxdc.
#   set x to x^2+c.
#   set der to der*2*x.
#   until (x/dxdc) < pixel*fatness/2 or x escapes or der vanishes.
#   if (x/dxdc) < pixel*fatness/2 then [black]. else [white].
#   succeed.
#   end.
#   dyn block
#   set d to 1.
#   set w to z.
#   do
#   set d to 2*z*d.
#   set z to z^2+c.
#   until z escapes or (z/d-w/d) < pixel*fatness or d vanishes.
#   if (z/d-w/d) < pixel*fatness/2 then [black]. else [white].
#   succeed.
#   end.
