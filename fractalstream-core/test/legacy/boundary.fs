# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" := 0
  fatness : R as "fatness =" := 0.05

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" := blue
  boundary_of_mandelbrot_set_color : Color as "Boundary of Mandelbrot Set" := red
  dynamical_plane_color : Color as "Dynamical Plane" := green
  boundary_of_julia_set_color : Color as "Boundary of Julia Set" := orange

  @config "Iteration settings"
  vanishing_radius : R as "Vanishing radius: " := 0.01

  @common
  shade(c, t) := if sin(2 pi t) > 0 then blend(sin(2 pi t), dark c, c) ...
    else blend(-sin(2 pi t), light c, c)

  define script(limit, in_parameter_plane):
    dzdc := 0
    dz := 0
    w := 0
    if in_parameter_plane:
      repeat up to limit times:
        dzdc <- 1
        dzdc <- 2 * z * dzdc + 1
        z <- z^2 + C
      until z escapes or |z / dzdc| < pixel_size * fatness
      if |z / dzdc| < pixel_size * fatness:
        flag_color <- boundary_of_mandelbrot_set_color
    if not in_parameter_plane:
      flag_color <- dynamical_plane_color
      dz <- 1
      w <- z
      repeat up to limit times:
        dz <- 2 * z * dz
        z <- z^2 + C
      until z escapes or |z / dz - w / dz| < pixel_size * fatness * 2
      if |z / dz - w / dz| < pixel_size * fatness * 2:
        flag_color <- boundary_of_julia_set_color

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  run script(max_iters, true)
  color <- if stuck then black else shade(flag_color, iterations / 20)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  run script(max_iters, false)
  color <- if stuck then black else shade(flag_color, iterations / 20)

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
      run script(1, false)
      draw point at z
      draw line from previous to z
    until z escapes
metadata:
  title: "boundary"
  description: Translated from a script for the original FractalStream.

# The original script:
#   default fatness to 0.05.
#   par block
#   do
#   set dzdc to 1.
#   set dzdc to 2 * z * dzdc + 1.
#   set z to z^2 + c.
#   until z escapes or z / dzdc < pixel * fatness.
#   if z / dzdc < pixel * fatness then [Boundary of Mandelbrot Set].
#   end.
#   dyn block
#   [Dynamical Plane].
#   set dz to 1.
#   set w to z.
#   do
#   set dz to 2*z*dz.
#   set z to z^2+c.
#   until z escapes or (z/dz-w/dz) < pixel * fatness * 2.
#   if(z/dz - w/dz) < pixel * fatness * 2 then [Boundary of Julia Set].
#   end.
