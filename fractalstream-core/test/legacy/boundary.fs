# Translated from a script for the original FractalStream.
script: |
  @config "Parameters"
  C : C as "C =" <- 0
  fatness : R as "fatness =" <- 0.05

  @config "Colors"
  default_exit_condition_color : Color as "Default Exit Condition" <- blue
  boundary_of_mandelbrot_set_color : Color as "Boundary of Mandelbrot Set" <- red
  dynamical_plane_color : Color as "Dynamical Plane" <- green
  boundary_of_julia_set_color : Color as "Boundary of Julia Set" <- orange

  @viewer "Parameter plane" var=C
  z := 0
  flag_color := default_exit_condition_color
  dzdc := 0
  repeat:
    dzdc <- 1
    dzdc <- 2 * z * dzdc + 1
    z <- z^2 + C
  until z escapes or |z / dzdc| < pixel_size * fatness
  if |z / dzdc| < pixel_size * fatness:
    flag_color <- boundary_of_mandelbrot_set_color
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @viewer "Dynamical plane"
  flag_color := default_exit_condition_color
  flag_color <- dynamical_plane_color
  dz := 1
  w := z
  repeat:
    dz <- 2 * z * dz
    z <- z^2 + C
  until z escapes or |z / dz - w / dz| < pixel_size * fatness * 2
  if |z / dz - w / dz| < pixel_size * fatness * 2:
    flag_color <- boundary_of_julia_set_color
  color <- if stuck then black else cycle(iterations / 20, dark flag_color, light flag_color)

  @tool "Dynamics" for="Parameter plane" shortcut=d
  on click-or-drag:
    pass
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
