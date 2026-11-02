script: |
  @config "Configuration"
  C : C as "Selected value of C:" <- -0.1226 + 0.7449i
  tiny : R as "Min. radius: " <- 0.001
  fatness : R as "Fatness" <- 0.1

  @config "Colors"
  boundary_color : Color as "Boundary" <- black
  exterior_color : Color as "Exterior" <- white
  interior_color : Color as "Interior" <- rgb(0.926, 0.863, 0.927)

  @common
  define shade(on_boundary):
    if z escaped:
      color <- exterior_color
    else if on_boundary:
      color <- boundary_color
    else:
      color <- interior_color

  @viewer "Parameter plane" var=C vanish=tiny
  z := 0
  tol := pixel_size fatness / 2

  dzdc := 0
  der := 1

  repeat:
    dzdc <- 1 + 2 z dzdc
    z <- z^2 + C
    der <- 2 z der
  until |z / dzdc| < tol or z escapes or der vanishes

  run shade(|z / dzdc| < tol)

  @viewer "Dynamical plane" vanish=tiny
  dz := 1
  w := z
  tol := 2 pixel_size fatness

  repeat:
    dz <- 2 z dz
    z <- z^2 + C
  until z escapes or |z/dz - w/dz| < tol or dz vanishes

  run shade(|z/dz - w/dz| < tol)
