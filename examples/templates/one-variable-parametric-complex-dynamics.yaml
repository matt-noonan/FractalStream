script: |
  @common
  # This script draws in the z-plane and in the C-plane, where C is a
  # complex parameter. It runs once per pixel in the view, and should use
  # the `color` variable to indicate what color to use for that pixel.
  # Use the `dynamic` variable to test if the script is running in the
  # z-plane.
  z0 := 0
  define script:
    iterate z -> z^2 + C until z escapes
    if stuck:
      color <- black
    else if im z > 0:
      color <- light rainbow(iterations / 20)
    else:
      color <- dark rainbow(iterations / 20)

  @config "Configuration"
  C : C as "C =" <- 0

  @viewer "Parametric complex dynamics parameter plane" var=C pixelSize=1/100
  dynamic := false
  z := z0
  run script

  @viewer "Parametric complex dynamics dynamical plane" pixelSize=1/100
  dynamic := true
  run script
