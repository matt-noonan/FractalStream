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
metadata:
  title: Write parameter and dynamical planes
  description: |
    A parameter plane and a dynamical plane sharing one script, which is
    yours to write. It runs once for each pixel in either plane, with
    `dynamic` saying which plane it's running in.
  preview-commands: |
    capture viewer "Parametric complex dynamics parameter plane" as preview
  preview: |
    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
