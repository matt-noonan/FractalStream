script: |
  # A disk viewer (view=disk), for dev-tools/web/check-tools.mjs: red
  # around the hyperbolic plane's origin, grey elsewhere.
  @viewer "Disk" view=disk pixelSize=1/250
  color <- if |z| < 0.04 then red else grey
