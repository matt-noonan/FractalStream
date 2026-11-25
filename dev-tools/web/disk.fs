format: 1
script: |
  # A disk viewer (on the disk), for dev-tools/web/check-tools.mjs: red
  # around the hyperbolic plane's origin, grey elsewhere.
  @viewer "Disk" on the disk
  setup:
    pixel size: 1/250
  color <- if |z| < 0.04 then red else grey
