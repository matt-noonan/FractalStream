# For dev-tools/web/check-tools.mjs's `density` section: a band 4 pixels
# wide (2 pixel_size either side of the imaginary axis), which should be 4
# CSS pixels wide on any screen, as on the desktop.
format: 1
script: |
  @viewer "V"
  setup:
    center: 0
    pixel size: 1/128
  color <- if |re z| < 2 pixel_size then black else white
