# For dev-tools/web/check-tools.mjs's `density` section: a band 4 pixels
# wide (2 pixel_size either side of the imaginary axis), which should be 4
# CSS pixels wide on any screen, as on the desktop.
script: |
  @viewer "V" center=0 pixelSize=1/128
  color <- if |re z| < 2 pixel_size then black else white
