# For dev-tools/web/check-tools.mjs's `viewkind` section: a plane viewer,
# rebuilt as a sphere viewer and back (its color is the same on either).
format: 1
script: |
  @viewer "V" center=0 pixelSize=1/64
  color <- white
