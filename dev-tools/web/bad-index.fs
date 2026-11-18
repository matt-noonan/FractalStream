# For check-tools.mjs (issues): the right half indexes a list out of
# range, so its pixels are magenta, and the viewer says why.
format: 1
script: |
  @viewer "Bad index" var=z center=0 pixelSize=1/50
  xs := [red, green]
  color <- if re z < 0 then xs @ 1 else xs @ 3
