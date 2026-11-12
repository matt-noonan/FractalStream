# For dev-tools/web/check-tools.mjs's `tables` section: a table its tool
# fills (click with Fill), which the viewer shows (black while it's empty).
script: |
  @config "P"
  T : Table 2 of C
  C : C := 0

  @viewer "V" var=C pixelSize=1/100
  (w, h) := table_size(T)
  color <- if w = 0 then black else rgb(re(T @ point C) / 600, im(T @ point C) / 600, 0.5)

  @tool "Fill" for="V"
  on click at p:
    reset T to (viewer_width, viewer_height)
    for each j in range(1, viewer_width):
      for each k in range(1, viewer_height):
        T @ (j, k) <- j + k i
