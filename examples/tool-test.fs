format: 1
script: |
  @config "Test"
  clicked : C as "test" := 0
  pixelSize : R as "test2" := 0

  @viewer "Test"
  color <- if |z| < 1 then blue else red

  @tool "Clicky"
  on click:
    z := click
    clicked <- z

  on activated:
    pixelSize <- pixel_size
