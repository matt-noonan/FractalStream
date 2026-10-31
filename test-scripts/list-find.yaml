script: |
  @config "Configuration"
  myList : List of R as "List" <- [-2, -1, 0.5, 1.5, 2.5]
  threshold : R as "Threshold" <- 0

  @viewer "List Find" var=z pixelSize="1/64"
  found : R <- find(myList, x -> x > threshold, -99)
  d : R <- |re z - found|
  color <- if found = -99 then grey else blend(exp(-d * 8), white, black)

wx-properties:
  viewers:
    "List Find":
      width: 512
      height: 512
