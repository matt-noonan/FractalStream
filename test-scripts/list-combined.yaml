script: |
  @config "Configuration"
  listA : List of R as "List A" <- [-3, -1, 2, 4]
  listB : List of R as "List B" <- [-2, 0.5, 1, 3]
  threshold : R as "Threshold" <- 5

  @viewer "List Pipeline (join→remove→transform→find)" var=z pixelSize="1/8"
  combined : List of R <- join(listA, listB)
  positives : List of R <- remove(combined, x -> x <= 0)
  squared : List of R <- transform(positives, x -> x * x)
  result : R <- find(squared, x -> x > threshold, -1)
  d : R <- |re z - result|
  color <- if result < 0 then grey else blend(exp(-d * 15), white, black)

wx-properties:
  viewers:
    "List Pipeline (join→remove→transform→find)":
      width: 512
      height: 512
