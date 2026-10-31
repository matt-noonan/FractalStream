script: |
  @config "Configuration"
  myList : List of C as "Points" <- [0, 1, -1, i, -i]
  radius : R as "Radius" <- 0.4

  @viewer "List Lookup (with first matching)" var=z pixelSize="1/48"
  color <- black

  with first c matching |z - c| < radius in myList:
    color <- blend(|z - c| / radius, red, yellow)

wx-properties:
  viewers:
    "List Lookup (with first matching)":
      width: 512
      height: 512
