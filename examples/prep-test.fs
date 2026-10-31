script: |
  @config "Configuration"
  myList : List of C <- [0, 1, i]
  radius : R as "Radius" <- 0.2
  myColor : Color as "My color" <- yellow

  @viewer "Voronoi"
  prepare computed : Color:
    computed <- myColor
  color <- if |z| < radius then blue else computed
