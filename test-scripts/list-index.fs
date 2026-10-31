script: |
  @config "Configuration"
  myList : List of C as "Points" <- [0, 1, -1, i, -i]
  idx : Z as "Index (1-based)" <- 1

  @viewer "List Index" var=z pixelSize="1/48"
  n : Z <- length(myList)
  # Direct index (1-based): light up point at 'idx'
  chosen : C <- myList @ idx
  # Cyclic index: should wrap; light up a second point
  chosen2 : C <- myList @@ (idx + n)
  d1 : R <- |z - chosen|
  d2 : R <- |z - chosen2|
  closest : R <- if d1 < d2 then d1 else d2
  color <- blend(exp(-closest * 8), white, black)

wx-properties:
  viewers:
    "List Index":
      width: 512
      height: 512
