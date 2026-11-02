script: |
  @config "Configuration"
  listA : List of C as "List A" <- [0, 1, -1]
  listB : List of C as "List B" <- [i, -i, 0.5 + 0.5i]

  @viewer "List Join" var=z pixelSize="1/48"
  all : List of C <- join(listA, listB)
  closest : R <- 1000
  for each c in all:
    d : R <- |z - c|
    if d < closest:
      closest <- d
  color <- blend(exp(-closest * 5), white, black)

wx-properties:
  viewers:
    "List Join":
      width: 512
      height: 512
