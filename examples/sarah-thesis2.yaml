script: |
  @viewer "Sarah thing"
  p := z
  x := z + 1
  y := z (z+1)^2 + 1

  repeat:
      x <- (y^2 - 1)/y^2
      y <- (y^2 - x^2) / y^2
  until x = 0 and y = 1
  saved_count := iterations

  z <- 0
  iterate z -> p z^2 + 1 until z escapes

  if z escapes:
      color <- cycle(saved_count / 20, dark red, light red)
  else:
      color <- cycle(saved_count / 20, dark green, light green)
