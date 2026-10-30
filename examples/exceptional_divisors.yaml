script: |
  @config "Configuration"
  tol : R as "Tolerance" <- 0.01
  t : C as "t = " <- 1+i

  @config "t-plane colors"
  tcolor1 : Color as "1" <- red
  tcolor2 : Color as "ω" <- green
  tcolor3 : Color as "ω²" <- blue

  @config "z-plane colors"
  zcol : Color as "escaped" <- yellow
  color8 : Color as "inf" <- red
  color9 : Color as "j" <- green
  color10 : Color as "jbar" <- blue
  exceptional : Color as "exceptional" <- rgb(1,0.7,0.7)

  @common
  # the primitive cube roots of 1
  j := -1/2 + i sqrt(3) / 2
  jbar := -1/2 - i sqrt(3) / 2

  @viewer "t-plane" var=t vanish=tol
  while t^3 != 1:
    t <- t (2 + t^3) / (2 t^3 + 1)

  if t = 1:
    color <- tcolor1
  else if t = j:
    color <- tcolor2
  else if t = jbar:
    color <- tcolor3
  else:
    color <- black

  @viewer "z-plane" vanish=tol
  y := 0.5 / z
  x := 0.5 * t / z

  until x = 0 or x^3 = -1 or x^3 = 1/8:
    w := -3 x (2 y^3 + x^3) / ((1 - 6 x y) - 4 (x^3 + y^3) + 6 x^2 y^2)
    y <- -3 y (2 x^3 + y^3) / ((1 - 6 x y) - 4 (x^3 + y^3) + 6 x^2 y^2)
    x <- w

  if x = 0:
    color <- zcol
  else if x = -1:
    w := (y + 1) / (x - y)
    iterate w -> 2 w^3 / 3 + w^2 + 2w + 1/3 until ...
      w escapes or w = j or w = jbar

    if w escaped:
      color <- color8
    else if w = j:
      color <- color9
    else if w = jbar:
      color <- color10
    else:
      color <- black
  else if x = -j:
    w := (y / jbar + 1)/(x/j - y/jbar)
    iterate w -> 2 w^3 / 3 + w^2 + 2w + 1/3 until ...
      w escapes or w = j or w = jbar
    if w escapes:
      color <- color8
    else if w = j:
      color <- color9
    else if w = jbar:
      color <- color10
    else:
      color <- black
  else if x = -jbar:
    w := (y/j + 1) / (x/jbar - y/j)
    iterate w ->  2 w^3 / 3 + w^2 + 2w + 1/3 until ...
      w escapes or w = j or w = jbar
    if w escapes:
      color <- color8
    else if w = j:
      color <- color9
    else if w = jbar:
      color <- color10
    else:
      color <- black
  else if x^3 = 1/8:
    color <- exceptional
  else:
    color <- black
