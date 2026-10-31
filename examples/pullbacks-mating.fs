script: |
  @config "Configuration"
  C : C as "C = " <- 0.5000000000 + 0.8660254038*i
  p : R as "Brightness: " <- 0.5
  color1 : Color as "Selected point" <- white

  @common
  let f(z) = (z^2 - C)/(z^2 - 1)
  # one of the two preimages of z under f (the other is its negative)
  let preimage(z) = sqrt((-C + z) / (z - 1))

  @viewer "C-plane" var=C center=0 pixelSize=1/128
  z := C
  iterate z -> f(z) until z escapes
  if stuck:
    color <- black
  else:
    color <- rainbow (iterations / 20)

  @viewer "Dynamics" center=0 pixelSize=1/128
  iterate z -> f(z) until z escapes
  if stuck:
    color <- black
  else:
    color <- dark rainbow (iterations / 20)

  @tool "Preimages" for=Dynamics shortcut="i" drawLayer=10
  pre1 : C as "Value" <- 0
  color2 : Color as "Color" <- red
  pre2 : C as "Value" <- 0
  color3 : Color as "Color" <- yellow
  on deactivated:
    erase
  on click-or-drag:
    erase
    use color1 for line
    draw point at z
    pre1 <- preimage(z)
    pre2 <- -preimage(z)
    use color2 for line
    draw point at pre1
    use color3 for line
    draw point at pre2

  @tool "Mark orbit of critical point" for=Dynamics shortcut=o drawLayer=11
  follow : Boolean as "Draw lines between iterations?" <- false
  zero_orbit_color : Color as "Orbit of 0" <- blue
  one_color : Color as "Mark 1" <- green
  button "Erase postcritical points"
  on button "Erase postcritical points":
    erase
  on refresh:
    erase
    use one_color for line
    draw point at 1
    use zero_orbit_color for line
    z := 0
    last_z := z
    repeat:
      draw point at z
      if follow:
        draw line from last_z to z
        last_z <- z
      z <- f(z)
    while z != 0

  @tool "Pullback" for=Dynamics shortcut=p drawLayer=12
  follow : Boolean as "Follow previous preimages?" <- false
  last_z : C as "Selected point" <- 0
  last_pre1 : C as "Value" <- 0
  color2 : Color as "Color" <- red
  last_pre2 : C as "Value" <- 0
  color3 : Color as "Color" <- yellow
  button "Erase"
  on button "Erase":
    erase
    follow <- false
  on drag-finished:
    follow <- false
  on drag:
    pre1 := preimage(z)
    pre2 := -preimage(z)

    if follow:
      use color1 for line
      draw line from last_z to z
      last_z <- z

      # Make sure that pre1 is the preimage that
      # is closest to last_pre1
      if |last_pre1 - pre2| < |last_pre1 - pre1|:
        tmp := pre1
        pre1 <- pre2
        pre2 <- tmp

      use color2 for line
      draw line from last_pre1 to pre1
      last_pre1 <- pre1

      use color3 for line
      draw line from last_pre2 to pre2
      last_pre2 <- pre2

    else:
      use color1 for line
      draw point at z
      last_z <- z

      use color2 for line
      draw point at pre1
      last_pre1 <- pre1

      use color3 for line
      draw point at pre2
      last_pre2 <- pre2
      follow <- true
