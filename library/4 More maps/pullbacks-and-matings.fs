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
metadata:
  title: Pullbacks and matings
  description: |
    The quadratic rational maps f(z) = (z² - C)/(z² - 1), where one
    critical point lies in a superattracting 2-cycle (∞ ↦ 1 ↦ ∞); many of
    them are matings of the basilica with a quadratic polynomial. Tools draw
    a point's two preimages, pull paths back under f, and mark the orbit of
    the other critical point.
  preview-commands: |
    capture viewer "C-plane" as preview
  preview: |
    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
    Mb8AAAAASUVORK5CYII=
