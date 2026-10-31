script: |
  @common
  # iterate f until stop is true
  let f(z) = z^2 + C
  let stop(z) = |z| > escape_radius

  define draw:
    iterate z -> f(z) until stop(z)

    if stop(z):
      c1 := if im(z) > 0 then blue else yellow
      c2 := if im(z) > 0 then green else red
      s := iterations + 1 - (log (log (|z|^2 / 2))) / log 2
      s <- cos (s pi / 10)^2
      color <- blend (s, c1, c2)
    else:
      color <- black

  @config "Configuration"
  C : C as "Selected value of C:" <- -0.1226 + 0.7449i

  @viewer "Parameter plane" var=C center=0 pixelSize=1/128
  z := 0
  run draw

  @viewer "Dynamical plane" center=0 pixelSize=1/128
  run draw

  @tool "Trace" for="Dynamical plane" shortcut=t
  on deactivated:
    erase
  on click-or-drag:
    prevZ := z
    erase
    until stop(z):
      prevZ <- z
      draw point at z
      z <- f(z)
      draw line from z to prevZ
