script: |
  @config "Configuration"
  use_arenberg : Boolean as "Use Arenberg rainbow" <- false
  speed : R as "Iterations per color cycle: " <- 20
  C : C as "f(z) = z^2 +" <- -0.11 + 0.78i

  @common
  let palette(t) = if use_arenberg then arenberg(t) else rainbow(t)
  define draw:
    iterate z -> z^2 + C until z escapes
    color <- black
    if z escaped:
      m := iterations + 1 - log (log (|z|^2) / (2 log 10)) / log 2
      color <- palette(m / speed)

  @viewer "Reference"
  color <- palette(re z / 4)

  @viewer "Parameter plane" var=C
  z := 0
  run draw

  @viewer "Dynamical plane"
  run draw

wx-properties:
  viewers:
    "Reference":
      width: 640
      height: 32
