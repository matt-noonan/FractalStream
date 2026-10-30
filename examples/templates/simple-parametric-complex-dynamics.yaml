script: |
  @common
  # iterate z ⟼ expression(z) until exit_condition(z), starting from
  # initialZ(C) in the parameter plane
  let expression(z) = z^2 + C
  let exit_condition(z) = z escapes
  let initialZ(C) = 0
  # colors can use color names like `red` or `black`, or color functions
  # involving the variables `z`, `C`, and `iterations`
  let on_success(z) = rainbow(iterations / 20)
  let on_failure(z) = black

  @config "Configuration"
  C : C as "Selected value of C:" <- -0.1226 + 0.7449i

  @viewer "Parametric complex dynamics parameter plane" var=C
  z := initialZ(C)
  iterate z -> expression(z) until exit_condition(z)
  color <- if stuck then on_failure(z) else on_success(z)

  @viewer "Parametric complex dynamics"
  iterate z -> expression(z) until exit_condition(z)
  color <- if stuck then on_failure(z) else on_success(z)

  @tool "Trace" shortcut=t for="Parametric complex dynamics"
  on refresh:
    erase
  on deactivated:
    erase
  on click-or-drag:
    use white for line
    erase
    prevZ := z
    until exit_condition(z):
      prevZ <- z
      draw point at z
      z <- expression(z)
      draw line from z to prevZ
