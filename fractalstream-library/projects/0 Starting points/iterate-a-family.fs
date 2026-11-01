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
metadata:
  title: Iterate a family of functions
  description: |
    Like "Iterate a function", but for a family of functions depending on a
    parameter C: a parameter plane shows how each C behaves, and a dynamical
    plane shows the selected C's dynamics. Edit the family and the colors at
    the top of the script.
  preview-commands: |
    capture viewer "Parametric complex dynamics parameter plane" as preview
  preview: |
    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
    QmCC
