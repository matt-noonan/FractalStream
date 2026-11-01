script: |
  @common
  # iterate z ⟼ expression(z) until exit_condition(z)
  let expression(z) = z^2 + i
  let exit_condition(z) = z escapes
  # colors can use color names like `red` or `black`, or color functions
  # involving the variables `z`, `C`, and `iterations`
  let on_success(z) = rainbow(iterations / 20)
  let on_failure(z) = black

  @viewer "Simple complex dynamics"
  iterate z -> expression(z) until exit_condition(z)
  color <- if stuck then on_failure(z) else on_success(z)

  @tool "Trace" shortcut=t
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
  title: Iterate a function
  description: |
    Iterates z ↦ f(z) for a function f of your choosing, and colors each
    point by whether its orbit escapes. The function, the stopping condition,
    and the colors are defined at the top of the script, ready to edit.
  preview-commands: |
    capture viewer "Simple complex dynamics" as preview
  preview: |
    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
    TkSuQmCC
