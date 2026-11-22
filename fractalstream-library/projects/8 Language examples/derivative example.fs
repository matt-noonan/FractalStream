format: 1
script: |
  @common
  f(z) ≔ z³ + i z - 1

  @viewer "Newton's method basin boundaries"
  w ≔ z
  iterate w ⟼ w - f(w) / f′(w) until w stops
  if |diff(w, z)| > vanishing_radius / 10:
    color ⟵ black
  else if |w - z| < 0.025:
    color ⟵ red
  else:
    color ⟵ white
metadata:
  title: Differentiate an iteration
  description: Demonstrates how to differentiate a quantity that changes during a
    script’s execution. In this example, we differentiate the final value of Newton’s
    method with respect to the initial value and mark points where the derivative
    is non-zero in black. This approximates the boundaries of the basins of attraction
    for Newton’s method.
  preview-commands: |
    select viewer "Newton's method basin boundaries"
    set viewer center to -0.31 + 0.55i
    set viewer pixel size to 0.00012
    capture viewer "Newton's method basin boundaries" as preview
  preview: |
    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
    F/M7Q9xqFG83AAAAAElFTkSuQmCC
