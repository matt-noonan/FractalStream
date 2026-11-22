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
  preview: |
    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
wx-properties:
  viewers:
    Newton's method basin boundaries:
      x: 100
      'y': 100
      width: 600
      height: 600
      center:
      - 0
      - 0
      pixel-size: 7.8125e-3
  configuration:
    x: 278
    'y': 191
