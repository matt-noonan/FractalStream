script: |
  @viewer "Douady rabbit"
  # Iterate until we escape or loop 100 times
  while |z| < 10:
    z ⭠ z² + -0.12256 …
            + 0.74486 ...
            𝑖

  # If we didn't escape in 100 loops, draw a black
  # point. Otherwise, draw a white point.
  if iterations = 100:
    color ⭠ black
  else:
    color ⭠ white

wx-properties:
  viewers:
    "Douady rabbit":
      width: 400
      height: 425
