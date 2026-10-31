script: |
  @viewer "Douady rabbit"
  iterate z ⟼ z² + -0.12256 + 0.74486 𝑖 until z escapes
  color ⭠ if z escaped then white else black

wx-properties:
  viewers:
    "Douady rabbit":
      width: 400
      height: 425
