script: |
  @config "Parameter"
  λ : C as "λ =" <- 0.005 + 0.1i

  @common
  let f(z) = z^2 + λ / z^2

  # The critical points (the fourth roots of λ) all map to 2√λ, so
  # one orbit tells whether the critical points escape.
  @viewer "Parameter plane" var=λ center=-0.1 pixelSize=1/1500
  z := 2 sqrt(λ)
  iterate z -> f(z) until z escapes
  color <- if stuck then black else cycle(iterations / 12, dark blue, white)

  @viewer "Julia set" var=z view=sphere center=1
  iterate z -> f(z) until z escapes
  color <- if stuck then black else cycle(iterations / 6, dark orange, light yellow)
metadata:
  title: The McMullen family
  description: |
    The rational maps z² + λ/z², on the Riemann sphere. ∞ is
    superattracting, and the disk around 0 that maps onto its basin is a
    second cap; between the two, the Julia set can be a Cantor set of
    circles, or, in the escape regions of the parameter plane, a
    Sierpiński curve. Click the parameter plane to choose λ, and drag the
    Julia set's globe to turn it.
  preview-commands: |
    capture viewer "Julia set" base as preview
  preview: |
    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
