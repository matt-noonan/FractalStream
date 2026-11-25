# For dev-tools/web/check-tools.mjs's `showif` section: a Julia set shown
# only once a point of the Mandelbrot set has been picked (show if:).
format: 1
script: |
  @config "Parameters"
  C : C := 0
  selected_C : Bool := false

  @viewer "Mandelbrot set" over C
  setup:
    pixel size: 1/128
  z := 0
  iterate z -> z^2 + C until z escapes
  color <- if z escaped then white else black

  @viewer "Julia set"
  setup:
    show if: selected_C
    pixel size: 1/128
  iterate z -> z^2 + C until z escapes
  color <- if z escaped then white else black

  @tool "Pick C" for "Mandelbrot set"
  on click:
    C <- click
    selected_C <- true
