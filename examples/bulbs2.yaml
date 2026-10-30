script: |
  @common
  let f(z) = z^2 + C

  @viewer "Mandelbrot set" var=C
  z := 0
  iterate z -> f(z) until z escapes

  if z escaped:
    color <- arenberg (iterations / 20)
  else:
    color <- black

  @tool "Bulb info" for="Mandelbrot set" shortcut=b
  text "Click on points of the Mandelbrot set to find the period of the attracting cycle."
  text "Points outside of the Mandelbrot set will result in \"X\""
  text "Cycles exceeding the maximum period will result in \"?\""
  warmup : Z as "Warmup iterations" <- 10000
  max_period : Z as "Maximum period" <- 1000
  tol : R as "Tolerance when checking for cycles" <- 0.0001
  button "Erase annotations"
  on button "Erase annotations":
    erase

  on click:
    z := 0
    iterate z -> f(z) until z escapes up to warmup times

    if not (z escaped):
      target := z
      repeat up to max_period times:
        z <- f(z)
      until |z - target| < tol
      if stuck:
        write "?" at C
      else:
        write iterations at C
    else:
      write "X" at C
