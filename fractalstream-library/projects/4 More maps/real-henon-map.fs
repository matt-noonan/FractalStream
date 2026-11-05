script: |
  @config "Hénon map"
  a : R as "a =" := 1.4
  b : R as "b =" := 0.3

  @config "Parameter plane"
  settle : Z as "Settling iterations" := 500
  max_period : Z as "Longest period" := 64
  tolerance : R as "Tolerance" := 0.000001

  @common
  # One step of the Hénon map, (x, y) ↦ (1 - a x² + y, b x), updating the
  # x and y where it's run.
  define step:
    x, y <- 1 - a x² + y, b x

  @viewer "Parameter plane" xvar=a yvar=b center=(0.6, 0) pixelSize=1/250 aspect=1/2
  # Follow the orbit of (0, 0) until it settles down; then look for the
  # period of the cycle it has settled onto.
  x, y := 0, 0
  iterate (x, y) -> (1 - a x² + y, b x) until (x, y) escapes up to settle times
  if not stuck:
    color <- cycle(iterations / 20, dark blue, white)
  else:
    x0, y0 := x, y
    repeat up to max_period times:
      run step
    until (x - x0)² + (y - y0)² < tolerance² or (x, y) escapes
    if (x, y) escapes:
      color <- dark blue
    else if stuck:
      color <- dark grey
    else:
      # periods 1, 2, 4, 8, ... a sixth of the way around the color wheel apart
      color <- rainbow(log(iterations) / log(2) / 6)

  @viewer "Dynamical plane" xvar=x yvar=y center=(0, 0) pixelSize=1/200 aspect=1/2
  iterate (x, y) -> (1 - a x² + y, b x) until (x, y) escapes
  color <- if stuck then black else cycle(iterations / 10, dark blue, white)

  @tool "Attractor" for="Dynamical plane" shortcut=a
  points : Z as "Points" := 5000
  on refresh:
    erase
    use orange for stroke
    x, y := 0, 0
    # (skipping the first few points, on their way to the attractor)
    n := 0
    repeat up to points + 100 times:
      run step
      n <- n + 1
      if n > 100:
        draw point at (x, y)
    until (x, y) escapes
  on deactivated:
    erase

  @tool "Orbit" for="Dynamical plane" shortcut=o
  orbit_length : Z as "Orbit length" := 1000
  on click-or-drag at (x, y):
    erase
    use white for stroke
    draw point at (x, y)
    use yellow for stroke
    repeat up to orbit_length times:
      run step
      draw point at (x, y)
    until (x, y) escapes
  on deactivated:
    erase
metadata:
  title: The real Hénon map
  description: |
    The Hénon map (x, y) ↦ (1 - ax² + y, bx), with real parameters. In the
    parameter plane, each (a, b) is colored by the period of the cycle the
    orbit of (0, 0) settles onto (dark grey for no cycle found, blue for
    escaping); click there to choose a and b. The dynamical plane shows
    which points escape, and its tools draw the strange attractor and the
    orbit of a point.
  preview-commands: |
    select viewer "Dynamical plane"
    select tool "Attractor"
    trigger refresh
    capture viewer "Dynamical plane" as preview
  preview: |
    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
