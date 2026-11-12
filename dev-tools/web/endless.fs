# For dev-tools/web/check-tools.mjs's `stop` section: handlers that never
# finish (a click, an `on refresh:`, and an overlay, once `hang` is set),
# and one that does (Count), to see that clicks work after a stop. (A
# `while` loop stops at the iteration limit unless it says `up to N times`;
# nested, these two never finish in practice.)
script: |
  @config "Configuration"
  count : Z := 0
  hang : Boolean as "Hang the overlay" := false

  @viewer "V"
  color <- grey

  @viewer "W"
  color <- grey

  @tool overlay for="W"
  on refresh:
    erase
    x : R := 0
    while hang and x >= 0 up to 2000000000 times:
      while x >= 0 up to 2000000000 times:
        x <- x + 1
    draw point at 0

  @tool "Loop" for="V" shortcut=l
  on click:
    count <- 100
    x : R := 0
    while x >= 0 up to 2000000000 times:
      while x >= 0 up to 2000000000 times:
        x <- x + 1
    draw point at 0

  @tool "Count" for="V" shortcut=c
  on click:
    count <- count + 1

  @tool "Slow refresh" for="V" shortcut=s
  on refresh:
    x : R := 0
    while x >= 0 up to 2000000000 times:
      while x >= 0 up to 2000000000 times:
        x <- x + 1
