script: |
  @config "Configuration"
  myList : List of C as "Centers" <- [0, 1, i]

  @viewer "Circle"
  if |z| < 1:
    color <- red
  else if im z > 0:
    color <- green
  else:
    color <- blue

  @tool "Show circles" shortcut=s
  on refresh:
    erase
    for each center in myList:
      draw circle at center with radius 1
