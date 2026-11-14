script: |
  # A viewer with a group (group=modular), for dev-tools/web/check-tools.mjs:
  # the modular group's domain red, the tiles one step away blue, the rest
  # grey, edges black. "Dot" marks a point, in every tile.
  @viewer "Group" view=halfplane group=modular
  color <- if side_distance < 1.5 pixel_size / im(z) then black ...
           else if fold_count = 0 then red else if fold_count = 1 then blue else grey

  @tool "Dot" for="Group" shortcut=d
  on click at p:
    erase
    use rgb(0, 1, 0) for fill
    use rgb(0, 1, 0) for stroke
    draw filled circle at p with radius 0.05
