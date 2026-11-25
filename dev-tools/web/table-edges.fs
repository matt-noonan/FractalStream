# For dev-tools/web/check-tools.mjs's `tableEdges` section: green if a
# point on each edge of a table is in its edge cell (and one just past
# them is off it), red if not.
format: 1
script: |
  @config "P"
  T : Table 2 of R

  @viewer "V"
  corners := table_cell(T, 0 + 2i) = (1, 1) and table_cell(T, 4 + 0i) = (4, 2)
  edges := corners and (0 + 0i) in T and (4 + 2i) in T and (4 + 1i) in T and (2 + 0i) in T
  past := not ((4.01 + 1i) in T) and not ((2 - 0.01i) in T)
  inside := table_cell(T, 1 + 1i) = (2, 2)
  color <- if edges and past and inside then green else red

  @tool overlay for "V"
  on refresh:
    if table_size(T) = (0, 0):
      reset T to (4, 2) from 0 + 2i to 4 + 0i
