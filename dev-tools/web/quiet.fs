# For dev-tools/web/check-tools.mjs's `sweep` section: a project with
# nothing to redraw once it's drawn (no tools, no overlay, no timers), to
# open after the library's tools, so that a drawing arriving afterwards
# can only be from a tool left running on a closed project.
format: 1
script: |
  @viewer "Quiet" over z
  color <- grey
