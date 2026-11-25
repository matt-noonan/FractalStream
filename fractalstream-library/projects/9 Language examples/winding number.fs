format: 1
script: |-
  @common
  path(t) ≔ (1 + 2 cos(2 π t)) e^{2 π i t} - 1

  @config "Winding number integration"
  steps : ℤ as "Integration steps:" ≔ 100

  @viewer "z plane"

  n ≔ winding_number(path, z, steps)

  if n = 0:
    color ⟵ yellow
  else if n = 1:
    color ⟵ green
  else if n = 2:
    color ⟵ blue
  else:
    color ⟵ red

  @tool overlay for "z plane"
  on refresh:
    erase
    use black for line
    t, dt ≔ 0, 1 / steps
    draw line from 2 to 1.9 - 0.3i
    draw line from 2 to 2.1 - 0.3i
    repeat steps times:
      draw line from path(t) to path(t + dt)
      t ⟵ t + dt
metadata:
  title: Computing winding numbers
  description: |
    Colors each point z by how many times the closed path
    t ⟼ (1 + 2 cos 2πt) e^(2πit) - 1, a limaçon with an inner loop drawn over the
    picture, winds around it: yellow for 0, green for 1, blue for 2. The standard
    library's `winding_number(path, z, steps)` integrates 1/(w - z) along the
    path, in `steps` steps: its imaginary part is 2π times the number of turns.
    With too few steps, points close to the path come out wrong.
  preview-commands: |
    select viewer "z plane"
    capture viewer "z plane" as preview
  preview: |
    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
    AAAAAElFTkSuQmCC
