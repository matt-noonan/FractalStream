configuration:
  size: 400x200
  title: Configuration
  vertical-contents:
  - text-entry:
      label: 'Selected value of C:'
      type: C
      value: "1.41406250000000 - 0.78906250000000\U0001D456"
      variable: C
  - text-entry:
      label: 'Max. iterations: '
      type: Z
      value: 100
      variable: maxIters
  - text-entry:
      label: 'Max. radius: '
      type: R
      value: 10
      variable: maxRadius
setup:
  size: 200x200
  title: Parametric complex dynamics
  vertical-contents:
  - text-entry:
      environment:
        C: ℂ
        z: ℂ
      label: 'Iterate f(z) = '
      type: C
      value: z^2 + C
      variable: f
  - text-entry:
      environment:
        C: ℂ
        maxRadius: ℝ
        z: ℂ
      label: until
      type: Boolean
      value: '|z| > maxRadius'
      variable: stop
viewers:
- code: "z : C <- 0\niterate z -> «f» until «stop»\n\nif «stop»:\n   color <- rainbow(iterations/20)\n
    \ \nelse:\n  color <- black\n"
  escape-radius: maxRadius
  iteration-limit: maxIters
  position: 36x110
  size: 569x586
  title: Parameter plane
  z-coord: C
- code: |
    iterate z -> «f» until «stop»

    if «stop»:
       color <- rainbow(iterations/20)
    else:
      color <- black
  escape-radius: maxRadius
  iteration-limit: maxIters
  position: 606x135
  size: 627x561
  title: Dynamical plane
  tools:
  - actions:
    - code: erase
      event: deactivated
    - code: |
        prevZ : C <- z
        erase
        until «stop»:
          prevZ <- z
          draw point at z
          z <- «f»
          draw line from z to prevZ
      event: click-or-drag
    name: Trace
    shortcut: t
  z-coord: z
