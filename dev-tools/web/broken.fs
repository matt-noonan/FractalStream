# For dev-tools/web/check-tools.mjs's `opening` section: a project whose
# script doesn't build (the stray `(` ending its last line), with a saved value and
# a saved window, which it should get once it's fixed and rebuilt.
script: |
  @config "Configuration"
  shade : R := 0.5

  @viewer "V" center=0 pixelSize=1/64
  color <- if shade > 0.3 then white else black (
values:
  config:
    shade: '0.25'
web-properties:
  viewers:
    V: { x: 300, y: 40, width: 220, height: 160, cx: 0, cy: 0, pixel-size: 0.015625 }
