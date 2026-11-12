# For dev-tools/web/check-tools.mjs's `progressive` section: a viewer slow
# enough (a second or two) to watch its render go, coarse to fine.
script: |
  @config "Configuration"
  work : Z := 1000

  @viewer "Slow" var=z center=0 pixelSize=1/128 limit=work
  s := 0
  k := 0
  while k < work:
    s <- s + sin(re(z) + k)
    k <- k + 1
  color <- if s + 4 im(z) > 0 then rgb(0.9, 0.4, 0.2) else rgb(0.2, 0.3, 0.8)
