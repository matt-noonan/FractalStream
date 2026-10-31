script: |
  @config "Configuration"
  x : C as "Selected value of x:" <- -0.1226 + 0.7449i

  @viewer "x plane" var=x
  color <- pink

  @viewer "z plane"
  color <- yellow
