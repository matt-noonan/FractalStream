script: |
  @config "Configuration"
  C : C as "f(z) = z² +" := -0.11 + 0.78i

  @viewer "Dynamical plane" center=0 pixelSize=1/128 colorizer="Binary decomposition"
  coloring:
    period <- 1
    smooth <- false
    upper2 <- rgb(0.067, 0.502, 0.008)
    lower2 <- rgb(0.502, 0.000, 0.502)
  iterate z ⟼ z² + C until z escapes

  @viewer "Parameter plane" var=C center=0 pixelSize=1/128 colorizer="Binary decomposition"
  coloring:
    period <- 1
    smooth <- false
    upper2 <- rgb(0.067, 0.502, 0.008)
    lower2 <- rgb(0.502, 0.000, 0.502)
  z ≔ 0
  iterate z ⟼ z² + C until z escapes

  @tool "Parameter plane external rays" for="Parameter plane" shortcut=r
  row:
    ps : List of Z as "Numerators: " := [0, 1, 2]
    q : Z as "Denominator: " := 3
  S : Z as "Subdivisions per level set: " := 10
  error : R as "Step size" := 0.0001
  iter_limit : Z as "Newton iteration limit" := 100
  on refresh:
    erase
    for each p in ps:
      R ≔ escape_radius
      theta ≔ p / q
      z ≔ R e^{2 π 𝑖 theta}
      dist ≔ error
      k ≔ 0
      while dist >= error:
        k ⭠ k + 1
        j ≔ 0
        while j < S and dist >= error:
          j ⭠ j + 1
          m ≔ (k - 1) S + j
          r_m ≔ R^(2^(-m / S))
          t_m ≔ r_m^(2^k) e^{2^{k+1} π 𝑖 theta}
          temp_c ≔ z
          difference ≔ error
          C_k ≔ temp_c
          D_k ≔ 1
          n ≔ 0
          while difference >= error up to iter_limit times:
            n ⭠ n + 1
            old_c ≔ temp_c
            C_k ⭠ old_c
            D_k ⭠ 1
            while true up to k times:
              D_k ⭠ 2 D_k C_k + 1
              C_k ⭠ C_k² + old_c
            temp_c ⭠ old_c - (C_k - t_m) / D_k
            difference ⭠ |old_c - temp_c|
          if difference < error:
            dist ⭠ 2 |C_k| log |C_k| / |D_k|
            draw line from z to temp_c
            z ⭠ temp_c
          else:
            dist ⭠ 0
            draw filled circle at z with radius error
  on deactivated:
    erase

  @tool "Dynamical plane external rays" for="Dynamical plane" shortcut=r
  row:
    ps : List of Z as "Numerators:" := [1, 2, 4]
    q : Z as "Denominator:" := 7
  S : Z as "Subdivisions per level set: " := 10
  error : R as "Step size" := 0.0001
  iter_limit : Z as "Newton iteration limit" := 100
  on refresh:
    erase
    for each p in ps:
      R ≔ escape_radius
      theta ≔ p / q
      z ≔ R e^{2 π 𝑖 theta}
      dist ≔ error
      k ≔ 0
      while dist >= error:
        k ⭠ k + 1
        j ≔ 0
        while dist >= error up to S times:
          j ⭠ j + 1
          m ≔ (k - 1) S + j
          r_m ≔ R^(2^(-m / S))
          t_m ≔ r_m^(2^k) e^{2^{k+1} π 𝑖 theta}
          difference ≔ error
          temp_z ≔ z
          z_k ≔ 0
          dz_k ≔ 0
          while difference >= error up to iter_limit times:
            old_z ≔ temp_z
            z_k ⭠ temp_z
            dz_k ⭠ 1
            t ≔ 0
            while t < k:
              t ⭠ t + 1
              dz_k ⭠ 2 dz_k z_k
              z_k ⭠ z_k² + C
            temp_z ⭠ old_z - (z_k - t_m) / dz_k
            difference ⭠ |old_z - temp_z|
          if difference < error:
            dist ⭠ |z - temp_z|
            draw line from z to temp_z
            z ⭠ temp_z
          else:
            dist ⭠ 0
            draw filled circle at z with radius error
  on deactivated:
    erase
metadata:
  title: External rays
  description: |
    Draws external rays of the Mandelbrot set and of Julia sets, at
    angles p/q given in the tools' configuration. Rays with angle p/q land
    at the roots of the corresponding bulbs, so they're a good way to see
    how the Mandelbrot set is organized.
  preview-commands: |
    select viewer "Parameter plane"
    select tool "Parameter plane external rays"
    trigger refresh
    capture viewer "Parameter plane" as preview
  preview: |
    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
    Bru2ZJv3AAAAAElFTkSuQmCC
