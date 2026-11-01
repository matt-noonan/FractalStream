script: |
  @config "Configuration"
  C : C as "f(z) = z² +" <- -0.11 + 0.78i

  @common
  define draw:
    iterate z ⟼ z² + C until z escapes

    color ⭠ black
    if z escaped:
      m ≔ iterations + 1 ...
           - log (log (|z|²) / (2 log escape_radius)) / log 2
      base_color ≔ if im z > 0 then dark purple else dark green
      color ⭠ blend (cos (π m / 20)², base_color, light base_color)

  @viewer "Parameter plane" var=C center=0 pixelSize=1/128
  z ≔ 0
  run draw

  @tool "Parameter plane external rays" for="Parameter plane" shortcut=r
  row:
    ps : List of Z as "Numerators: " <- [0, 1, 2]
    q : Z as "Denominator: " <- 3
  S : Z as "Subdivisions per level set: " <- 10
  error : R as "Step size" <- 0.0001
  iter_limit : Z as "Newton iteration limit" <- 100
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

  @viewer "Dynamical plane" center=0 pixelSize=1/128
  run draw

  @tool "Dynamical plane external rays" for="Dynamical plane" shortcut=r
  row:
    ps : List of Z as "Numerators:" <- [1, 2, 4]
    q : Z as "Denominator:" <- 7
  S : Z as "Subdivisions per level set: " <- 10
  error : R as "Step size" <- 0.0001
  iter_limit : Z as "Newton iteration limit" <- 100
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
    gg==
