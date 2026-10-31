script: |
  @config "Input θ = p / q for S_θ"
  row:
    p : Z as "" <- 9
    text "/"
    q : Z as "" <- 56

  @config "Computed values"
  kneading_seq : Text as "Kneading sequence" <- "42"
  iters : List of Z as "Kneading sequence for the script's eyes only:" <- []
  row:
    μ : Z as "Preperiod" <- 0
    λ : Z as "Period" <- 0

  @config "Configuration"
  next_leg : Z <- 0
  spider : List of List of C as "Current spider representative" <- []
  spider_ready : Boolean as "Spider representative ready?" <- false
  contour : List of C as "γ =" <- []
  encloses_zero : Boolean as "Encloses 0?" <- false
  foot : R as "Foot radius:" <- 0.03

  @viewer "Standard spider" center=0 pixelSize=1/128
  color <- white

  @tool "Draw standard spider" for="Standard spider" shortcut=s refreshCanUpdate=true
  on refresh:
    # Save p and q to restore later, so we can manipulate
    # them in the script without changing the user's input
    savedP := p
    savedQ := q

    h2 := p
    h1 := p + q
    p <- 2 p
    q <- 2 q

    erase
    use grey for line
    draw circle at 0 with radius 1

    star1 := e^{2 pi i h1 / q}
    star2 := e^{2 pi i h2 / q}
    draw line from star1 to star2
    use black for line
    write "A" at i star2 / 2
    write "B" at -i star2 / 2

    # Brent's cycle detection algorithm
    power := 1
    λ <- 1
    tortoise := p
    hare := mod (2 p, q)
    while tortoise != hare:
      if power = λ:
        tortoise <- hare
        power <- 2 power
        λ <- 0
      hare <- mod (2 hare, q)
      λ <- λ + 1

    tortoise <- p
    hare <- p
    iterate hare -> mod (2 hare, q) while true up to λ times
    μ <- 0
    while tortoise != hare:
      tortoise <- mod (2 tortoise, q)
      hare <- mod (2 hare, q)
      μ <- μ + 1

    # Now λ tells us the length of the periodic part,
    # and μ tells us the length of the preperiodic part.
    delta := 1 / (λ + μ)
    ci := 0

    t := p

    # We'll track the kneading sequence in a computing-friendly
    # form here, while also accumulating a human-friendly form
    # into `kneading_seq`
    iters <- []
    S1 := 0
    S2 := 1
    A  : Z <- 2
    B  : Z <- 3

    kneading_seq <- ""

    t <- p
    while true up to μ times:
      omega := e^{2 pi i t / q}
      c := rainbow(ci)
      ci <- ci + delta
      use dark c for line
      use light c for fill

      if t = h1:
        iters <- append(iters, S1)
        kneading_seq <- text (kneading_seq, "★₁")
      else if t = h2:
        iters <- append(iters, S2)
        kneading_seq <- text (kneading_seq, "★₂")
      else if re (i star2 bar(omega)) > 0:
        iters <- append(iters, A)
        kneading_seq <- text (kneading_seq, "A")
      else:
        iters <- append(iters, B)
        kneading_seq <- text (kneading_seq, "B")

      draw line from omega to 10 omega
      draw filled circle at omega with radius foot
      t <- mod (2 t, q)

    kneading_seq <- text (kneading_seq, "[")

    while true up to λ times:
      omega := e^{2 pi i t / q}
      if t = h1:
        iters <- append(iters, S1)
        kneading_seq <- text (kneading_seq, "★₁")
      else if t = h2:
        iters <- append(iters, S2)
        kneading_seq <- text (kneading_seq, "★₂")
      else if re (i star2 bar(omega)) > 0:
        iters <- append(iters, A)
        kneading_seq <- text (kneading_seq, "A")
      else:
        iters <- append(iters, B)
        kneading_seq <- text (kneading_seq, "B")

      c := rainbow(ci)
      ci <- ci + delta
      use dark c for line
      use light c for fill

      draw line from omega to 10 omega
      draw filled circle at omega with radius foot
      t <- mod (2 t, q)

    kneading_seq <- text (kneading_seq, "]")

    spider <- []
    spider_ready <- false

    p <- savedP
    q <- savedQ

  @viewer "Iterated spider" center=0 pixelSize=1/128
  color <- white

  @tool "Reset spider" for="Iterated spider" shortcut=r
  button "Apply spider map"
  on refresh:
    if not spider_ready:
      erase

  on activated:
    erase
    spider <- []
    next_leg <- 0
    R : R <- 10

    # Move the first foot of the spider to 0
    b := -e^{2 pi i p / q}

    # Subdivide the legs of the standard spider, then twist
    omega := e^{2 pi i p / q}

    delta := 1 / (λ + μ)
    ci := 0

    leg : List of C <- []

    while true up to μ + λ times:
      zi := omega + b

      c := rainbow(ci)
      ci <- ci + delta
      use dark c for line
      use light c for fill

      # Make a curve from R omega to z
      t := 0
      N := 100
      dt := 1 / N
      last_gamma := R omega
      draw line from 20 omega to last_gamma
      leg <- []
      while true up to N times:
        t <- t + dt
        gamma := R^{1 - t} omega + b t
        leg <- prepend(leg, last_gamma)
        draw line from last_gamma to gamma
        last_gamma <- gamma

      draw filled circle at zi with radius foot
      omega <- omega^2

      spider <- append(spider, leg)

    # Reset omega
    omega <- e^{2 pi i p / q}

    z2 := omega^2 + b
    # P(z) = z2 (1 + z/2)^2
    # -2 +/- 2 sqrt (w / z2)
    leg1 := spider @ 1
    last_gamma := leg1 @ 1
    use light grey for line

    # Construct the inverse image of \gamma_1 under
    # $P(z) = z_2 (1 - z/2)^2$. Since
    # \gamma_1 starts at z_1 = 0 and 0 is the critical
    # value of $P$, the inverse image of \gamma_1 is
    # a curve that splits \hat{C} in half, passing through
    # infinity and -2. We'll just compute a hopefully-big-
    # enough portion of P^{-1}(\gamma_1) and then complete
    # it into a contour by adding rectangular edges, like
    # this:
    #   ____________________________________
    #   \                                   |
    #    \__                                |
    #       \                               |
    #        \______________________________|
    #
    contour <- []

    # Compute the first half of P^{-1}(\gamma_1)
    last_pre := -2 + 2 sqrt(last_gamma / z2)
    for each pt in leg1:
      pre1 := -2 + 2 sqrt(pt / z2)
      pre2 := -2 - 2 sqrt(pt / z2)
      pre := if |pre1 - last_pre| < |pre2 - last_pre| ...
        then pre1 else pre2
      contour <- prepend(contour, pre)
      last_pre <- pre

    # Compute the second half of P^{-1}(\gamma_1)
    last_pre <- -2 - 2 sqrt(last_gamma / z2)
    t := 0

    for each pt in leg1:
      pre1 := -2 + 2 sqrt(pt / z2)
      pre2 := -2 - 2 sqrt(pt / z2)
      pre := if |pre1 - last_pre| < |pre2 - last_pre| ...
        then pre1 else pre2
      contour <- append(contour, pre)
      last_pre <- pre

    first_pt := contour @ 1
    last_pt := contour @ 200
    contour <- join(contour, ...
      [10 + i im last_pt, 10 + i im first_pt, first_pt])

    # Draw the contour in grey, with -2 marked
    use light grey for line
    t <- 0
    first := true
    for each pt in contour:
      if first:
        first <- false
        first_pt <- pt
      else:
        t <- t + 0.005
        draw line from pt to last_pt
      last_pt <- pt

    # Determine if the contour encloses 0 or not
    lastPt := 0
    first <- true
    integral := 0
    for each pt in contour:
      if first:
        first <- false
      else:
        dz := pt - lastPt
        integral <- integral + dz / lastPt
      lastPt <- pt
    encloses_zero <- im integral > 0

    # Pull each path back
    use light grey for fill
    draw point at -2
    write "-2" at -2.2
    write "γ" at first_pt - 0.15i

    spider_ready <- true

  on button "Apply spider map":
    erase

    new_spider : List of List of C <- []
    z_2 := (spider @ 2) @ 1

    j := 1
    while true up to μ + λ times:
      # To compute \tilde{z}_j and \tilde{\gamma}_j,
      # we need to pick one of the two preimages of
      # z_{j+1} and \gamma_{j+1} under P(z) = z_2 (1 - z/2)^2
      #
      gamma_jp1 := spider @@ (j + 1)
      z_jp1 := gamma_jp1 @ 1

      # See which preimage -2 +/- 2 sqrt(z_{j+1} / z_2) is
      # inside the contour
      w := -2 + 2 sqrt(z_jp1 / z_2)

      integral := 0
      last_zeta := 0
      first := true
      for each zeta in contour:
        if first:
          first <- false
        else:
          dzeta := zeta - last_zeta
          integral <- integral + dzeta / (zeta - w)
        last_zeta <- zeta

      zero_winding := if encloses_zero then 1 else -1
      sqrt_sign := if im integral > 0 then 1 else -1

      # Determine if we should pick the preimage inside or outside
      # the contour based on the jth term of the kneading sequence.
      # If it was A, pick the preimage with sqrt_sign. If it was
      # B, pick the preimage with negated sqrt_sign.
      # TODO: deal with *s in the kneading sequence
      S1 := 0
      S2 := 1
      A  : Z <- 2
      B  : Z <- 3
      symbol := iters @ j
      last_pre := 0
      if symbol = A:
        last_pre <- -2 + 2 sqrt_sign zero_winding sqrt(z_jp1 / z_2)
      else if symbol = B:
        last_pre <- -2 - 2 sqrt_sign zero_winding sqrt(z_jp1 / z_2)

      # Try to rescale the new spider so that it is roughly
      # the same size as the old one
      scale := sqrt(10)

      new_leg := [scale last_pre]
      first <- true
      for each pt in gamma_jp1:
        if first:
          first <- false
        else:
          pre1 := -2 + 2 sqrt(pt / z_2)
          pre2 := -2 - 2 sqrt(pt / z_2)
          pre := if |last_pre - pre1| < |last_pre - pre2| ...
            then pre1 else pre2
          new_leg <- append(new_leg, scale pre)
          last_pre <- pre

      # Add one more segment to the leg so that we reach at least
      # radius 10
      if |last_pre| < 10:
        new_leg <- append(new_leg, scale 10 last_pre / |last_pre|)
      new_spider <- append(new_spider, new_leg)
      j <- j + 1

    spider <- new_spider
    # Now redraw the spider

    delta := 1 / (λ + μ)
    ci := 0

    for each leg in spider:
      c := rainbow(ci)
      ci <- ci + delta
      use dark c for line
      use light c for fill

      z_j := leg @ 1
      last_pt := z_j
      for each pt in leg:
        draw line from last_pt to pt
        last_pt <- pt
      draw filled circle at z_j with radius foot

    # Recompute the contour \gamma
    leg1 := spider @ 1
    last_gamma := leg1 @ 2
    use light grey for line
    z2 := (spider @ 2) @ 1

    contour <- [-2]

    # Compute the first half of P^{-1}(\gamma_1)
    last_pre := -2 + 2 sqrt(last_gamma / z2)
    first := true
    for each pt in leg1:
      if first:
        first <- false
      else:
        pre1 := -2 + 2 sqrt(pt / z2)
        pre2 := -2 - 2 sqrt(pt / z2)
        pre := if |pre1 - last_pre| < |pre2 - last_pre| ...
          then pre1 else pre2
        contour <- prepend(contour, pre)
        last_pre <- pre

    # Compute the second half of P^{-1}(\gamma_1)
    last_pre <- -2 - 2 sqrt(last_gamma / z2)
    t := 0

    first <- true
    for each pt in leg1:
      if first:
        first <- false
      else:
        pre1 := -2 + 2 sqrt(pt / z2)
        pre2 := -2 - 2 sqrt(pt / z2)
        pre := if |pre1 - last_pre| < |pre2 - last_pre| ...
          then pre1 else pre2
        contour <- append(contour, pre)
        last_pre <- pre

    first_pt := contour @ 1
    last_pt := contour @ -1
    contour <- join(contour, ...
      [10 + i im last_pt, 10 + i im first_pt, first_pt])

    # Draw the contour in grey, with -2 marked
    use light grey for line
    t <- 0
    first <- true
    for each pt in contour:
      if first:
        first <- false
        first_pt <- pt
      else:
        t <- t + 0.005
        draw line from pt to last_pt
      last_pt <- pt

    # Determine if the contour encloses 0 or not
    lastPt := 0
    first <- true
    integral := 0
    for each pt in contour:
      if first:
        first <- false
      else:
        dz := pt - lastPt
        integral <- integral + dz / lastPt
      lastPt <- pt
    encloses_zero <- im integral > 0

    # Pull each path back
    use light grey for fill
    draw point at -2
    write "-2" at -2.2
    write "γ" at first_pt - 0.15i

wx-properties:
  viewers:
    "Standard spider":
      width: 512
      height: 512
      resizable: false
    "Iterated spider":
      width: 512
      height: 512
      resizable: false
