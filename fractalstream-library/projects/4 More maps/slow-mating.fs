script: |
  @config "Slow mating"
  c1 : C as "First polynomial z² + c1, c1 =" <- -0.122561 + 0.744862i
  c2 : C as "Second polynomial w² + c2, c2 =" <- -1
  t : R as "Mating time t =" <- 0.5
  text "Each Julia set is cut along its equipotential of level t, and the two are glued there, as the equator of a sphere. As t goes to 0 this approaches the mating."
  row:
    color1 : Color as "First Julia set" <- rgb(1, 0.8, 0.3)
    color2 : Color as "Second Julia set" <- rgb(0.15, 0.3, 0.75)
  show_bands : Boolean as "Shade bands between preimages of the equator" <- true
  show_equator : Boolean as "Draw the equator" <- true

  @common
  # The slow mating of P1(z) = z² + c1 and P2(w) = w² + c2 (Milnor's
  # "Pasting together Julia sets"; Chéritat's slow mating movies).
  #
  # For R = e^s > 1, the sphere S_s is the filled Julia set K1 out to its
  # equipotential of level 2s, glued to K2 out to the same level, by
  # φ1(z) φ2(w) = R² (φ the Böttcher coordinates). Its equator is the
  # level-s equipotential of each side. P1 and P2 together are a degree-2
  # map S_s → S_2s. Uniformize each S_s by the Riemann sphere, with the
  # critical point 0 of P1 at 0 and that of P2 at ∞. In these coordinates
  # the map S_s → S_2s is a rational map
  #     Q_s(u) = (u² + A) / (B u² + 1),
  # where A is the point c1 of S_2s, and 1/B the point c2. For large s,
  # u ≈ z / R on side 1 and u ≈ R / w on side 2, which is where we start:
  # at a level 2^L t ≥ 30, every point of the postcritical orbits is
  # where that approximation puts it. Each level's points are then pulled
  # back to the level below through that level's map, taking the square
  # root nearest the same approximation (this agrees with following the
  # roots continuously as t decreases from ∞).
  #
  # A pixel u of S_t is pushed forward through the maps Q_t, Q_2t, ...,
  # to the top level, where it is a point of one of the two planes; there
  # P1 or P2 tells whether it lies in its filled Julia set, and if not, its
  # potential (on the overlap G1 + G2 = 2t, and the equator is G1 = G2 = t).
  define mating:
    tt := if t < 0.0001 then 0.0001 else t
    # levels 0 .. L, the level-k sphere being S at 2^k tt
    L := 1
    top := 2 tt
    while top < 30 up to 64 times:
      L <- L + 1
      top <- 2 top
    # the critical orbits c1, P1(c1), ... and c2, P2(c2), ...
    xs := [c1]
    ys := [c2]
    x := c1
    y := c2
    for each n in range(0, L):  # L + 2 points: the top level needs them all
      x <- x^2 + c1
      y <- y^2 + c2
      xs <- append(xs, x)
      ys <- append(ys, y)
    # points of the orbits on the top sphere: u ≈ z/R, 1/u ≈ w/R
    a := transform(xs, p -> p exp(-top))
    b := transform(ys, p -> p exp(-top))
    As : List of C <- []
    Bs : List of C <- []
    s := top
    k := L - 1
    while k >= 1 up to 64 times:
      s <- s / 2
      A := a @ 1
      B := b @ 1
      na : List of C <- []
      nb : List of C <- []
      for each j in range(1, k + 2):
        v := a @ (j + 1)
        r := sqrt((v - A) / (1 - B v))
        g := (xs @ j) exp(-s)
        if |r - g| > |r + g|:
          r <- -r
        na <- append(na, r)
        v <- b @ (j + 1)
        r <- sqrt((v - B) / (1 - A v))
        g <- (ys @ j) exp(-s)
        if |r - g| > |r + g|:
          r <- -r
        nb <- append(nb, r)
      As <- prepend(As, A)
      Bs <- prepend(Bs, B)
      a <- na
      b <- nb
      k <- k - 1
    As <- prepend(As, a @ 1)
    Bs <- prepend(Bs, b @ 1)
    # push the pixel up to the top level
    # (and its derivative, for drawing the equator at a constant width)
    u := z
    du : C <- 1
    for each n in range(1, L):
      u2 := u^2
      A := As @ n
      B := Bs @ n
      du <- 2 u (1 - A B) du / (B u2 + 1)^2
      u <- (u2 + A) / (B u2 + 1)
    # then follow it in its own plane
    first := |u| < 1
    p := if first then u exp(top) else exp(top) / u
    dp := if first then du exp(top) else -du * exp(top) / u^2
    cc := if first then c1 else c2
    until |p| > 1000:
      dp <- 2 p dp
      p <- p^2 + cc
    inside := stuck
    # the potential, as a fraction of the equator's: in (0, 1) on either side
    g := log(|p|) / exp((L + iterations) log 2) / tt
    # bands between the preimages of the equator, at levels t/2, t/4, ...
    band := floor(-log(g) / log 2)
    if inside:
      color <- if first then color1 else color2
    else:
      side := if first then color1 else color2
      color <- blend(g, blend(0.3, side, white), dark side)
      if show_bands and mod(band, 2) = 1:
        color <- blend(0.85, color, black)
      # |grad g|, and the size of a pixel in the plane at z
      dg := |dp| / (tt |p| exp((L + iterations) log 2))
      px := pixel_size (1 + |z|^2) / 2
      if show_equator and 1 - g < 1.5 px dg:
        color <- red

  @viewer "Slow mating" view=sphere center=1
  run mating

  @viewer "Mandelbrot set" var=c center=-0.6 pixelSize=1/180
  z := 0
  iterate z -> z^2 + c until z escapes

  # c1 and c2, always marked
  @tool overlay for="Mandelbrot set"
  on refresh:
    erase
    use black for stroke
    use color1 for fill
    draw filled circle at c1 with radius 5 pixel_size
    draw circle at c1 with radius 5 pixel_size
    use color2 for fill
    draw filled circle at c2 with radius 5 pixel_size
    draw circle at c2 with radius 5 pixel_size

  @tool "Pick c1" for="Mandelbrot set" shortcut=1
  on click-or-drag at w:
    c1 <- w

  @tool "Pick c2" for="Mandelbrot set" shortcut=2
  on click-or-drag at w:
    c2 <- w
metadata:
  title: Slow mating
  description: |
    The slow mating of two quadratic polynomials z² + c1 and w² + c2, on
    the Riemann sphere. Each filled Julia set is cut along its
    equipotential of level t and the two are glued there, along the
    sphere's equator (drawn in red); the result is shown in coordinates
    that make it a round sphere. As t decreases to 0 the picture
    approaches the mating of the two polynomials. Change t to watch it
    happen. Pick c1 (1) and Pick c2 (2) choose the polynomials in the
    Mandelbrot set, where both are always marked.
  preview-commands: |
    capture viewer "Slow mating" base as preview
  preview: |
    iVBORw0KGgoAAAANSUhEUgAAAIAAAACACAYAAADDPmHLAAAzDUlEQVR4nO1d93+XVbLef+IqTVFX
    d+8qBEhPqEqTElKoKkWKyq6C0kvoCRCSkN6AQAIkBAIJINIRLNixrSs2BEVFV929V/fevXevZe55
    5px53/O++SYE8oWAfn84n4SWXb/znJlnnpkz85uuXbtS6Px6z29a+/9A6IQAEDohAIROCAChEwJA
    6IQAEDohAIROCAChEwJA6IQAEDohAIROCAChEwJAS87Cx1No5YIHaHXqBNq3ZQGdP1VMX7y5gb56
    p5K+Pl1Nf/uwlv7z43r6/tyT9F/nD9A/Pz9MxdnzqSRnIZW1aUNTbr+91f8bQgBo5nl0wt00ffIA
    qi97kI5WTaP3Tqygj55fQ2dfyqVPXyumz99YT1/+eRN9/e5W+vb97fQfH+2i787upX98up8N/78X
    nqb/++sz9NOeQqLf/Ib+1i2eDjx3gZ488TntPnaeskv30aMzV1FC8gOt/t8aAoA5j0/sRbmLhtCO
    gtHK8BPoyNZH6Y0DqfTBs6vozAvZ9Mkr+erWl9AXb1m3/oMd9B9n6ui7c3vpv8T4X8L4z9JPe7Xx
    Xy2sp/3K+HuPf0Z1Rz+l2oNnadtTZ2jLng+oou492lD7Lk1+dDkNShjT6p/Brw4AMybGUunygVSb
    l0jb80ZSXclY2rN+Mh2tns63/sPnMujjF9fSJ68W0Gevlyrjlyvjb6ZvLON/D+Ofv1zj/4VKt/2Z
    ira+RXmVr1N2+Ws0dsoi6j94VKt/Nr9oAPzp/ijand+fDqxPoW05yVRbeB/Vl06gvRseolNPLaD3
    T6QZl5+jjF+ojF9GFxozfmM3v0gb/0ll/HoY/9BZqlHG37r3Q6qof4/K1c0vq1HGr3qL8ivfoLUb
    X6PM9a/QqpIXaUXhSfrTvI3Ud+CIVv+sflEAmDY2nKpWdacj5Ym0My9J3foRtLPoAdpdNpGeLH+Y
    Xt+/kN5/Jr1R4/vdfnNifv2xT2nnoXNUs/8MVT35IVXWv0/lO9+lddvfoeKqt6lg8xuUs+kUZW14
    lVaXvkTpRS/QsvznaNHaZ2j+mqdp0hNl1KdfSqt/dtc1AMqW9KJjZT1pT8EAOrAumarXJtGO/FG0
    q3gs7V43qQnji9tHzK+ibz8A4aszhO8p+u/PDtH/XDjGxv/xm+eJunejvyvjH3z+S9r3jCZ8uw6f
    o+0HPmbjb979Pm3cdZrWK+OXVL9NhVvepNwKuP5XKaPsZVpZ/CItL3ieluQ+SwuzTtDcjGM0M/0w
    TVt6gIbdv4Tieg5q9c/yugIADH+0JJ525fThW78jd5hy+SlUWzBGxftx2vgbH/EY/+MXc6yYv4G+
    fKdCsf0qZvt/V2z/P8/uoX98Yoz/xVH611cn6Adl/J+V8XH7D538kp565gva87Qy/pFPaIcyfvW+
    jzjub4Lxd0jcf5PjPlz/mnXa9acp1780z739s1cdpRlph+gxBYAxj5TRiClFNHDEAoruPrDVP9tr
    GgD33t2Flv2xK5Uvi+Fbf2xTCu3MT6Jtucr4hZbxy43xVcz/8HlD+Azb//xNner9VaV637xfQ3//
    cKfK83fT95/sU6TvoDL+EfrXl8fph6+fU3G/yCV9z36hSZ8yfiDGb8f9nI2nOO67rv95WpyD23+c
    b/+s9CP0+PKDNHl2DY1+pJSGKwAkPlhAgx/Iobh7xrX653xNAgDGr1kdQxUrutPh9UPZ5VdlJVBN
    7nCf8R+mU/sXKLavjP/cak71zr2cZ/L8dXTh7Y301V+20DfvbVNxv1bFfSPyfHrAS/pM3D8zabYb
    94+6cR+krxKkzxf3c03c97v+1OwTNM+5/dr9j59eSaMeLqHhkzUAhozNpQGjs6nHoJmt/nlfUwBY
    Nb0bbVgSRfV5fenIhmHM8quyhmnjK7evY/5E2lv+EB1Dqnd8uc7zT2bR2Zch8hQx6fPG/R1G6Nnj
    ifv/+soX909eatyH61fGN6xfXP+CzOM0Z7WO/bj9jyyopzFTy2jkQyWUMqmQhk3QABg4Zi3dM2IN
    9UzOpMi4wa3+2bcqAKaMjlHGD6eNyuXvzuunjJ9IB9n46ubnDGdxB8aHsrdnwxSV50+j008vU3F/
    JX10UsV9kL5XCrTQ86aK+3+uYNf/bUDXf9Rx/T/PncS3/xWf69+hXL8T9+tO04Yd3nzfSfmU608z
    rH9xjjK+uP6VR+iJFYes21+q4n8xJU1UABifzyEAAOg7MpN6Ja2mHsk5FNF74q8TADB+xdJuyvix
    nNvD+AeM8UH4wPZ3Ft3PeT5EHiF92vVnGNef73P9ku/X6pTvrJXvX9Cu/8dvTvpcf9P5fjHivkn5
    7Li/XOK+z/VPX6ZjP8jfyIeK+fbD/Q8dl0eD7s/hENB3RCb1Tl5N8Qnp1GtEKYX3nf/rAkDigG5U
    sSycSlOjjfGH0dMVI1jZq16bzOoeRB6J+3tV3D9ZN4tOG9f/0clM1vfdlA+3v9LL+j8W1n/QZf1f
    u6z/ILP+zzXr97v+nac57sP1F8D1N4j7J524D9Y/RxlfXP9U5frvU64fsR/sn2//hHx2//fet5b6
    j8riENBbeYDuCgA9EtdSfFIRhfWa8esAAG5+5fIIbXzl9g+vT1BsX8V6lepVZyequD/Cifvs+tdP
    oX0bp9K7Ty/llE9Y/zmwflPgufC2Yv2NEr8jjah9xvUf1a5/m+X6JeUrRMrXrLgP1q/TvnHi+gPc
    frj/fgoAdw/P4BDAAEhaSz1SSih6aD79Ie5Pv2wA4ObD+CXK+CB8hxTbP7YxWcX94VSVPczj+utK
    xzus/2TdbEX8VrjEz3P7yxvc/u8+tnP+Y07O31Dw8bL+JqVeK+4vaiLuI+1j1z8Zxtexf8gD5vbD
    /av43yclg3omrqL4oRoAPYeXUOywQup2by7dETn5lwkAGL8uO5ZKFkZRzZredGjdYDpankRPV46k
    rZlDWeWDxIuUj1m/5/Yv07cfRZ4X1jaI/Xz7Tzfz9jvE77wmfkbwEdbvuP6AUq/k+yruZ6i4r4wP
    wWf6sgNW3Ndpn+36B92/Vt/+kcr9D4f7z6Aew1ZS3JA06jNqHXuA2MQiihicR3f2XkF3xDz6ywIA
    Gz8rhsqXRlNFWnc6UHovHd6QwELPnuKkRm9/4Niv5d7zp0od5v+1j/n/QzF/T+z33P4LtO+E3H4h
    fkbwEde/RVz/xfN9kD4n5TM5f7IyfqIyvrh+if19VezH7e+VqN1//LAM6j2yjLqnaA8QMTifugxY
    S7fFzKZbw6f8MgAAgQeED3l+6aIY5fr7qds/pMHt98b+CXz7vczfFn2KTLFnY8NiTwDm77/9IvcK
    8avcrQs9ZTXvaNZvXL9IvRL3bdLHxrfyfSF9bHwT95H22a5fYj/f/qFp1Gt4IWcB8cnFFJNQwB6g
    y4Acuj12Lt0SPY9ujZx2/QMA0u72DOX6U7XrP1g6SBG/Yfr2FyWx4IPSrpf5T+QS775Nf3Tzftb7
    fZKvRf6cvP+cuv3nGzJ/+/aj0oe0T+ReIX6a9Yvr1ykfx30P6Wuc8YvxEyzjI+3rp4zPrj9Zx37c
    /u7q9vcZVUY9VPyPU+4/emgBhQ/KpTDlAQCAW6Nm0S2xS+nOLr2vXwBA4UPcL14QQWV8+xXxKxvC
    BR5O+xTzh/tn0cfK+13yN8sifxdP/b6zCj7/61T7dN7/kcr7JfZ7br9N/LZ6XX96sZvvL/Do/Ido
    6kJl/D+K8YsbGv9+y/iO61/lxP7e5vZ3V7cf7j9ySL4igTn0B8UBfhszRwMgJpXah8+nu7r1u/4A
    ILn+5rRovv3Q+A+U3Kvc/1B2/8cDuv8HjPv3Cz8+9/+G6/69jR4u+fOrfq7k67v9ltwLwYddvzR4
    mHyfxR6L8XvSvWYbX7t+MP/4BB37wf7jk+D+C7X7H5hDd+D2R8+mjhEzVRhIpY6xaXRzlyuXGVwR
    ACDuly0Mo/rsOL79iP3a/d+r836V+h2vHEVVmQmG/Wv3L+xfu/+pxv0Hzv297r/WkX1t98+av1H9
    nBavALdfcv7cCpf1w/XbcR+kT9K9QMYfGsjti/GTXONr17/Ovf3K/Uex+89j94/bf0vkTL79t8an
    MwDadp1LN4VdGcn4igBg5TR9+zn2L4xk91+zpg8dLBvkZf8s+yZr9++L/yj6nH5asX8T/8++2FD3
    d9h/A/fvJX+vFNax6oeCD/J++/brSp9N/MD6JeUzcV8qfMuMxv9I48bv78T8DI759s23jY/ULy6p
    iGIc95/LKSADIGoe3Ry1hDpELKAOKgS0CZtJ7WMyKSwm+K1mQQfAgymdadPSrrRtVZRy/zEGALFU
    k6kBANkXyp+kfzUe3d8v/iz3xP9PA3T7/A3dPh72b7v/yQyAA89rzV+rfh+7LV5y+43cK4IPWL/u
    7lFx31T4kO6Nf1wLPY7xJ1gqn6R6hu07hM8w/nif8eH6Eftx++H+uw5E/F/uAKBDxCJqH72Kb/8N
    nR6jm+JyqWPP9dc+ADYuCTPuP5a2pCsApHo9AOv+CgB7i5ON9IuS72ju8bMLPy8AANzhi/if5cT/
    zxH/37bjv3L/Z9yHHQHdv6R+h0X1M8zfuv2Qe5n4Wa4f+T7ivpR32fgPFbt5viPyZLPE29dy+cL2
    Qfh6JOUw4/cYP7GQolXqh9sP9o/8/464eXSbif/sAdStv7l7MbXtlkptusxhANzVK7iFo6ACALf/
    ydwodv/1a+M0AVQewOEAAgCTAVSvFe1/tCGAov49Qu8eW6o7fhQAPjYE8Lxf/bPivyP+GPZvu3+H
    /Jly72Yn79e3Xyp9IH4QfMT1S9yfunC3qe0r40/SxheFb8AYN97LrbddPnJ9TfhKWfBB4Qc33zY+
    bn+ne1Zz+scAAAeIS6c2nWfQ74aeYMPfGDaHfjtgN//6mgXAG9U9adMSLfyAAIIDgAQiC9hmNACU
    fQGArZlDNAHMDSD/MgFcavr9LAJ4qtjT8qXj/06n6eOfnx9i6feHr5912L/j/g3526rcP1Q/5P3F
    YP7W7RfiB9aPlG9Guo7746ZVsMrHxR3T2DHIMH1b4OmpDO/e+rXUZ6R16xXhQ76PmI+cXxs/j1M/
    3P7f91hEv42d43iAWxUA2nWbx64fRr+h0xMMBHwfTC8QNAA8MroLvVzZneM/AID8H16AQQAesDjW
    AoBf/7/P7frxZQCuACQEcL1FAN3nXaL+Sfx32b92/zsPu+QPmj/f/q1u7F9pbr80dorrl8YOaesS
    t99/tOvy7ViPJg/b3Xc3hpdbHzU0n2M+33xjfJA/vv0m/78ZAIhfqQlgl7nULmqlY3w51xwAvj46
    QHuApd2oYE5nDYIsXfrdokIBAwAhoDyRNYAqBYBtOUkBFcCnjALoyQC441eeeFV4CKB+5XNAN3xa
    8R/iDzp9bfcP8oeXPXbeL7d/Se5zTPyg9rHrl7iPrh6nrGvV9JNx63VVr2cjho8Rww8Rw+cx44fb
    t40P8ofbjxSwY9RcTgFvilyiSOA8lQXMaQCAYHmBoAAAbdyfHeinCGAX9gD5CgA4+L4qrRtzARSB
    DpYNNiLQKCsFHKkIoBcAbgFopVUAclu+3QxghyKAWgD6b0MA7fgP8QfNnmD/7P73fqjI33uc+qHc
    ix6/TJX3r7JuP4gf6vvTFOtn1/9QCZO+hPGa7SPNg/G5ng+XP0x9n1JoEbwiJngxfNu1q5cbbxse
    Mf/33Rd5jI/bzwIQNAAVAjqEL6AbO09jL4AQYAMgWF4gKACgU0OZ/QMAO9dEsNHxPQjhvtxozga2
    remlAcD1/xSvBoAKYMl40/gpErAGwBkDAEkBL5gUUBRAftptCKDIv3b8h/iz6/AnzP7Z/SvyV6rc
    Pzp9+Pavc28/5F4hfijvwvWnKNePsi7y/AG28VWs76nYfa8RuponOT1cfCCjhxmjI9UD22fDI+b7
    jH9zxAy6Tbl/kEDIwAIA6AB+AATDC7QYALj9cP+1GeGUN+tOBsK7tb3pcFEMewV4gfKl0gMwhAHg
    FoEaawCZ1YgGYL31e6/aZAD1JgPQ+r8mgJO5+AP1D/G/9tC5hu5fkT+ofsj7G9x+POiYWua4fu7m
    vU83c8Ltd+devkJm9lzIAaMfnG8MnuO55XDxIHgwuBidb3zMbDb8bcrwt0a6xof8i9sPBbBN5+ms
    AwgP8AMgGF6gxQB4rbo/GxoGh/HxFR4BB7+Pxk9uAsnsw3WAYxsD9wA0TwSCBlCuUsDNvhTQzgBc
    AvjUszr+o+WryrB/dv9b32LyB80fBR9m/lkn+PY/rm7/pFk1XORBypcwPp/jPvJ8ED6keCB60sUj
    Is6dfVbw7f5d91S6Pc5n7OjZ5pbrmy7nlshZOuYb498cOde5/TdFLlaGn8kawI3K/QcKATgtVQdb
    DAAYGjcet790fifH+PAKCAM1q6O5/29/yQDT/5dy+SqgEYFcAKgUEADgFFAyAJcAIv9Hx6/Ef2H/
    4v5XK/IH1Q/VPvv2j1a3X17zyGMO7uVntr+G4z3iPG484vit0V7Deg08i/N6x8gBDsf82GVsfLn9
    cP8ggLf0rqA2XRc26gFaGgZaBAC4fxgb6R+MDw8gAAAoqtMjmABCCDpQMtABAFTAoAHA0QA0AJwM
    YOIsTQCP6Pi/efcHLP3C/ecZ94/UD5o/8n40d8rtR84P4gfWD32/30i3jbunInxQ8hDn71Q3vmOU
    MlKPAtbtb1GGYwMqNw6j8onWxsXv42bfEpvO3yPNu8056c6fc/UvZoVx//OY/d/Wr5aBEAgALQ0D
    LQIA3L9728M4DRQAsCK4PJw2qPiPFJDbwNbrOkAgADSbAwQAgFMDQAq4p0BnAM9/aQigzv8R/1n8
    Afvf9Dp3+2jy9yzNzzzuvf3I+ScUOMQPQk8PNHEmZHDcR/m2qyJ3cO8dY5aykXBLb4rLU+56Gt0U
    tZgNCWMDFHKjUdTBn+N7/efeI8Zv13WWAsA0jv9w/00Zv6VhoEUAEGOLwQUA+H7Tkq6mGSTSAMAt
    BDUOgIm+PkA3DbRJYFMA+GmONwPYaQigxP9Cdv+naHXpy9zfn6rIHwo+YP6TZpvYb26/ED+wfuT6
    kHTl9t+lYr7u2kljw4sxoNwxcQN7V7dYn2l0Y9gMahuuwSJ/DiDgIN7D8Pjaros2Pth/23AV/zs3
    Hv+DEQYuGwDi/uUI+QMIwAfA/iEHoxawLaOXUQHdQlADAEga6JSCjRB00vvu380CfAAwItBPkgJK
    BmAI4CaJ/5td978073mV+p2gWSuPcrVvjLn9w8zt728RPwg9aN9CqiexHwTuJuX621vxGcUb6Pbt
    FHlrx0UclcJFLGNDipgDYacdG1cbuo3v8O8pwLRT/+6GOx+9KABaEgaCBgDEfLB+SQU9ALALQRYA
    3CzA3wvolYLPGinYLgVLGshvAGwPYAEA9X9kAEwAd56mkm1/5jd+kH5R9RPyh1r/ZPv2j83jCp/k
    /JB47duPzh12/xBtlAfA7RbjSjjooDwBgID8HTcfvwdDCkAACBi4feQy9WdzOTTcaBmff7/LnCYJ
    YKsCwDa+/yAElC4I87wDsAGwxwGAFoK8UrAuBr3rKwb5p31948z4kyzACwDUADgFVBkABCAQQDzx
    RvoH6RftXqnZz2jyt/wQjZ1WoW5/Md9+Vvxw+5MzWO1DRQ8qH2I/bj/SPU7rYnTLVttu85RbX8iG
    BWG7mLEAjjbm7+MAGAIGOe2M14DxA4lAwQoDQQOATQDhDfDryhWRjQCgYTew9zFIoHJwvhnyaIpB
    IgU7OsDhgABACrhZAQACEPJ/Hf9fYvaPki+aPKcu2M2qn+f2D/emfRB8cPs798ug26VtS7F7na7N
    dW77xQwlB6VdGBcEj8u9CgTI+WF4nLbdNEDwd6QMfE0DAEaX75EWghOgGui0gzvdQFY7GD8ElXKw
    1RBabhpCjvsbQnQ/gB72KLUArQQ6AJjjAgDt39v3f8z1f2QAIIAo/eKhB9g/3D/I37jpFaz66dtv
    pX0o8qQUcGEHJVzIulD1mPyZhk2INe2beUsbO7YnuOGuR/n8m4r98BL42pyfcVUB4I//QgLBASQt
    zJ/diTav0CJQAwAUJTIAnH4AqyPITgVPN0gFi9yGECkHoxhkpGDoAD/NmegC4OinrAHg4QcyACGA
    SP8g/aLqB/KHih9uv4g+dxvi57h+Jn55LO3exi1bc+nm6CVcrwfJg/Eu1/jiDWBogEi4gg2K5v6c
    VgUAiB/qAXD9KAZtWxmpRSAPALQQpDuCpSUcgyAaZgLuo5B0qyTsZgLoCEJDCPoBuBqIWoAHABdc
    ANS/z69+oP8j/0f8h/iDmj/IH9q8UPCB6HPPiEzj+nXOD8UPcm/nvhn6xQ7UPZX7Q6QBoWsOQWvu
    kc4fkEacGwOUga9pAOBAEURPYM2qKPMcLNrbEcwt4SO5I6gKPYEmE9A8YFwjPCBD8wA/EUQqyB1B
    e3kKCLqBRQhiABzTAKgwAEAGAAK43MR/POxEyTdlUhHffhA/uP4eiXbc1/360Pi5YqduP7t+Ffdh
    nObE50vxBHLr4RGaSyqvOgBEAbRjvtQCqtK7UVlqF47/7psAA4D1CVwNtF8FuURQ8QC7MTTAs3CH
    B9hEEJkAtAD0A3A1UBeDPpw4i0lgjeIAAAC6f1H/hwCE4g8eeuBtH1I/qH64/Xcr4sdl3uQCbXxz
    89n4ivVDt0ejBqt6CgC4pcEyvh8E+NkAwqUA7HIUwcsCQCACKP0A0hLGALAbQlkK1q+CAAAhgu7L
    oNFNhIGV3kGQUhMAD/hguy4Jq1QQHUF4D+AC4DyTQAcAm17n8i8IIOL/lDnb+U3f0HH69vdMWsVx
    P856qi3PtKRLB926IH4QcxACgg0AHNEQLiX+41wOEQwKAIT1AwBlC7tQ5XIAIN4BAIQg1ALQD4CW
    MMjBxzePMn2Bic7bQEkHpTtYPw9b4MwF8oaBcj0TCDzgjOIBnzzpZAJSDIISiDSw0gZAyUtMABH/
    J86q4ZIv8n7cfrh+MH4QPtz6W1DV46LOUi35Ri3WGn14KhvpShgfB24fxr8U99+qANDErysDAK1g
    ddkxtDtHABBt3gUO5KEQnAkYHuCEAUkHC8ZwNlBnsgFHFvbNBuL3AU4YqDaS8F7uCrIzAQx8hhKI
    NBAcAACABAwAQP0bPbWUyR+YP7dzJ+umzd/FpzLLhzYvZVnnhOse/Zakfc05l/PzWxUAyADEA+BV
    UD0DIIIBsG5xHPcDODzAhIGDG4ab94F+L+CSQc8j0ee9Y+B1GNjCzaEQhFATQFfQT3MeZACgHwDF
    ICiBDIAKDQA8+kD+j7Ivij6QfHH70dYFoQcvczr2LGGSBzePOAyDIN9vY9KzS2Hnl3Mu5+e3GgCQ
    92sPoGsATggACUyN4qdh9bl9mQe4YSDlIl5gvOMFjjpewFQHX9YbQKAKSjagw8A+loSFB3zw4Ex+
    Co5awPrtf+Gn3wAAUkC0fA+fUszun5l/su7juyN+Id0cuYBVuUCuGWnfpcbmyzltI5ZdcobRqh7A
    SQMXdKZKIYHmUUiZIYIcBsoGm2zA9gJDqUp5gW3sBUZaXMB+Ku6Oh/V7AbwRQDaAyiCHga+OO0QQ
    qSDKwZj3Cx0AHAAeAAIQGj4h+/ZMyuR8Hyofmjjahs1o8sO/1Nh8qUcygUsNA60GACiAIIL4HgCQ
    LGCLeRwKIrhucawOA6WDuDcQD0TQH4gxMTtyE7g2IBkBJoTuLLxfF4jKJtIeZAQbrQqhxQXEC7Ao
    dKZOZwMXjjo8AD0BSAXRDgYpGDoAOAA8AF76gP13T1xLYf3WcHNH27CZ1LFX0+63ufp8S9y/qIHX
    BQAkE5CnYawDZMeyEggegLFw/ELYeIGDxgscUVyAM4JKlRGsGdI0ISx/2J0T/JzeB4SMwPYCKA9/
    r8jgPz87RD/U5zIA3p8wk3kAMgFUA1EKRhoIDgAPgMld8Yl5dFvMfGX4DfxBNpXe4c8g114JDUC8
    i8jAN15iqtmqAJDOYACggDOBWIcHiBfQXOAekxIOdgmhAgFCAdJCqIPoE3DEoWJTIzDikBMKDCFE
    pxDqAy4XqHPI4M/du9K3XeNYEkZTCOoBqAZCCUQaOFJxADzxigMAeubwc2wYAPG/QwD3iz+D8eWh
    xpXIBOBd7DrApZDBqwaAQFKweAEXADHmhXCM8QJ6SlhlWnfaXzzA0QUOq7RQ1EEHBGsTTbPISJ82
    MLnBogiEArwahjyM+gAqhOACaBP/cfYE9gInc2pZD0BTCJ6DoRqISuDEmdtYAu6VnEnh92ap3H8p
    3Ryhbl3UQuqgUsAOsVlsABwAAlU6pIJo2kBHD8q2wRKDEFKkWcSuBQBwzf0Zl2PLoAEATSBQBGvX
    RLAWoB+IxnE6WKS8ADQBCQWVaT0aBcGhDSOMQti4QAQ+8PqBVFYIUSmERPzZKRMKTldxgQhcAF4A
    AIAXQDaAvgB0BSMMoBvo0cV7uQ7Qf1S2SgMz+VlX1wGZ3PAB9e8mlQ3o9/nzqH3EfO78RR2gY0wa
    N27q4Q2XLtgEOmD9cvNRB5ByME5zvUCrAgBpIECASiC0gHWKB7gvhHVGICCAJ8BbQaSGgUCwIyfB
    BwK/J5jCiyG5bxDPx17I4o4hDI/Aw1EQQugCyAhsL4ChkOgMQjqIkjCmfaINHO/9QQb1o48CfvDR
    tX8mp4RQAm+LXeT09kMjkNc77c3rXdzeyyWF+HfS+w8wcRtYl7n8FSCAJ2iul7lqAMDA50AeQNRA
    aAEIAxCEhAt4QRAVgBME8ARGJAocDmwQmCWR4AMqK+BQAEL4yZOcEfwcr7kA3ghs2a29QNb6V7ko
    BC4w9rFNDAIMe+CnX8PW6AHOCboJpNPdaVwXgDzMtQG84lVAAAjgBZA1tDHdO5difHgOFpgQXkxH
    EBpI20e4LWGQnJvDNf7Qf8PVAwBO4+XgzlSVHs5hgL1AVoxnVgBKxEIMdUiI4ZBgZwe2RtCAGJrs
    wLNHwGwKZVKI1PCtch0Kzuzi5ZD/V5ejM4LxM7g4BC6A5hC0hoks/ABA8KCuCqIdjL2BmeQNjQAS
    MWoEMsuH3/JHz+PUUXr+oRQ2FwQiKOGmo/0LHcRtTXModwQrjgFA8N9pBg+4JlrCpB8AL4TRE1DA
    IOjqAQG8AeoESBEZDCZDYF5gg8CkiHhJXJun1ULsEtANJKITjPesjxMQgBSCDyArQCgAIYQXAAhQ
    IpaMAA2ieBuId4FIC+EJMOQZLeH9TG+AjHTnh6BJ+iEodwf1zXBGuuHlj1QJpZdPDAP3DheOw82f
    ZtgDp5IwPGoLeDDiawu/sdNj3BwKL4C/ezGecdUB4O8JAACQBQAE4AHVxgsU+DyBfTA8Aoui8HoY
    nUMoGmGaKGoGGCUvkjHEIpSPt2bZ3sC7VwCSMVJEBsFLOVwxBB9AVoCOof+xCCF0ARSIEArQI4i1
    b2gQYRBMq2CBiB+GOK3hq7hWgGdh4hEinS6hObpLSBpE1a2VRhFJ5dpy6jjH0z7WntO9GWxwPAaR
    RyLsTTpP1w9DzMtgez5QY+dyXwcF7V2ALQkjDCAb0CDoxCAQICA7wMGvoRpy99Bq7Q1KrLqBhxew
    N9C1A7SUAwjVHr3gPq4dgBecemo+gwDpIUgh+AC6hqANSCjgtFCFAjwWQacw+ABeCekuocPcJIoH
    Iu7L4EzngQieh9ldwmH9MvgVMIY7d1CZgv1IBDdcG3Gm0/MPo7NbR+8/nojFpDnPxCTD0GXn6c5s
    gOYUny7XjpcNgEBEUIpC6A6CFxAQ2FNDCsxX8AQQR/wb/Dk8gguCGI9qCOkYQDhigMANJUWJOjRI
    ezlvFH2AQwL2CKNmgFZyDJXEG4KGoeA81wiQFeC1MHoF3VaxwzwbCJ3Csu9P3ggGahXv1He1Hu8a
    vcSRkdnN+4zc0fQU8HMwFTLwGFQ/FDXHeiAqwyGkKNVUJnC5BLBFAAjEA6AESlXQBgG+8uQQAwZ8
    j9sPAOjRMmHKK+jB0lus0XIcEuANAITSQXRw3WAODRoMiZwxME9Yl8L7hvRC6fsZBOAEEIrQSYyx
    suggRijwawOoFK7b/hcuFKFOIO8FZioQPDK/nkfEJD1YaFa+eUOCgABVRH4qptJDqIk47dA0qgCB
    0OC8FMZDkmh9bjEviG81R56IAwgMgpg03XxiQkpTmUCrvA0MFAaEBAIE+AoQaCDoXyM0IE3Ux31N
    DO4AkAAcaCbdvkY/KpUsAY9L0VWE3kJwBAACIYJBUaYPwsX+skROGwEChIM3Dy7iLiLUC/CgFCNl
    7azghdyd3DBSyQ0jpmfQBkH6YR4Qib6BJH4wqvf+9TVPxgQEsu4Fr4XadX2cOkZp/QDaAaZ/BJoZ
    4D3u/ACZEXyL8QLtzAPRpmoPLbFhiwDgDwMggmJsNIjgezlCEGWSiPy+cAeEAniDsgXgBFFOylgi
    ukGqJoqlqTHsHZyjfg+bSPYXxNOm5XH0VKkOCSCHR6oeZaEISiGkYrwn/O7sbm4dk1Cw52ldLcTr
    YZBCDI1EegiNYL4PBJ5XwzIryAcCpIj/3muZOuorf4+ZQHOtqSHu5BDv9BANBG48jdPzBNCRhDDA
    2kAjHqAl7r/FAAgUBlwi2MnRBmRukLeBNMwBhJSScWo5hezi1BIABNQT9Il2zhbzFQSyemUE/+/u
    y4vhUTTgByCHKCW/UD+H5WKkhpwVfFjLL4kwTkZCAYpF4APQBzA/ANNDMDlsae5zPDNQQNDg6bjP
    E0iaiFZyz5AoZ26QHhYFUanhsCgvCDA4QuYEtena+OOTVp0QEigMIK7bBpbXwuLy5ciNl9dEu5Th
    Hc+QHm6UxEhP2oiBE/vyojlUwOjIJvC9gGsf1MiV8cwPqrOTWDBCGRmFI84K3lxvBKJ6lomlZPyi
    CgVIDcEHNsgTMjwht56QgRgG9ARmfoCtFwQaDnnxcXEuCHhSKLhDrNuC3hgAWmq/FgPAHwZwo+X2
    46sMkLSHRzV27H+7yfQXcgo5V2cPMDaAgp+ngdaF/754EPx7jKTD5nGkiygkocUcXgBNJGgjQyOp
    qw0cdUIBGkfwkhj6wDq8Itr8hvcZeUZgEMgIGZ0mrqI4XgWX02A+sH9MrDMt1IyQ+/ceBgQ8LFJv
    C5FRcTxnIIAa2FL3HxQA4NiiEG60EEEYUpj+xYxvhxD8e2kzl8wBX5E54Gf6fx5+LUCDV6jO6MmE
    EDoBuoqOVD3mcAHXC9TxYxIhhB88OItDwbZ9Z2jTrve4jRxFI35KVniSJWPMD+YZgtYcIT1A0kwP
    Ha43gsqwaAyV0MOiS3mqGHuGBsOi8zhEAATwBPY7BACgLd4gKOMHGhMTjP0BQQGA3wsIEZSb2lzj
    +1vMhDjKTQco7GfofuBIRrE1PdaEgUR+bILF02gqRcEICuGX71QaL7CXJ4v8aLqIX7BCAaaJgw/k
    mHkCMk5mjhkng90BIz2j5HK4mCSLIZ0tIQn+qeHWGFkZIWtqDDIxHOPkbjbDJ+D+pWJogyAYtz9o
    APB7AZvpX6rx7SO8QH4WQGA/Q/c3o3BD6vII2rQinnsOUVLeUTCGw8DJ+tncRYRJI7pYVO14Abwl
    EEIoj0mcJ+Vb9Ui5VWamAE8UwzzB5RoEox6W9fAN9wN718WksYoYaGNIlLMzIIczBj18YpHuP5Cx
    MsoL2GJQsLaHBA0AtheQqWGXc/sDqYsCBHxt7O8BHAgVaESFiggPgEoieAAqhygdo4EEZBDDpjBn
    yOECnysvMFt7gRfzdnHZeNtTerAU9IF8z1whPVCah0ouP2iBwJ0rOMhaGqW3h7gLJGKHuPsCBQR6
    0mieW2kED4hJ5efnTjeS1R4WrNsfVAD4vYCd57f04NZLvwEAEQgkLEMrnrAlXa+nQf1gb8lwLhqB
    ByAbwENTNI+gZOxUC1VGIF4A+wX/1i2eCSE2i0ko4NmCFe5sIZ4rjIUSK70gcEbKO8ui9SYRe5mE
    7BPoZa+Nk3Kz2RvEOwOiU+mm2Cy64a7HnKKQACCYu4OCCgB4ASFoMiYmGAAQIwMEtmbguH7DD9CA
    gpE02zPvZg+wtySFy8c7i8eyMgh5WIeBfG4n/yt3Eu/kByV4WPrD7jz2Ai/l7zKzBdxQgKKR8IE0
    M110fqa1V+Bxd68A5guDHDqLpOyQYHkChANkChg+BS8QYcrMSAchFaNeoKuDWhJGCAjm7Q86AHBW
    TuvqgMBvrGABQUgf1EadNuqUkaeSpRoAbBjGJWRkAiCCqA9g6ohUCiEP6/cE23ncrDwoES+AKaNY
    MgGBaBOHAkwY0SNmVvOA6ZPOgGnMGeKlUtNtEBQGBIF4ApBD2R8ou4MjeXWsXh3XMVK0AC0J4/aD
    BAZ7c1jQAYCdgRB/EK+bk/tfDgBESJKUUcSjmlXR3GACDoCmEjST4LEJ+gbQM8A84EQ61wc+laYR
    kEHTRApd4Ifd+Y4X4Ofl1pQxCQUoH0MkAinEpDGkhzM9IChpAIKBfhBY+4N7mvWxSAvBA2R1rJaF
    9eoYlJbv6J0VVFtdEQDgYH1MsEig/5xYF9cgE5A3CfACkIeRBaCMjEohOol25I/mfgHwgNOKB0AU
    +uSVQi4VO/2DSAk/P+Q8LxcvgF0D7qRRNxRAH5ANY+gowrRR2S2IfgKbEyTYnMDaMcTqIbSCEToM
    RJlBVEIE2QtgeVSXmdwjeCVsdcVWxy6dGsYhQIwTDOPjZ8HtB/p5+DOeTbQqkjuMUDUEEazN02tp
    8OIYesDr+1OZB6BX4LPX1zlhgF8XKzKoXxdP9HiBHQcUIdyjh01CIEJWoEOBmTae+6yzYPKJNL1l
    7D7eMuYumkIlUVbK9jVewNkjjDCQXGzazXJ59Lw9iBJawJ1XaH/wFV0eLR4AxrlURbAxADSlB8A7
    SFGofGkMTyTBVFIQwV1MBCfTC/WzFQ9YrR+UnCrlXgEdBuq4SKSnjDzjThw/rr0AFEJ34PTbet3M
    ep0aopHEs2xqxaEGewZl64hIx9JcAsUQZFCmkYEIYg4xE0GVCuKByu+jr7PdwXKwQFqMLsYLlMY1
    99hFJltmxpFRtQDAuzv7MADQSQQiyJlA0QMKAJN47hBkYd1BXMwTxzBzkLOBc3v1uLmv3CETWDoh
    Owe2WF6gwAydxsoZe9mk5gOHG/CBRHvp1Ogsz+oZ7j62FlAgFWQ1MHIBdeg07orZ54oDAGfR1GjH
    gFIqhrEuFQii9+uuo25ObQC/xsmffRdXA9+s7sU8AA0kUAOdVBAAMJkAqoOQhdEt5PAAM3VUwsCP
    hgziibln6rjlBTB1TNbOYPQcr5n3hQJePePzAgONZNzbjKJF8Ug2kOhB1KlcC4AOcKXtc8UBgOOX
    iaXidykkUQzNdQHTco7CD6RfHKSAm1dEcTUQrWSaBKbQ1qwEFoN0CLABkOU8JIEqyK+JztjDpp5x
    yOC+Z76guiNaF9CLp047i6dyNp7ihdPpvHBaeYG1WDmrV81PX+7zAmbpJIcBq9FUVtDwHgKVBfDt
    j8++IsuiWwUAfhCIJ5BOoYt5A0n3+N+YXgG0mct6Wv8gCrSIgQBiORXSwB3cMDqOOYAXAAUNXhZ/
    H3DsrJ48rkfPf+RuHq16y+sFCn1egHcP7nZ3D1olZHsUPUIAtACkgXz74zKuivGvKgAa8wTSPiah
    AV4BRyp/oijiQFtwVtMq40vLWKn1ukhG0OAtAW5/TW4K6wDSNg4O4AAAI+h59nCFHj/vmTrqDp18
    paDOWT+H7SOye5i5gCyfbIQLIC28T/YO2/sHLQDwaLqkIm4xx0ujq2X8qw6ApjyBhAQweRhabjz+
    jsR/IYLYTmrPIWTjW/OHeBBlXqKeRcyLKe/Xj0fKH+LmEH5VjLeEFgDsaWP+oZOygKrOWkCFNwXO
    BrKKxtfPIgzYW8iGmdKxAABZAHQAtJEh7bvrCqV71wwA/CCQSp88JuHuYDSCpIXzkSHUWvHTfwYA
    8GZyvCby3/yK4XrkDDaSmNX0u0r0XmK8I3zXfkzqAUCVM2cIncOyhRTSsGQDnjBQ7y6gRhjILn+V
    JWKEgSVGF5A1dM4Sykl6CSUAAA7AC6dV/I9LLKQ7ui+lO7v0ueq2aBUA+EEgrp/fFC4Icx6QCNHD
    6yF7KbUePROpXxHxU7LBziTyg+uHu6PocfsL7zfuf7KeM8QjZmTQVKEBgIyflwUUja2g0dmAEwZq
    fWHA3kCedcJpHpk8ezuNesQAYLweSg0xiFfRKPKHLuJO4f1bxQ6tBgA/CGTKuPu0XBpC45xHpV7C
    p5+We2YQmyHU6ARybj/vIXiQB03pbWQr9NRRA4AG+wf8CyiMHgAegMGTu45Ya2hlCzmyAZGHUSMQ
    HmDkYYykFQDIm0OO/4r8de6ffdXd/jUDABy7kURy/HwDgPpsbXjnSblnrkBf3kR2yJo/vNdaRbM9
    37eVXIZP83yhNWYTWVEAANS7ALD3EE6aRfvMHsLAPOB1hwfYRSIGADyA6SFEGgj3j9F0UQMWtupn
    f00AQEAg3kByfTwV804a888YMuPmzOxhuP6tzg4C7xIKmSPA4+et2UI8clY4gAcA+xqsorU3kdqr
    aJ10sMKkgyUvOdKwBoDmAKgLyFRyxP7IXldW4buuACBHQADSB3IIENRkxDgzh/0vh/XcYd/EUevF
    sDNy1po87m4kz204et4CgKSCDXcRayJY/SSWUb/H7wiECDoAMJmA7COU18Zw/4j9EbGtE++veQDg
    SFeRKH/bZPFEqp45fKDJmcPWoMkid4CEu5Le7B4wO4jOvxZoCVUjAJg8m/YLABrZRp5teYBU0zwq
    K2nQJXRP4txW/3yveQDIWTEtltND1PgxREJYP6aNHmLWn9Bw6niObwlVqW8Jlb2FTMbOv7mBn4w1
    BQCkggIAdA1zJgAA7H6fR9BqAIgH0CRQL6Q8whnA0DGLKLbHoFb/TK8rAMiZNjacQwHeCDr7h0q1
    B/Cvn9nmWT8TaA1dujtxPNAeQk4D/QB4ln6eO1kDwGwjdQBgeQBnISU/J3tOzyGcXkq9+ya3+md4
    XQNAzsSUMNpfoHv+wAO08IMQMJLHzFZb+4e0+7/ETaT2IsqLeAA3BLgkMNe8IkorfIH+OLec7hkw
    vNU/s18UAORMHxdO6xeH8z5igAAkkNfPeFbRegHg7CJ+tqldxHoVrasDNE0CpSjEC6nNQsoZSzZT
    /0EjW/0z+kUDQA6aTyePiqC6nD4sAu3I1dPGnZlBFgDsbeQXXUfvrKLVOoCdBj71jG4O4RYxs494
    yZpamjh1aat/Hr86ANgneVA0zX24D88MggS8gxVAzQEYALsCLKFkDUAAsIW+fa/GrKCz19G7QtAr
    hXVOk+iawr009fH0Vv/vDgGgkfPYxL40fVJ/enzKvawB2HuHvGto17MK6NlCeu5J3kGIFnFsHnlt
    YE8qa9OGZi3IoqTh14Z4EwLAZZ4HRg6k1akTKGPxRFqzZDJlLXuYsldMpbVpj1LuymmUt/oJKlgz
    q9X/f4YAEDohAIROCAChEwJA6IQAEDohAIROCAChEwJA6IQAEDohAIROCAChEwJA6ATr/D8pDheJ
    IDZxtQAAAABJRU5ErkJggg==
