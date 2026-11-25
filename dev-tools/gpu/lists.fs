# Lists on the GPU (check by hand with dev-tools/web/check-gpu.mjs: every
# pixel green, on the CPU and both GPU backends): appending in place must
# never change a list another variable (or list) holds; ranges in for each.
format: 1
script: |
  @viewer "Aliasing" over z
  setup:
    center: 0
    pixel size: 1/50
  n := round(|z| * 10) + 3
  ok := true
  # a copy, then the original appended to: the copy keeps its length
  xs : List of Z := [1]
  ys := xs
  xs <- append(xs, 2)
  ok <- ok and length(ys) = 1 and length(xs) = 2 and xs @ 2 = 2
  # appended in a loop, a copy taken part way
  zs : List of C := []
  mid : List of C := []
  for each k in range(1, n):
    zs <- append(zs, k z)
    if k = 2:
      mid <- zs
  ok <- ok and length(zs) = n and length(mid) = 2 and |(zs @ n) - n z| < 0.0001 (1 + n |z|)
  # a list held in another list, read out and appended to
  outer := [[1, 2], [3]]
  inner := outer @ 1
  inner <- append(inner, 9)
  ok <- ok and length(outer @ 1) = 2 and length(inner) = 3
  # appended in one branch only
  ws : List of Z := [5]
  if re z > 0:
    ws <- append(ws, 6)
  ok <- ok and length(ws) = (if re z > 0 then 2 else 1)
  # a range as a value, and counted through
  r := range(2, n)
  total := 0
  for each k in range(1, n):
    total <- total + k
  ok <- ok and length(r) = n - 1 and total = n (n + 1) // 2
  color <- if ok then green else red
