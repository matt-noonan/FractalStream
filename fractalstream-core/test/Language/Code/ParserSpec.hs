{-# language QuasiQuotes #-}
module Language.Code.ParserSpec (spec) where

import Test.Hspec

import FractalStream.Prelude
import Data.List (isInfixOf)

import Language.Type
import Language.Value
import Language.Value.Typecheck
import Language.Code.Parser
import Language.Code.Simulator
import Language.Draw
import Data.Color

import Text.RawString.QQ

runWithX :: forall xt
          . Scalar xt
         -> String
         -> Either String (HaskellType xt)
runWithX (Scalar xt x) input = withKnownType xt $
  let env = BindingProxy (Proxy @"x") xt
          $ BindingProxy (Proxy @InternalIterations) IntegerType
          $ BindingProxy (Proxy @InternalIterationLimit) IntegerType
          $ BindingProxy (Proxy @InternalStuck) BooleanType
          $ EmptyEnvProxy
      ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType 100
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ()))
             . (\c -> simulate noDraw c >> eval (Var (Proxy @"x") xt bindingEvidence)))
     $ parseCode env noCommonDefs ("n : Z := 0\n" ++ input)

runWithXY :: forall xt yt
           . Scalar xt
          -> Scalar yt
          -> String
          -> Either String (HaskellType xt)
runWithXY (Scalar xt x) (Scalar yt y) input = withKnownType xt $ withKnownType yt $
  let ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @"y") yt y
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType 100
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ()))
             . (\c -> simulate noDraw c >> eval (Var (Proxy @"x") xt bindingEvidence)))
     $ parseCode (envProxy Proxy) noCommonDefs ("n : Z := 0\n" ++ input)

runWithXYC :: forall xt yt
           . Scalar xt
          -> Scalar yt
          -> String
          -> Either String Color
runWithXYC (Scalar xt x) (Scalar yt y) input =
  withKnownType xt $ withKnownType yt $
  let ctx = Bind (Proxy @"x") xt x
          $ Bind (Proxy @"y") yt y
          $ Bind (Proxy @"color") ColorType grey
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType 100
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ()))
             . (\c -> simulate noDraw c >> eval (Var (Proxy @"color") ColorType bindingEvidence)))
     $ parseCode (envProxy Proxy) noCommonDefs ("n : Z := 0\n" ++ input)

-- | Like 'runWithX' for a real @x@, with a vanishing radius (of 0.0001)
-- in scope, as approximate comparisons need.
runWithRealX :: Double -> String -> Either String Double
runWithRealX x input =
  let env = BindingProxy (Proxy @"x") RealType
          $ BindingProxy (Proxy @InternalVanishingRadius) RealType
          $ BindingProxy (Proxy @InternalIterations) IntegerType
          $ BindingProxy (Proxy @InternalIterationLimit) IntegerType
          $ BindingProxy (Proxy @InternalStuck) BooleanType
          $ EmptyEnvProxy
      ctx = Bind (Proxy @"x") RealType x
          $ Bind (Proxy @InternalVanishingRadius) RealType 0.0001
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType 100
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ()))
             . (\c -> simulate noDraw c >> eval (Var (Proxy @"x") RealType bindingEvidence)))
     $ parseCode env noCommonDefs input

-- | Like 'runWithX' for a complex @x@, with an escape radius (of 1000) and
-- a vanishing radius (of 0.0001) in scope, for points of the sphere.
runSphere :: String -> Either String (Complex Double)
runSphere input =
  let env = BindingProxy (Proxy @"x") ComplexType
          $ BindingProxy (Proxy @InternalEscapeRadius) RealType
          $ BindingProxy (Proxy @InternalVanishingRadius) RealType
          $ BindingProxy (Proxy @InternalIterations) IntegerType
          $ BindingProxy (Proxy @InternalIterationLimit) IntegerType
          $ BindingProxy (Proxy @InternalStuck) BooleanType
          $ EmptyEnvProxy
      ctx = Bind (Proxy @"x") ComplexType 0
          $ Bind (Proxy @InternalEscapeRadius) RealType 1000
          $ Bind (Proxy @InternalVanishingRadius) RealType 0.0001
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType 100
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ()))
             . (\c -> simulate noDraw c >> eval (Var (Proxy @"x") ComplexType bindingEvidence)))
     $ parseCode env noCommonDefs input

noDraw :: DrawHandler (HaskellTypeM ())
noDraw = DrawHandler (const $ pure ())

spec :: Spec
spec = do

  describe "when parsing code blocks" $ do

    it "can parse if/then/else blocks" $ do

      let p1 = [r|
if true:
  pass
  x <- 1 + 2
else:
  x <- 3 + 4
|]
          p2 = "x <- 1 + 3"
          p3 = [r|
if y:
  x <- 1 + 3
  pass
else:
  pass
|]
      runWithX  (Scalar IntegerType 7) p1 `shouldBe` Right 3
      runWithX  (Scalar IntegerType 7) p2 `shouldBe` Right 4
      runWithXY (Scalar IntegerType 7) (Scalar BooleanType True)  p3 `shouldBe` Right 4
      runWithXY (Scalar IntegerType 7) (Scalar BooleanType False) p3 `shouldBe` Right 7

    it "can bind new variables" $ do

       let p1 = [r|
x <- 5
y : Z := x - 2
if true:
  x <- 2 * y
else:
  pass
|]
           p2 = [r|
x <- 5
y : Z := x - 2
if true:
  x <- 2 * y
|]

       runWithX (Scalar IntegerType 0) p1 `shouldBe` Right 6
       runWithX (Scalar IntegerType 0) p2 `shouldBe` Right 6

    it "can coerce variable types" $ do
        let p1 = [r|
k : Z := 1
x <- k|]
        runWithX (Scalar RealType 0) p1 `shouldBe` Right 1.0

  describe "when parsing more complex code" $ do

    it "can parse a checkered Mandelbrot program" $ do
      let mandel = [r|
C : C := x + y i
z : C := 0
while |z| < 100:
  z <- z^2 + C
if |z| < 100:
  color <- black
else if im z > 0:
  color <- red
else:
  color <- yellow|]

          runMandel (x :+ y) = runWithXYC (Scalar RealType x) (Scalar RealType y) mandel
      runMandel 0 `shouldBe` Right black
      runMandel (1 :+ 1) `shouldBe` Right yellow
      runMandel (1 :+ (-1)) `shouldBe` Right red

  describe "when declaring variables with :=" $ do

    it "takes the variable's type from its initial value" $ do
      runWithX (Scalar RealType 0) "k := 3\nx <- k / 2" `shouldBe` Right 1.5
      runWithX (Scalar ComplexType 0) "w := 1 + 2i\nx <- w" `shouldBe` Right (1 :+ 2)

    it "rejects assignments that don't fit the inferred type" $
      runWithX (Scalar RealType 0) "k := 3\nk <- true\nx <- k"
        `shouldSatisfy` either ("I expected an integer here, but `true` is a truth value" `isInfixOf`) (const False)

  describe "when inferring the types of := declarations" $ do

    it "accounts for later assignments" $
      runWithX (Scalar RealType 0) "k := 3\nk <- 0.5\nx <- k" `shouldBe` Right 0.5

    it "infers complex iterates" $
      runWithXY (Scalar ComplexType 0) (Scalar ComplexType (0 :+ 1)) [r|
z := 0
repeat up to 3 times:
  z <- z*z + y
until false
x <- z|] `shouldBe` Right (0 :+ (-1))

    it "infers through iterate statements" $
      runWithXY (Scalar ComplexType 0) (Scalar ComplexType (0 :+ 1)) [r|
z := 0
iterate z -> z*z + y until false up to 2 times
x <- z|] `shouldBe` Right ((-1) :+ 1)

    it "propagates through other inferred variables" $
      runWithX (Scalar RealType 0) [r|
a := 1
b := a
a <- 0.25
b <- b + a
x <- b|] `shouldBe` Right 1.25

    it "widens list item types" $
      runWithX (Scalar RealType 0) [r|
xs := [1, 2]
xs <- append(xs, 0.5)
x <- xs @ 3|] `shouldBe` Right 0.5

    it "infers through function calls" $
      runWithX (Scalar RealType 0) [r|
half(t) := t / 2
k := 1
k <- half(k)
x <- k|] `shouldBe` Right 0.5

    it "keeps separate declarations in separate scopes apart" $
      runWithXY (Scalar IntegerType 0) (Scalar RealType 0) [r|
if true:
  k := 1
  x <- k
else:
  k := 1
  k <- 0.5
  y <- k|] `shouldBe` Right 1

    it "reports uses that conflict with the inferred type" $
      runWithX (Scalar RealType 0) [r|
k := 1
k <- k + 0.5
j : Z := k // 2
x <- j|] `shouldSatisfy` isLeft

    it "rejects declarations whose type can't be determined" $
      runWithX (Scalar RealType 0) "xs := []\nx <- 1" `shouldSatisfy`
        either ("can't tell what type" `isInfixOf`) (const False)

    it "rejects redeclaring a variable" $
      runWithX (Scalar RealType 0) "x := 3" `shouldSatisfy` isLeft

  describe "when defining functions" $ do

    it "inlines calls" $
      runWithX (Scalar RealType 0) "f(t) := 2 t + 1\nx <- f(3) + f(0.5)" `shouldBe` Right 9.0

    it "specializes each call at its own argument types" $
      runWithXY (Scalar IntegerType 0) (Scalar ComplexType (0 :+ 1)) [r|
sq(t) := t * t
x <- sq(3)
y <- sq(y)|] `shouldBe` Right 9

    it "supports several parameters, nested calls, and calls between functions" $
      runWithX (Scalar RealType 0) [r|
f(a, b) := a - b
g(t) := f(t, f(1, t))
x <- g(f(5, 2))|] `shouldBe` Right 5.0

    it "lets parameters shadow variables in scope" $
      runWithX (Scalar RealType 2) "f(x) := x + 1\nx <- f(10)" `shouldBe` Right 11.0

    it "captures variables in scope at the definition" $
      runWithX (Scalar RealType 0) "c : R := 4\nf(t) := t + c\nx <- f(1)" `shouldBe` Right 5.0

    it "scopes functions to the rest of their block" $
      runWithX (Scalar RealType 0) [r|
if true:
  f(t) := t
x <- f(1)|] `shouldSatisfy` either ("No function named f" `isInfixOf`) (const False)

    it "rejects bodies that refer to undefined variables" $
      runWithX (Scalar RealType 0) "f(t) := t + q\nx <- f(1)" `shouldSatisfy` isLeft

    it "rejects calls with the wrong number of arguments" $
      runWithX (Scalar RealType 0) "f(a, b) := a + b\nx <- f(1)" `shouldSatisfy` isLeft

    it "rejects function names that clash with variables" $
      runWithX (Scalar RealType 0) "x(t) := t\npass" `shouldSatisfy` isLeft

  describe "when differentiating user-defined functions" $ do

    it "differentiates calls inside diff" $
      runWithX (Scalar RealType 2) "f(t) := t^3\nx <- diff(f(x), x)" `shouldBe` Right 12.0

    it "desugars f' to a derivative, at any numeric argument" $ do
      runWithX (Scalar RealType 0) "f(t) := t^3\nx <- f'(2)" `shouldBe` Right 12.0
      runWithX (Scalar RealType 0) "f(t) := t^3\nx <- f'(0.5)" `shouldBe` Right 0.75
      runWithXY (Scalar ComplexType 0) (Scalar ComplexType (0 :+ 1)) [r|
f(t) := t*t + 1
x <- f'(y)|] `shouldBe` Right (0 :+ 2)

    it "supports higher derivatives" $
      runWithX (Scalar RealType 0) "f(t) := t^3\nx <- f''(2)" `shouldBe` Right 12.0

    it "differentiates through calls to other functions" $
      runWithX (Scalar RealType 0) [r|
f(t) := t^3
g(s) := f(s) + 2 s
x <- g'(1)|] `shouldBe` Right 5.0

    it "differentiates derivatives inside diff" $
      runWithX (Scalar RealType 2) "f(t) := t^3\nx <- diff(f'(x), x)" `shouldBe` Right 12.0

    it "differentiates through local bindings by the chain rule" $ do
      -- f'(2x) = 12 x^2, so its derivative at x = 1 is 24
      runWithX (Scalar RealType 1) "f(t) := t^3\nx <- diff(f'(2x), x)" `shouldBe` Right 24.0
      runWithX (Scalar RealType 1) "f(t) := t^3\nx <- diff(1 + x f'(2x), x)" `shouldBe` Right 36.0
      runWithX (Scalar RealType 2) "f(t) := t^3\ng(s) := f(s + 1)\nx <- diff(g(x x), x)" `shouldBe` Right 300.0

    it "suggests diff for f' of functions of several variables" $
      runWithX (Scalar RealType 0) "f(a, b) := a b\nx <- f'(1, 2)" `shouldSatisfy`
        either ("use diff instead, like diff(f(a, b), a)" `isInfixOf`) (const False)

  describe "when defining and running code" $ do

    let failsWith input needle =
          runWithX (Scalar RealType 0) input `shouldSatisfy` either (needle `isInfixOf`) (const False)

    it "runs code without parameters" $
      runWithX (Scalar RealType 0) [r|
define bump:
  x <- x + 1
  x <- 2 x
run bump
run bump|] `shouldBe` Right 6.0

    it "runs code with parameters" $ do
      runWithX (Scalar RealType 1) [r|
define add(k):
  x <- x + k
run add(2.5)|] `shouldBe` Right 3.5
      runWithX (Scalar RealType 3) [r|
define affine(a, b):
  x <- a x + b
run affine(2, 1)|] `shouldBe` Right 7.0

    it "passes arguments as local copies" $ do
      runWithX (Scalar RealType 1) [r|
define inc(c):
  c <- c + 1
  x <- c
run inc(x)
run inc(x)|] `shouldBe` Right 3.0
      -- a parameter may share a name with a variable where the code is run
      runWithX (Scalar RealType 3) [r|
define clobber(x):
  x <- 7
run clobber(5)|] `shouldBe` Right 3.0

    it "keeps the code's own declarations local to each run" $
      runWithX (Scalar RealType 0) [r|
define p:
  t := x + 1
  x <- t
run p
run p|] `shouldBe` Right 2.0

    it "runs code from inside code, loops, and conditionals" $
      runWithX (Scalar RealType 0) [r|
define a:
  x <- x + 1
define b:
  run a
  run a
if true:
  repeat up to 3 times:
    run b
  while true|] `shouldBe` Right 6.0

    it "infers parameter types from the arguments" $
      runWithXYC (Scalar RealType 0) (Scalar RealType 0) [r|
define paint(c):
  color <- blend(0.5, dark c, light c)
run paint(red)|] `shouldBe` runWithXYC (Scalar RealType 0) (Scalar RealType 0)
        "color <- blend(0.5, dark red, light red)"

    it "reports code that isn't defined" $ do
      failsWith "run q" "No code named q"
      -- code can't run itself
      failsWith "define p:\n  run p\nrun p" "No code named p"
      -- definitions are scoped to the rest of their block
      failsWith "if true:\n  define p:\n    pass\nrun p" "No code named p"

    it "reports the wrong number of arguments" $ do
      failsWith "define p:\n  pass\nrun p(1)" "p takes no arguments"
      failsWith "define p(a):\n  pass\nrun p" "p takes 1 argument"
      failsWith "define p(a, b):\n  pass\nrun p(1)" "p takes 2 arguments"

  describe "when using `stops` in a loop's condition" $ do

    let failsWith input needle =
          runWithRealX 0 input `shouldSatisfy` either (needle `isInfixOf`) (const False)

    it "stops a loop once a value stops changing" $ do
      let newton = runWithRealX 2
            "iterate x -> x - (x^2 - 2) / (2 x) until x stops"
      -- successive iterates within 0.0001: then x is far closer than that
      fmap (\v -> abs (v - sqrt 2) < 1e-6) newton `shouldBe` Right True
      runWithRealX 2 [r|
iterate x -> x - (x^2 - 2) / (2 x) until x stops
x <- if stuck then -1 else iterations|] `shouldSatisfy` either (const False) (\k -> k > 1 && k < 10)

    it "is false at the first check of a loop that checks first" $ do
      runWithRealX 5 "until x stops:\n  pass\nx <- iterations" `shouldBe` Right 1
      runWithRealX 5 "iterate x -> x until x stops\nx <- iterations" `shouldBe` Right 1
      runWithRealX 5 "while not (x stops):\n  pass\nx <- iterations" `shouldBe` Right 1

    it "compares against the start of the latest pass in repeat loops" $ do
      runWithRealX 5 "repeat:\n  pass\nuntil x stops\nx <- iterations" `shouldBe` Right 1
      runWithRealX 0 [r|
repeat:
  x <- x + 1
  x <- x - 1
until x stopped
x <- iterations|] `shouldBe` Right 1

    it "works on expressions, and alongside other conditions" $ do
      runWithRealX 0 [r|
k := 0
repeat:
  k <- k + 1
  x <- if k < 3 then k else 3
until 2 x stops or k > 50
x <- k|] `shouldBe` Right 4

    it "can't be used outside a loop's condition" $ do
      failsWith "if x stops:\n  pass" "`stops` can only be used in a loop's condition"
      failsWith "until false:\n  x <- if x stops then 1 else 2" "`stops` can only be used in a loop's condition"
      failsWith "b := x stops" "`stops` can only be used in a loop's condition"

  describe "when using `previous` in a loop" $ do

    it "is the value at the start of the pass, in the loop's body" $
      runWithRealX 0 [r|
total := 0
k := 0
repeat:
  x <- x + 2
  total <- total + (x - previous x)
  k <- k + 1
until k = 5
x <- total|] `shouldBe` Right 10

    it "is the value at the start of the pass just made, in the loop's condition" $ do
      runWithRealX 0 "repeat:\n  x <- x + 1\nuntil previous x = 3" `shouldBe` Right 4
      runWithRealX 0 [r|
y := 1
iterate (x, y) -> (y, x + y) until previous x = 13|] `shouldBe` Right 21

    it "is the value itself, before a loop that checks first has made a pass" $ do
      runWithRealX 5 "until previous x = 5:\n  x <- x + 1" `shouldBe` Right 5
      runWithRealX 0 "until previous x = 5:\n  x <- x + 1" `shouldBe` Right 6
      runWithRealX 0 "iterate x -> x + 1 while previous x < 3" `shouldBe` Right 4

    it "belongs to the innermost loop around it" $
      runWithRealX 0 [r|
outer := 0
inner := 0
k := 0
repeat:
  x <- x + 10
  j := 0
  repeat:
    x <- x + 1
    inner <- inner + (x - previous x)
    j <- j + 1
  until j = 3
  outer <- outer + (x - previous x)
  k <- k + 1
until k = 2
x <- 100 outer + inner|] `shouldBe` Right 2606

    it "works in a `for each` loop" $
      runWithRealX 0 [r|
total := 0
for each p in [1, 2, 3, 4]:
  x <- x + p
  total <- total + (x - previous x)
x <- total|] `shouldBe` Right 10

    it "belongs to the loop around a `run`" $
      runWithRealX 0 [r|
d := 0
define step:
  x <- x + 3
  d <- x - previous x
repeat up to 4 times:
  run step
while true
x <- d|] `shouldBe` Right 3

    it "can't be used outside of a loop, or of a variable declared in it" $ do
      let failsWith input needle =
            runWithRealX 0 input `shouldSatisfy` either (needle `isInfixOf`) (const False)
      failsWith "if x = previous x:\n  pass" "`previous` can only be used inside a loop"
      failsWith "y := previous x" "`previous` can only be used inside a loop"
      failsWith "repeat:\n  y := x\n  x <- previous y\nuntil true" "`y` is declared inside the loop"
      failsWith "for each p in [1, 2]:\n  x <- previous p" "`p` is declared inside the loop"

  describe "when writing pair types" $
    it "accepts × as well as x" $ do
      runWithX (Scalar RealType 0) "p : R × R := (1.5, 2)\nx <- (1.5 : R) × 2" `shouldBe` Right 3.0
      runWithX (Scalar RealType 0) "p : R x R := (1.5, 2)\nx <- 2 * 1.5" `shouldBe` Right 3.0

  describe "functions with a body" $ do

    let gives input v = runWithX (Scalar RealType 0) input `shouldBe` Right v
        failsWith input needle =
          runWithX (Scalar RealType 0) input `shouldSatisfy` either (needle `isInfixOf`) (const False)

    it "give the value their body assigns to their name" $ do
      "function sq_plus(a, b):\n  t := a a\n  sq_plus <- t + b\nx <- sq_plus(3, 1)" `gives` 10
      "function sq_plus(a, b):\n  t := a a\n  sq_plus <- t + b\ny := sq_plus(3, 1)\nx <- y" `gives` 10
      -- the last assignment wins, and the name can be read once assigned
      "function f(a):\n  f <- a\n  f <- f + 1\nx <- f(2)" `gives` 3
      -- a value on every path
      "function m(a, b):\n  if a > b:\n    m <- a\n  else:\n    m <- b\nu := m(2, 5)\nv := m(7, 1)\nx <- u + v" `gives` 12

    it "can give a tuple" $
      "function both(a):\n  both <- (a, a + 1)\n(p, q) := both(2)\nx <- p q" `gives` 6

    it "have variables of their own, whatever the caller's are called" $
      "t := 100\nfunction f(a):\n  t := a + 1\n  f <- t\ny := f(1)\nx <- t + y" `gives` 102

    it "don't change the caller's iterations or stuck" $
      unlines [ "repeat up to 3 times:"
              , "  pass"
              , "until false"
              , "function g(a):"
              , "  s := 0"
              , "  for each k in range(1, a):"
              , "    s <- s + k"
              , "  n := 0"
              , "  while n < a:"
              , "    n <- n + 1"
              , "  g <- s + n"
              , "y := g(4)"
              , "x <- iterations + y + (if stuck then 100 else 0)" ] `gives` 117

    it "can call other functions with bodies, and one-line functions" $
      unlines [ "sq(a) := a a"
              , "function f(a):"
              , "  f <- sq(a) + 1"
              , "function g(a):"
              , "  b := f(a)"
              , "  g <- 2 b"
              , "x <- g(3)" ] `gives` 20

    it "must give a value on every path" $ do
      failsWith "function f(a):\n  if a > 0:\n    f <- a\nx <- f(1)" "`f` isn't given a value on every path"
      failsWith "function f(a):\n  while a > 0:\n    f <- a\nx <- f(1)" "`f` isn't given a value on every path"
      failsWith "function f(a):\n  b := f + 1\n  f <- b\nx <- f(1)" "`f` is read here before its body has given it a value"

    it "only work out a value" $ do
      failsWith "function f(a):\n  draw point at 0\n  f <- a\nx <- f(1)" "f can't draw"
      failsWith "function f(a):\n  x <- 3\n  f <- a\nx <- f(1)" "f can't change `x`"
      failsWith "define b:\n  pass\nfunction f(a):\n  run b\n  f <- a\nx <- f(1)" "f can't run the code block `b`"

    it "see only what's in scope where they're defined" $
      failsWith "function g(a):\n  g <- a + later\nlater := 2\nx <- g(1)" "No variable named later"

    it "can be called inside an expression, in the order it's written" $ do
      "function f(a):\n  f <- a + 1\nx <- f(1) + 1" `gives` 3
      "function f(a):\n  f <- a + 1\nx <- f(f(1)) f(0)" `gives` 3
      "function sq(a):\n  sq <- a a\nx <- 2\nx <- x - sq(x) / (2 x)" `gives` 1
      "function f(a):\n  f <- a + 1\nx <- 0\nif f(x) > 0:\n  x <- 5" `gives` 5
      "function f(a):\n  f <- a + 1\ny := 0\nx, y <- f(y), f(x)" `gives` 1
      "function both(a):\n  both <- (a, a + 1)\nfunction add(t):\n  (p, q) := t\n  add <- p + q\nx <- add(both(3)) + 0" `gives` 7
      "function g(a):\n  g <- a a\nfunction f(a):\n  f <- g(a) + g(a + 1)\nx <- f(1)" `gives` 5

    it "is only called where it would be worked out, in `if`, `and` and `or`" $ do
      -- (`slow` never finishes, so it can't be called)
      let slow = "function slow(a):\n  b := a\n  while true:\n    b <- b + 1\n  slow <- b\n"
      (slow ++ "x <- if x < 1 then 4 else slow(x)") `gives` 4
      (slow ++ "x <- if x > 0 then slow(x) else 6") `gives` 6
      (slow ++ "x <- if x < 1 or slow(x) > 0 then 1 else 2") `gives` 1
      (slow ++ "x <- if x > 0 and slow(x) > 0 then 1 else 2") `gives` 2
      (slow ++ "if x > 0:\n  x <- slow(x)\nelse if x < 1:\n  x <- 8\nelse if slow(x) > 0:\n  x <- 9") `gives` 8
      "function f(a):\n  f <- a + 1\nx <- if x < 1 then f(f(2)) else 0" `gives` 4
      "function f(a):\n  f <- a > 0\nx <- if x < 1 and f(1) then 1 else 2" `gives` 1
      "function f(a):\n  f <- a > 0\nx <- if x > 1 or f(-1) then 1 else 2" `gives` 2
      "function f(a):\n  f <- a + 1\nx <- if x < 1 then (if x < 1 then f(1) else f(2)) else f(3)" `gives` 2

    it "is called before each check of a loop's condition, and once for its limit" $ do
      "function f(a):\n  f <- a < 10\nwhile f(x):\n  x <- x + 3" `gives` 12
      "function f(a):\n  f <- a > 10\nuntil f(x):\n  x <- x + 3" `gives` 12
      "function f(a):\n  f <- a < 10\nrepeat:\n  x <- x + 3\nwhile f(x)" `gives` 12
      "function f(a):\n  f <- a < 0\nrepeat:\n  x <- x + 3\nwhile f(x)" `gives` 3
      "function f(a):\n  f <- a + 3\nfunction g(a):\n  g <- a > 10\niterate x -> f(x) until g(x)" `gives` 12
      "function f(a):\n  f <- a + 3\nwhile x < 10:\n  x <- f(x) + 0" `gives` 12
      "function f(a):\n  f <- a + 2\nrepeat f(1) times:\n  x <- x + 1" `gives` 3
      "function f(a):\n  f <- a + 2\nwhile x < 100 up to f(1) times:\n  x <- x + 1" `gives` 3
      "function f(a):\n  f <- a + 2\nfunction g(a):\n  g <- a < 100\nwhile g(x) up to f(1) times:\n  x <- x + 1" `gives` 3

    it "can be differentiated, by `diff`" $
      "function sq(a):\n  sq <- a a\nx <- 3\nx <- diff(sq(x) + x, x)" `gives` 7

    it "reports a type error in its body, not that its value's type is unknown" $ do
      failsWith "function bad(v):\n  bad <- v + true\nx <- bad(1)" "`true` is a truth value"
      failsWith "function bad(v):\n  w := v\n  bad <- w + true\nx <- 1 + bad(1)" "`true` is a truth value"
      failsWith "function f(v):\n  f <- v\nfunction bad(v):\n  bad <- f(v) + true\nx <- bad(1)" "`true` is a truth value"
      failsWith "function bad(v):\n  bad <- nope + 1\nx <- bad(1)" "No variable named nope"
      -- (through a call whose result another call uses)
      failsWith "function tw(g, v):\n  tw <- g(g(v))\nfunction dbl(a):\n  dbl <- 2 a\nx <- tw(dbl, true)"
        "dbl's parameter a is a truth value"

    it "names a parameter in errors as the script does" $ do
      failsWith "sq(a) := a a\nx <- sq(true)" "but sq's parameter a is a truth value"
      failsWith "twice(g, v) := g(g(v))\nx <- twice(w -> w w, true)" "but the parameter w is a truth value"

    it "can't be called where its call can't be lifted out, for now" $ do
      failsWith "function f(a):\n  f <- a\ng(a) := f(a) + 1\nx <- g(1)" "`g` is defined with `:=`"
      failsWith "function f(a):\n  f <- a\nx <- partial(f(x), x)" "`partial` treats everything but its variable"
      failsWith "function f(a):\n  f <- a\nl := transform([1, 2], v -> f(v))\nx <- 1" "`transform` works this out for each `v`"
      failsWith "function f(a):\n  f <- a\nl := find([1, 2], v -> f(v) > 1, 0)\nx <- 1" "`find` works this out for each `v`"

  describe "derivatives through statements" $ do

    let gives input v = runWithX (Scalar RealType 0) input `shouldBe` Right v
        near input v = case runWithX (Scalar RealType 0) input of
          Right got -> abs (got - v) `shouldSatisfy` (< 0.000000001)
          Left err -> expectationFailure err
        failsWith input needle =
          runWithX (Scalar RealType 0) input `shouldSatisfy` either (needle `isInfixOf`) (const False)
        loop3 = "c := 0.5\nz := 0\nk := 0\nwhile k < 3:\n  z <- z^2 + c\n  k <- k + 1\n"

    it "follow a variable back through a loop" $
      -- z_3 = (c^2 + c)^2 + c, so dz_3/dc = 2 (c^2 + c) (2 c + 1) + 1 = 4 at c = 0.5
      (loop3 ++ "x <- diff(z, c)") `gives` 4

    it "follow tuples, through functions that give pairs (as SaddleDrop's Hénon step)" $
      unlines [ "step(p, q, c) := (p^2 + c - 0.3 q, p)"
              , "w := 0.2"
              , "u, v := (w, w)"
              , "du, dv := 1.0, 1.0"
              , "k := 0"
              , "while k < 4:"
              , "  du, dv <- 2 u du - 0.3 dv, du"
              , "  u, v <- step(u, v, 0.4)"
              , "  k <- k + 1"
              , "x <- diff(u, w) - du" ] `near` 0

    it "follow the branch taken" $
      "c := 2.0\ny := 0.0\nif c > 1:\n  y <- c^3\nelse:\n  y <- c\nx <- diff(y, c)" `gives` 12

    it "nest, for higher derivatives, or take an order" $ do
      "c := 2.0\ny := c^3\nx <- diff(diff(y, c), c)" `gives` 12
      "c := 2.0\ny := c^3\nx <- diff(y, c, 2)" `gives` 12
      -- d^2 z_3/dc^2 = 2 (2 c + 1)^2 + 4 (c^2 + c) = 11 at c = 0.5
      (loop3 ++ "x <- diff(z, c, 2)") `gives` 11

    it "work in Newton's method, with the variable changing between passes" $
      unlines [ "w := 1.0"
              , "fw := 1.0"
              , "repeat up to 30 times:"
              , "  fw <- w^2 - 2"
              , "  w <- w - fw / diff(fw, w)"
              , "until |fw| < 0.0000000001"
              , "x <- w" ] `near` sqrt 2

    it "follow |u| with respect to a real variable" $
      "t := 3.0\ny := |t - 5|\nx <- diff(y, t)" `gives` (-1)

    it "can be written as fractions: ⅆf/ⅆz for diff, ∂f/∂z for partial" $ do
      "c := 2.0\ny := c^3\nx <- ⅆy/ⅆc" `gives` 12
      "c := 2.0\ny := c^3\nx <- ∂y/∂c" `gives` 0
      "c := 2.0\nx <- ∂(c^3)/∂c + 1" `gives` 13
      "c := 2.0\ny := c^3\nx <- 2 ⅆ²y/ⅆc²" `gives` 24
      "c := 2.0\ny := c^3\nx <- ⅆ y / ⅆ c" `gives` 12
      "f(t) := t^3\nc := 2.0\nx <- ∂f(c)/∂c" `gives` 12
      -- (a power of a derivative takes parentheses)
      "c := 2.0\ny := c^3\nx <- (ⅆy/ⅆc)^2" `gives` 144
      failsWith "c := 2.0\ny := c^3\nx <- ⅆy/ⅆc^2" "put it in parentheses: (ⅆf/ⅆz)^2"
      failsWith "c := 2.0\ny := c^3\nx <- ⅆ²y/ⅆc³" "the order on top (2) isn't the sum of the orders on the bottom (3)"

    it "can be mixed, with several variables on the bottom" $ do
      -- f = a^2 b^3: ∂²f/∂a∂b = 6 a b^2, ∂³f/∂a∂b² = 12 a b
      "a := 2.0\nb := 3.0\nf := a^2 b^3\nx <- ∂²(a^2 b^3)/∂a∂b" `gives` 108
      "a := 2.0\nb := 3.0\nf := a^2 b^3\nx <- ⅆ²f/ⅆaⅆb" `gives` 108
      "a := 2.0\nb := 3.0\nf := a^2 b^3\nx <- ⅆ³f/ⅆaⅆb²" `gives` 72
      "a := 2.0\nb := 3.0\nf := a^2 b^3\nx <- 2 ⅆ²f/ⅆaⅆb + 1" `gives` 217
      failsWith "a := 2.0\nb := 3.0\nf := a^2 b^3\nx <- ⅆ²f/ⅆaⅆb²" "isn't the sum of the orders on the bottom (3)"

    it "leave partial to the expression alone" $ do
      "c := 2.0\ny := c^3\nx <- partial(y, c)" `gives` 0
      "c := 2.0\nx <- partial(c^3, c)" `gives` 12
      "c := 2.0\nx <- partial(c^3, c, 2)" `gives` 12

    it "say when the variable changes in between" $ do
      failsWith "c := 2.0\ny := c^2\nc <- 3\nx <- diff(y, c)" "`y` was worked out from an earlier value of `c`"
      failsWith "w := 2.0\nw <- w^2\nx <- diff(w, w)" "`w` changes before this, from its own value"

    it "say what they can't follow, rather than give zero" $ do
      failsWith "c := 2.0\nxs := [c, c^2]\ny := xs @ 2\nx <- diff(y, c)" "through an item of a list or table"
      failsWith "c := 2.5\ny := mod(c, 1.0)\nx <- diff(y, c)" "which jumps"

  describe "functions given as arguments" $ do

    let gives input v = runWithX (Scalar RealType 0) input `shouldBe` Right v
        failsWith input needle =
          runWithX (Scalar RealType 0) input `shouldSatisfy` either (needle `isInfixOf`) (const False)

    it "can be given to a one-line function, by name or written in place" $ do
      "sq(a) := a a\ntwice(g, v) := g(g(v))\nx <- twice(sq, 3)" `gives` 81
      "twice(g, v) := g(g(v))\nx <- twice(w -> w + 1, 3)" `gives` 5
      "ap(g, a, b) := g(a, b)\nx <- ap((u, v) -> u - v, 5, 2)" `gives` 3

    it "are applied however the body writes it: g(x), g x, 2 g(x), g(x)^2" $ do
      "sq(a) := a a\nh(g, v) := g(v)^2\nx <- h(w -> w + 1, 2)" `gives` 9
      "sq(a) := a a\nh(g, v) := 2 g(v)\nx <- h(sq, 3)" `gives` 18
      "h(g, v) := g v + 1\nx <- h(w -> 10 w, 3)" `gives` 31

    it "keep the names where they're written" $
      -- (the function's `x` is the caller's, not h's parameter)
      "h(g, x) := g(x) + x\nx <- h(w -> w + x, 2)" `gives` 4

    it "can be differentiated with g'(x)" $ do
      "sq(a) := a a\nh(g, v) := g'(v)\nx <- h(sq, 3)" `gives` 6
      "h(g, v) := g''(v)\nx <- h(w -> w^3, 2)" `gives` 12
      "newton(f, z) := z - f(z) / f'(z)\nx <- newton(w -> w^2 - 4, 4)" `gives` 2.5
      "sq(a) := a a\nh(g, v) := g'(v)\nk(g, v) := h(g, v)\nx <- k(sq, 5)" `gives` 10

    it "can be composed with ∘, and the composition applied" $ do
      "sq(a) := a a\ninc(a) := a + 1\nx <- (sq ∘ inc)(2)" `gives` 9
      "sq(a) := a a\ninc(a) := a + 1\nx <- (inc ∘ sq)(2)" `gives` 5
      "sq(a) := a a\ninc(a) := a + 1\nx <- (inc ∘ sq ∘ inc)(2)" `gives` 10
      "sq(a) := a a\nx <- (sq ∘ (w -> w - 1))(4) + 1" `gives` 10
      "sq(a) := a a\nx <- (sq ∘ (w -> w - 1))(4)^2" `gives` 81
      "sq(a) := a a\nh(g, v) := (sq ∘ g)(v)\nx <- h(w -> 3 w, 1)" `gives` 9
      "sq(a) := a a\ny := (sq ∘ (w -> cos(w)))(0)\nx <- y + 1" `gives` 2

    it "can give a composition to a function" $ do
      "sq(a) := a a\ninc(a) := a + 1\ntwice(g, v) := g(g(v))\nx <- twice(sq ∘ inc, 1)" `gives` 25
      "inc(a) := a + 1\nh(g, v) := g'(v)\nx <- h((w -> w^3) ∘ inc, 1)" `gives` 12

    it "can differentiate a composition" $ do
      "sq(a) := a a\ninc(a) := a + 1\nx <- (sq ∘ inc)'(2)" `gives` 6
      "sq(a) := a a\nx <- (sq ∘ sq)''(1)" `gives` 12

    it "spreads a tuple over a function's parameters" $ do
      "sub(a, b) := a - b\npair(t) := (t + 5, t)\nx <- (sub ∘ pair)(2)" `gives` 5
      "sub(a, b) := a - b\npair(t) := (t + 5, t)\nx <- sub(pair(2))" `gives` 5
      "f(a, b, c) := a - b c\ntrip(t) := (t, t + 1, t + 2)\nx <- f(trip(1))" `gives` (-5)
      "pair(t) := (t + 5, t)\nx <- (((u, v) -> u v) ∘ pair)(1)" `gives` 6

    it "rejects composing something that isn't a function" $
      "sq(a) := a a\nh(g, v) := (sq ∘ g)(v)\nx <- h(3, 1)" `failsWith` "isn't a function"

    it "rejects spreading a value that isn't a tuple" $
      "sub(a, b) := a - b\nsq(a) := a a\nx <- sub(sq(2))" `failsWith` "I expected a tuple"

    it "composes functions with a body, applied" $ do
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- (sq ∘ dbl)(1)" `gives` 4
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- (dbl ∘ sq)(3)" `gives` 18
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- (dbl ∘ inc ∘ dbl)(1)" `gives` 6
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- (dbl ∘ dbl)(2) + (inc ∘ dbl)(1)" `gives` 11
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- ((w -> dbl(w) + 1) ∘ inc)(1)" `gives` 5

    it "applies a composition with a body only when it would run" $
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- if x > 100 then (dbl ∘ dbl)(x) else 7" `gives` 7

    it "gives compositions to functions with a body, and code blocks" $ do
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\ndefine ap(g):\n  x <- g(x)\nx <- 3\nrun ap(dbl ∘ inc)" `gives` 8
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\ndefine ap(g):\n  x <- g(x)\nx <- 3\nrun ap(sq ∘ inc)" `gives` 16
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nfunction twice(g, a):\n  b := g(a)\n  twice <- g(b)\nx <- twice(dbl ∘ inc, 1)" `gives` 10
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nfunction h(g, v):\n  h <- g'(v)\nx <- h(sq ∘ inc, 2)" `gives` 6

    it "spreads a tuple from a function with a body" $
      "function pair(t):\n  pair <- (t, t + 1)\nsub(a, b) := a - b\nx <- (sub ∘ pair)(2) + sub(pair(5))" `gives` (-2)

    it "applies a function written in place that calls a function with a body" $
      "function dbl(a):\n  dbl <- 2 a\nfunction ap(g, v):\n  ap <- g(v) + 1\nx <- ap(w -> dbl(w) + 1, 2)" `gives` 6

    it "differentiates functions with a body, and compositions with them" $ do
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ninc(a) := a + 1\nx <- (sq ∘ dbl)'(1)" `gives` 8
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ninc(a) := a + 1\nx <- cube'(2)" `gives` 12
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ninc(a) := a + 1\nx <- cube''(2)" `gives` 12
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ninc(a) := a + 1\nx <- (sq ∘ cube)'(1)" `gives` 6
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ninc(a) := a + 1\nx <- (inc ∘ cube ∘ dbl)'(1)" `gives` 24
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ninc(a) := a + 1\nx <- ((w -> cube(w) + w) ∘ inc)'(1)" `gives` 13
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ninc(a) := a + 1\nx <- if x > 100 then cube'(x) else 7" `gives` 7
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ninc(a) := a + 1\nfunction h(g, v):\n  h <- g'(v)\nx <- h(cube, 2) + h(sq ∘ cube, 1)" `gives` 18
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ninc(a) := a + 1\ndefine ap(g):\n  x <- g'(x)\nx <- 2\nrun ap(dbl ∘ cube)" `gives` 24

    it "composes built-in functions" $ do
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\ntwice(g, v) := g(g(v))\nx <- (sq ∘ cos)(0) + (cos ∘ sq)(0)" `gives` 2
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\ntwice(g, v) := g(g(v))\nx <- (floor ∘ (w -> w + 0.5))(2)" `gives` 2
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\ntwice(g, v) := g(g(v))\nx <- (re ∘ exp)(0)" `gives` 1
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\ntwice(g, v) := g(g(v))\nx <- (sin ∘ (w -> 2 w))'(0)" `gives` 2
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\ntwice(g, v) := g(g(v))\nx <- (sin ∘ dbl)'(0)" `gives` 2
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\ntwice(g, v) := g(g(v))\nx <- twice(floor ∘ (w -> w + 1.5), 0)" `gives` 2
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\ntwice(g, v) := g(g(v))\ndefine ap(g):\n  x <- g(x)\nx <- 1.3\nrun ap(floor ∘ dbl)" `gives` 2

    it "gives functions with a body to one-line functions" $ do
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ntwice(g, v) := g(g(v))\nap(g, v) := g(v) + 1\nx <- twice(dbl, 1)" `gives` 4
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ntwice(g, v) := g(g(v))\nap(g, v) := g(v) + 1\nx <- twice(sq ∘ dbl, 1)" `gives` 64
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ntwice(g, v) := g(g(v))\nap(g, v) := g(v) + 1\nx <- ap(w -> dbl(w) + 1, 2)" `gives` 6
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ntwice(g, v) := g(g(v))\nap(g, v) := g(v) + 1\nx <- 1 + twice(dbl, 1)" `gives` 5
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ntwice(g, v) := g(g(v))\nap(g, v) := g(v) + 1\nx <- if x > 1 then twice(dbl, 1) else 3" `gives` 3
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ntwice(g, v) := g(g(v))\nap(g, v) := g(v) + 1\nx <- twice(sq, 3) + twice(dbl, 3)" `gives` 93
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ntwice(g, v) := g(g(v))\nap(g, v) := g(v) + 1\nh(g, v) := g'(v)\nx <- h(cube, 2)" `gives` 12
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ntwice(g, v) := g(g(v))\nap(g, v) := g(v) + 1\nfour(g, v) := twice(g, twice(g, v))\nx <- four(dbl, 1)" `gives` 16
      "function dbl(a):\n  dbl <- 2 a\nfunction cube(a):\n  b := a a\n  cube <- a b\nsq(a) := a a\ntwice(g, v) := g(g(v))\nap(g, v) := g(v) + 1\ndefine go(g):\n  x <- twice(g, x)\nx <- 3\nrun go(dbl)" `gives` 12

    it "can be iterated with the standard library's compose" $ do
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- compose(sq, 3, 2)" `gives` 256
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- compose(dbl, 3, 1)" `gives` 8
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- compose(w -> w + 1, 5, 0)" `gives` 5
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- compose(sq ∘ inc, 2, 1)" `gives` 25
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- compose(dbl, 0, 7)" `gives` 7
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- 1 + compose(dbl, 2, 1) + compose(inc, 2, 1)" `gives` 8
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\ninc(a) := a + 1\nx <- compose(w -> dbl(w) + 1, 2, 0)" `gives` 3
      "function dbl(a):\n  dbl <- 2 a\nsq(a) := a a\nx <- 3\nx <- diff(compose(sq, 2, x), x)" `gives` 108

    it "multiply when the argument is a number" $
      "h(g, v) := g(v)\nx <- h(3, 2)" `gives` 6

    it "can be given to a code block" $ do
      "define apply_to_x(g):\n  x <- g(x)\nx <- 3\nrun apply_to_x(w -> w w)" `gives` 9
      "function cube(a):\n  cube <- a a a\ndefine apply_to_x(g):\n  x <- g(x)\nx <- 2\nrun apply_to_x(cube)" `gives` 8

    it "can be given to a function with a body, even another such function" $ do
      unlines [ "function iter(g, z, n):"
              , "  w := z"
              , "  k := 0"
              , "  while k < n:"
              , "    w <- g(w)"
              , "    k <- k + 1"
              , "  iter <- w"
              , "sq(a) := a a"
              , "x <- iter(sq, 2, 3)" ] `gives` 256
      unlines [ "function inc(a):"
              , "  inc <- a + 1"
              , "function twice(g, a):"
              , "  b := g(a)"
              , "  c := g(b)"
              , "  twice <- c"
              , "x <- twice(inc, 1)" ] `gives` 3

    it "take `previous` in a body to the body's loop, and in an argument to the caller's" $ do
      unlines [ "function last_step(a):"
              , "  w := a"
              , "  d := 0"
              , "  k := 0"
              , "  while k < 3:"
              , "    w <- 2 w"
              , "    d <- w - previous w"
              , "    k <- k + 1"
              , "  last_step <- d"
              , "x <- last_step(1)" ] `gives` 4
      -- (`previous z` is z at the start of the caller's pass, 5, not z in
      -- the body's loop, which is 7 throughout)
      unlines [ "function total(g, n):"
              , "  s := 0"
              , "  k := 0"
              , "  while k < n:"
              , "    s <- s + g(k)"
              , "    k <- k + 1"
              , "  total <- s"
              , "z := 5"
              , "k := 0"
              , "while k < 1:"
              , "  z <- 7"
              , "  x <- total(j -> previous z, 2)"
              , "  k <- k + 1" ] `gives` 10

    it "can be given on to another call" $
      "sq(a) := a a\nap(g, v) := g(v)\nh(g, v) := ap(g, v) + 1\nx <- h(sq, 3)" `gives` 10

    it "are functions, not values, anywhere else" $ do
      failsWith "sq(a) := a a\ny := sq\nx <- 1" "`sq` is a function"
      failsWith "f(a) := a + 1\nsq(a) := a a\nx <- f(sq)" "a function, so it can only be given to a function or a code block"

  describe "points of the Riemann sphere" $ do

    let gives input v = runSphere input `shouldSatisfy` either (const False) (\z -> magnitude (z - v) < 1.0e-9)
        failsWith input needle =
          runSphere input `shouldSatisfy` either (needle `isInfixOf`) (const False)

    it "do arithmetic in Ĉ, as complex numbers do" $ do
      "w : Ĉ := 2 + i\nw <- w^2 + 1\nx <- affine(1, w)" `gives` (4 :+ 4)
      "w : Cinf := 3\nv : Ĉ := (2 w + 1) / (w - 1)\nx <- affine(1, v)" `gives` 3.5
      "p := [2 : 1]\nw := (p : Ĉ) + 1\nx <- affine(1, w)" `gives` 3

    it "are right at ∞, within an expression" $ do
      "p := ∞\nq : Ĉ := (p^2 - 2) / (p^2 - 1)\nx <- affine(1, q)" `gives` 1
      "p := infinity\nq : Ĉ := (2 p + 1) / (3 p - 1)\nx <- affine(1, q)" `gives` (2 / 3)
      -- Newton's method for z^3 - 1 fixes ∞, and 1 / 0 is ∞
      "p := ∞\nq : Ĉ := p - (p^3 - 1) / (3 p^2)\nx <- if q = ∞ and q escapes then 1 else 0" `gives` 1
      "w : Ĉ := 0\nq : Ĉ := 1 / w\nx <- if q = ∞ then 1 else 0" `gives` 1
      "f(w) := (w^2 - 2) / (w^2 - 1)\nq : Ĉ := f(∞)\nx <- affine(1, q)" `gives` 1

    it "leave ∞ - ∞ undefined, equal to nothing" $
      "p := ∞\nq : Ĉ := p - p\nx <- if q = 0 or q = 1 or q = ∞ then 1 else 0" `gives` 0

    it "iterate to ∞ without overflowing, and stop there" $
      "w : Ĉ := 3\niterate w -> w^2 until w stops up to 50 times\nx <- if w = ∞ then 1 else 0" `gives` 1

    it "are in the chart where they're nearest the origin, or a given affine chart" $ do
      "w, k := chart((3 : Ĉ))\nx <- w + k" `gives` (1 / 3)
      "w, k := chart((0.5 : Ĉ))\nx <- w + 10 k" `gives` 10.5
      "w, k := chart(∞)\nx <- w + k" `gives` 0
      "x <- affine(0, [2 : 6])" `gives` 3
      "x <- affine(1, [2 : 6]) + affine(0, ∞)" `gives` (1 / 3)

    it "are compared by the chordal distance" $ do
      "x <- if [2 : 1] = [4 : 2] then 1 else 0" `gives` 1
      "x <- chordal(0, ∞)" `gives` 2
      "x <- chordal(1, i)" `gives` sqrt 2

    it "can be taken apart into homogeneous coordinates, and maps made from them" $ do
      "p := [2 : 1]\n[a : b] := p\nq := [a^2 + b^2 : b^2]\nx <- affine(1, q)" `gives` 5
      "p : ℙ¹ := [1 : 0]\n[a : b] := p\nq := [a^2 - 2 b^2 : a^2 - b^2]\nx <- affine(1, q)" `gives` 1
      "p := [6 : 2]\n[a : b] := p\nx <- a / b" `gives` 3
      "w : Ĉ := 3\n[a : b] := w\nx <- a / b" `gives` 3

    it "have their types' other names" $ do
      "w : Cinf := 2\nv : ℂ\x0302 := w\nx <- affine(1, v)" `gives` 2
      "p : P1 := [1 : 1]\nq : ℙ¹ := p\nr : P^1 := q\ns : ℙ := r\nx <- affine(1, s)" `gives` 1

    it "can be a function's value, and be in lists" $ do
      "function g(w):\n  g <- 1 / w\nq : Ĉ := g((0 : Ĉ))\nx <- if q = ∞ then 1 else 0" `gives` 1
      "ps := [∞, 0, 1]\nfor each p in ps:\n  if p escapes:\n    x <- x + 1" `gives` 1
      "ps := [∞, 2]\nq := find(ps, p -> p = 5, ∞)\nx <- if q = ∞ then 1 else 0" `gives` 1
      "ps := [∞, 2]\nx <- affine(1, ps @ 2) + affine(1, find(ps, p -> p = 2, 0))" `gives` 4

    it "aren't numbers" $ do
      failsWith "w : Ĉ := 1\nx <- sin(w)" "`w` is a point of the Riemann sphere"
      failsWith "w : Ĉ := 1\ny := |w|" "use `affine(1, w)`"
      failsWith "w : Ĉ := 1\nx <- w" "is a point of the Riemann sphere"
      failsWith "w : Ĉ := 1\ny := w < 2" "`w` is a point of the Riemann sphere"
      failsWith "w : Ĉ := 2\ny := partial(w^2, w)" "depends on the chart"
      failsWith "w : Ĉ := 2\nn := 2\ny : Ĉ := w^n" "a whole number for its exponent"

    it "of ℙ¹ have no arithmetic" $
      failsWith "p := [1 : 2]\nq := p + 1" "A point of ℙ¹ has no arithmetic"

    it "have homogeneous coordinates only up to scale" $ do
      failsWith "p := [1 : 2]\n[a : b] := p\ny := a" "only defined up to scale"
      failsWith "p := [1 : 2]\n[a : b] := p\ny := a + 1" "only defined up to scale"
      failsWith "p := [1 : 2]\n[a : b] := p\nq := [a^2 : b]" "of the same degree"
      failsWith "p := [1 : 2]\n[a : b] := p\na <- 2" "can't be changed"

    it "are only in ℙ¹ for now" $
      failsWith "x <- affine(1, [1 : 2 : 3])" "higher-dimensional projective spaces"

    it "leave lists of values with types to parentheses" $
      runWithX (Scalar RealType 0) "xs := [(1 : R), 2]\nx <- xs @ 1" `shouldSatisfy` either (const False) (const True)

  describe "when a built-in name is used for the script's own" $ do

    let failsWith input needle =
          runWithX (Scalar RealType 0) input `shouldSatisfy` either (needle `isInfixOf`) (const False)
        gives input v = runWithX (Scalar RealType 0) input `shouldBe` Right v

    it "takes it for the script's own: a variable, function, or code block, not the built-in" $ do
      "define foo(bar):\n  bar <- bar + 1\n  x <- bar\nrun foo(2)" `gives` 3
      "sin(t) := t + 1\nx <- sin(1)" `gives` 2
      "define re:\n  x <- 5\nrun re" `gives` 5
      "for each red in [1, 2]:\n  x <- x + red" `gives` 3
      "fall := 2.0\nx <- fall + 1" `gives` 3
      "x <- transform([1.0], arg -> arg + 1) @ 1" `gives` 2
      -- (a variable followed by parentheses is a product, as for any variable)
      "re := 2.0\nx <- re(3)" `gives` 6
      "point := 1.5\nx <- point" `gives` 1.5
      failsWith "rgb := 2.0\nx <- rgb(1, 2)"
        "`rgb` is this script's own variable here, not the built-in, so `rgb(...)` multiplies it"

    it "but a statement's own word is the statement's" $ do
      "pixels := 2.0\nx <- pixels" `gives` 2
      runWithX (Scalar RealType 0) "pixels := 2.0\ndraw filled circle at 0 with radius 4 pixels\nx <- pixels"
        `shouldBe` Right 2

    it "but not a keyword" $ do
      failsWith "i := 0" "`i` is the imaginary unit, so it can't also name a variable. Try another name (k, j, n, ...)."
      failsWith "length : Z := 3" "`length` is a built-in function, so it can't also name a variable"
      failsWith "define range:\n  pass" "`range` is a built-in function, so it can't also name a code block"

    it "leaves built-ins used as built-ins alone" $ do
      runWithX (Scalar RealType 0) "x <- re(2 + i) + cos(0)" `shouldBe` Right 3.0
      runWithX (Scalar RealType 0) "f(t) := t + 1\nx <- f(re(i))" `shouldBe` Right 1.0
