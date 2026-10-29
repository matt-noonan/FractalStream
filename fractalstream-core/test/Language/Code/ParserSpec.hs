{-# language QuasiQuotes #-}
module Language.Code.ParserSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Value.Typecheck
import Language.Code.Parser
import Language.Code.Simulator
import Language.Draw
import Data.Color

import Text.RawString.QQ

{-
splices :: Splices
splices = Map.singleton internalIterationLimit (ParsedValue NoSourceRange f)
  where
    f :: (forall env ty. (KnownEnvironment env, KnownType ty)
      => TypeProxy ty
      -> TC (Value '(env, ty)))
    f = \case
      IntegerType -> pure (Const $ Scalar IntegerType 100)
      _ -> error "internal error"
-}
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
     $ parseCode env noSplices ("n : Z <- 0\n" ++ input)

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
     $ parseCode (envProxy Proxy) noSplices ("n : Z <- 0\n" ++ input)

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
     $ parseCode (envProxy Proxy) noSplices ("n : Z <- 0\n" ++ input)

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
y : Z <- x - 2
if true:
  x <- 2 * y
else:
  pass
|]
           p2 = [r|
x <- 5
y : Z <- x - 2
if true:
  x <- 2 * y
|]

       runWithX (Scalar IntegerType 0) p1 `shouldBe` Right 6
       runWithX (Scalar IntegerType 0) p2 `shouldBe` Right 6

    it "can coerce variable types" $ do
        let p1 = [r|
k : Z <- 1
x <- k|]
        runWithX (Scalar RealType 0) p1 `shouldBe` Right 1.0

  describe "when parsing more complex code" $ do

    it "can parse a checkered Mandelbrot program" $ do
      let mandel = [r|
C : C <- x + y i
z : C <- 0
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
      runWithX (Scalar RealType 0) "k := 3\nk <- 0.5\nx <- k"
        `shouldSatisfy` isLeft

    it "rejects redeclaring a variable" $
      runWithX (Scalar RealType 0) "x := 3" `shouldSatisfy` isLeft

  describe "when defining functions" $ do

    it "inlines calls" $
      runWithX (Scalar RealType 0) "let f(t) = 2 t + 1\nx <- f(3) + f(0.5)" `shouldBe` Right 9.0

    it "specializes each call at its own argument types" $
      runWithXY (Scalar IntegerType 0) (Scalar ComplexType (0 :+ 1)) [r|
let sq(t) = t * t
x <- sq(3)
y <- sq(y)|] `shouldBe` Right 9

    it "supports several parameters, nested calls, and calls between functions" $
      runWithX (Scalar RealType 0) [r|
let f(a, b) = a - b
let g(t) = f(t, f(1, t))
x <- g(f(5, 2))|] `shouldBe` Right 5.0

    it "lets parameters shadow variables in scope" $
      runWithX (Scalar RealType 2) "let f(x) = x + 1\nx <- f(10)" `shouldBe` Right 11.0

    it "captures variables in scope at the definition" $
      runWithX (Scalar RealType 0) "c : R <- 4\nlet f(t) = t + c\nx <- f(1)" `shouldBe` Right 5.0

    it "scopes functions to the rest of their block" $
      runWithX (Scalar RealType 0) [r|
if true:
  let f(t) = t
x <- f(1)|] `shouldSatisfy` isLeft

    it "rejects bodies that refer to undefined variables" $
      runWithX (Scalar RealType 0) "let f(t) = t + q\nx <- f(1)" `shouldSatisfy` isLeft

    it "rejects calls with the wrong number of arguments" $
      runWithX (Scalar RealType 0) "let f(a, b) = a + b\nx <- f(1)" `shouldSatisfy` isLeft

    it "rejects function names that clash with variables" $
      runWithX (Scalar RealType 0) "let x(t) = t\npass" `shouldSatisfy` isLeft
