module Language.StdlibSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Data.Fixed (mod')
import Data.List (isInfixOf)
import qualified Data.Set as Set

import Language.Type
import Language.Value
import Language.Value.Builtins (InternalIterations, InternalIterationLimit, InternalStuck)
import Language.Code.Parser
import Language.Value.Parser (stdlibDefinitions, stdlibBlockFunctions)
import Language.Code.Syntax
import Language.Code.Simulator
import Language.Draw
import Language.Stdlib
import Data.Color

-- | The color a piece of code leaves in @color@, given a real @x@.
colorWith :: Double -> String -> Either String Color
colorWith = colorWithDefs noCommonDefs

-- | 'colorWith', with these common definitions.
colorWithDefs :: CommonDefs -> Double -> String -> Either String Color
colorWithDefs defs x input =
  let ctx = Bind (Proxy @"x") RealType x
          $ Bind (Proxy @"color") ColorType grey
          $ Bind (Proxy @InternalIterations) IntegerType 0
          $ Bind (Proxy @InternalIterationLimit) IntegerType 100
          $ Bind (Proxy @InternalStuck) BooleanType False
          $ EmptyContext
  in first (`ppFullError` input)
     $ fmap ((`evalState` (ctx, ()))
             . (\c -> simulate noDraw c >> eval (Var (Proxy @"color") ColorType bindingEvidence)))
     $ parseCode (envProxy Proxy) defs input

-- | The standard library, with its own @rainbow@ replaced by one with a
-- body (written like the standard library's source), defined after the
-- rest (so it can use them).
withBlockRainbow :: String -> CommonDefs
withBlockRainbow src = noCommonDefs { commonCode = filter (not . isRainbow) stdlibCode ++ parseStdlib src }
  where isRainbow (Stmt _ def) = case def of
          SFun f _ _ -> f == stdlibName "rainbow"
          _ -> False

blockRainbow :: String
blockRainbow = unlines
  [ "function rainbow(t):"
  , "  s := mod(t, 1.0)"
  , "  rainbow <- if s < 0.5 then dark red else blue" ]

noDraw :: DrawHandler (HaskellTypeM ())
noDraw = DrawHandler (const $ pure ())

-- | The color wheels as they were built into FractalStream before they
-- moved into the standard library: eight colors over [0, 1), blending
-- from each into the next.
referenceWheel :: [Color] -> Double -> Color
referenceWheel cs t =
  let u = t `mod'` 1
      k = length (takeWhile (<= u) [0.125, 0.25 .. 0.875])
      wheel = cs ++ take 1 cs
  in mixColors (8 * (u - fromIntegral k * 0.125)) (wheel !! (k + 1)) (wheel !! k)

rainbowColors, arenbergColors, fireColors :: [Color]
rainbowColors = [red, orange, yellow, green, rgbToColor (0, 128, 128), blue, purple, violet]
arenbergColors = map rgbToColor
  [ (255, 80, 81), (255, 174, 0), (152, 190, 0), (0, 183, 74)
  , (0, 237, 232), (0, 172, 255), (184, 161, 255), (255, 134, 226) ]
fireColors = let cs = map rgbToColor [ (0xff, 0x3d, 0x3d), (0xff, 0x7b, 0x39), (0xff, 0xab, 0x47)
                                     , (0xff, 0xd4, 0x67), (0xff, 0xfa, 0x94) ]
             in cs ++ reverse (drop 1 (take 4 cs))

samples :: [Double]
samples = [-1.3, -0.5, 0, 0.01, 0.0625, 0.124, 0.125, 0.2, 0.3, 0.4375, 0.5, 0.61, 0.7, 0.8, 0.875, 0.93, 0.999, 1, 2.71]

spec :: Spec
spec = do
  describe "The standard library" $ do

    it "parses, and its definitions go by names no script can write" $ do
      stdlibCode `shouldSatisfy` (not . null)
      forM_ stdlibCode $ \(Stmt _ def) -> case def of
        SFun f _ _ -> f `shouldSatisfy` isJust . fromStdlibName
        SFunction f _ _ -> f `shouldSatisfy` isJust . fromStdlibName
        other -> expectationFailure ("not a function definition: " ++ show other)
      Set.fromList ["rainbow", "cycle", "dark", "pixel_size_at", "acos"]
        `shouldSatisfy` (`Set.isSubsetOf` stdlibFunctionNames)

    it "has the same definitions for values outside a script as for scripts" $ do
      let byKind = [ (isBlock, f) | Stmt _ def <- stdlibCode
                                  , (isBlock, Just f) <- case def of
                                      SFun g _ _ -> [(False, fromStdlibName g)]
                                      SFunction g _ _ -> [(True, fromStdlibName g)]
                                      _ -> [] ]
          blocks = Set.fromList [ f | (True, f) <- byKind ]
      blocks `shouldSatisfy` (not . Set.null)
      stdlibBlockFunctions `shouldBe` blocks
      [ f | (f, _, _) <- stdlibDefinitions ] `shouldBe` [ f | (False, f) <- byKind ]

    it "has the same color wheels as before" $
      forM_ [("rainbow", rainbowColors), ("arenberg", arenbergColors), ("fire", fireColors)] $ \(name, cs) ->
        forM_ samples $ \t ->
          (name, t, colorWith t ("color <- " ++ name ++ "(x)")) `shouldBe` (name, t, Right (referenceWheel cs t))

    it "has cycle, dark, and light" $ do
      colorWith 0 "color <- cycle(x, blue, red)" `shouldBe` Right blue
      colorWith 0.5 "color <- cycle(x, blue, red)" `shouldBe` Right red
      colorWith 1.25 "color <- cycle(x, blue, red)" `shouldBe` Right (mixColors 0.5 red blue)
      colorWith 0.25 "color <- dark cycle(x, blue, red)" `shouldBe` Right (mixColors 0.333 black (mixColors 0.5 red blue))
      colorWith 0 "color <- dark red" `shouldBe` Right (mixColors 0.333 black red)
      colorWith 0 "color <- light light red" `shouldBe` Right (mixColors 0.333 white (mixColors 0.333 white red))

    it "gives way to a script's own definitions of its names" $ do
      colorWith 0.3 "rainbow(t) := green\ncolor <- rainbow(x)" `shouldBe` Right green
      colorWith 0 "fire := blue\ncolor <- fire" `shouldBe` Right blue

    it "uses its own definitions, not a script's" $
      -- (the standard library's rainbow uses its wheel8)
      colorWith 0.0625 "wheel8(t) := black\ncolor <- if x > 1 then wheel8(x) else rainbow(x)"
        `shouldBe` Right (referenceWheel rainbowColors 0.0625)

  describe "A standard library function with a body" $ do

    it "can be called as the whole right-hand side of an assignment" $ do
      colorWithDefs (withBlockRainbow blockRainbow) 1.25 "color <- rainbow(x)"
        `shouldBe` Right (mixColors 0.333 black red)
      colorWithDefs (withBlockRainbow blockRainbow) 0.75 "c := rainbow(x)\ncolor <- c"
        `shouldBe` Right blue

    it "can't be called anywhere else, like any function with a body" $
      colorWithDefs (withBlockRainbow blockRainbow) 0 "color <- light rainbow(x)"
        `shouldSatisfy` \case
          Left err -> "`rainbow` is a function with a body" `isInfixOf` err
                      && not ("[std]" `isInfixOf` err)
          Right _ -> False

    it "uses the standard library's definitions, not a script's" $
      colorWithDefs (withBlockRainbow blockRainbow) 0.25 "dark(c) := green\ncolor <- rainbow(x)"
        `shouldBe` Right (mixColors 0.333 black red)

    it "is said to be at the call, in errors in its body" $
      colorWithDefs (withBlockRainbow "function rainbow(t):\n  rainbow <- t + red\n") 0
        "x <- 2 x\ncolor <- rainbow(x)"
        `shouldSatisfy` \case
          Left err -> "color <- rainbow(x)\n" `isInfixOf` err && "^^^" `isInfixOf` err
          Right _ -> False
