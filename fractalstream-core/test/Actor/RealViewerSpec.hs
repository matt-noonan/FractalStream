{-# language QuasiQuotes #-}
-- | Real viewers: @\@viewer ... xvar=x yvar=y@, with a pair of real
-- coordinates and pixels that needn't be square.
module Actor.RealViewerSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Actor.Script
import Actor.Ensemble (checkScript, buildEnsembleFromScript, Ensemble(..))
import Actor.Event
import Actor.Viewer (defaultIterLimit, defaultMaxRadius, defaultMinRadius)
import Actor.Viewer.Complex (ComplexViewer(..))
import Data.DynamicValue
import Language.Code.Parser (noCommonDefs)
import Language.Draw (DrawSink(..))
import Language.Environment
import Language.Type

import Data.List (isInfixOf)
import Text.RawString.QQ

spec :: Spec
spec = do

  describe "a real viewer's header" $ do

    it "names its two coordinates, center, and aspect" $
      case parseScript [r|
@viewer "Plane" xvar=x yvar=y center=(1, -1/2) pixelSize=1/64 aspect=2
color <- if x < y then red else blue
|] of
        Right [ViewerStanza{..}] -> do
          vsCoord `shouldBe` RealCoordinates "x" "y"
          vsCenter `shouldBe` Just "(1, -1/2)"
          vsPixelSize `shouldBe` Just "1/64"
          vsAspect `shouldBe` Just "2"
        Right other -> expectationFailure ("expected one viewer, got " ++ show (length other) ++ " stanzas")
        Left (_, err) -> expectationFailure err

    it "keeps `var=` (or the default `z`) for a complex viewer" $
      case parseScript "@viewer \"V\"\npass\n" of
        Right [ViewerStanza{..}] -> vsCoord `shouldBe` ComplexCoordinate "z"
        _ -> expectationFailure "expected one viewer"

    let headerError hdr needle = case parseScript (hdr ++ "\npass\n") of
          Left (_, err) -> err `shouldSatisfy` isInfixOf needle
          Right _ -> expectationFailure ("expected an error for " ++ hdr)

    it "rejects both `var=` and `xvar=`/`yvar=`" $
      headerError "@viewer \"V\" var=z xvar=x yvar=y" "not both"
    it "rejects `xvar=` without `yvar=`, and the other way around" $ do
      headerError "@viewer \"V\" xvar=x" "also needs `yvar=`"
      headerError "@viewer \"V\" yvar=y" "also needs `xvar=`"
    it "rejects the same variable for both coordinates" $
      headerError "@viewer \"V\" xvar=x yvar=x" "must be different"
    it "rejects `aspect=` on a complex viewer" $
      headerError "@viewer \"V\" var=z aspect=2" "pixels are always square"

  describe "building a real viewer" $ do

    let ok src = checkScript src `shouldBe` Right ()
        failsWith src needle = case checkScript src of
          Left (_, err) -> err `shouldSatisfy` isInfixOf needle
          Right () -> expectationFailure "expected an error"

    it "binds its coordinates as reals, with its pixel width and height" $
      ok [r|
@viewer "Plane" xvar=x yvar=y
w : R := x + y
color <- if pixel_height > pixel_width + pixel_size then red else blue
|]

    it "doesn't give a complex viewer a pixel width or height" $
      failsWith [r|
@viewer "Plane" var=z
color <- if pixel_height > 0 then red else blue
|] "pixel_height"

    it "sets real configuration variables named as its coordinates" $
      ok [r|
@config "Parameters"
a : R := 1.4
b : R := 0.3

@viewer "Parameter plane" xvar=a yvar=b
color <- if a > b then red else blue
|]

    it "rejects a coordinate that's a complex configuration variable" $
      failsWith [r|
@config "Parameters"
a : C := 0
b : R := 0

@viewer "Parameter plane" xvar=a yvar=b
pass
|] "`a` should be a real number"

    it "rejects a variable already named `pixel_height`" $
      failsWith [r|
@config "Parameters"
pixel_height : R := 1

@viewer "Plane" xvar=x yvar=y
pass
|] "pixel_height"

    it "takes a center as a point or a complex number, but not anything else" $ do
      ok "@viewer \"Plane\" xvar=x yvar=y center=(1, 2)\npass\n"
      ok "@viewer \"Plane\" xvar=x yvar=y center=\"1 + 2i\"\npass\n"
      failsWith "@viewer \"Plane\" xvar=x yvar=y center=\"red\"\npass\n" "must be a point"
      failsWith "@viewer \"Plane\" var=z center=(1, 2)\npass\n" ""

    it "needs a positive aspect" $
      failsWith "@viewer \"Plane\" xvar=x yvar=y aspect=-1\npass\n" "must be positive"

    it "lets its tools name events by real or complex coordinates" $
      ok [r|
@viewer "Plane" xvar=x yvar=y

pass

@tool "Lines"
on click:
  draw point at (x, y)
on drag from (p, q) to (u, v):
  draw line from (p, q) to (u, v)
  write text(pixel_width, " by ", pixel_height) at (u, v)
on drag-finished from w0 to w1:
  draw line from w0 to w1
on refresh:
  draw circle at (0, 0) with radius pixel_height
|]

    it "builds with its coordinates, center, and aspect" $ do
      result <- buildEnsembleFromScript [r|
@config "Parameters"
a : R := 1.4
b : R := 0.3

@viewer "Parameter plane" xvar=a yvar=b center=(1, 0.25) aspect=1/2
pass
|]
      case result of
        Left (_, err) -> expectationFailure err
        Right ens -> do
          [viewer] <- getDynamic (ensembleViewers ens)
          cvCoord viewer `shouldBe` RealCoordinates "a" "b"
          cvCenter viewer `shouldBe` (1, 0.25)
          cvAspect viewer `shouldBe` 0.5

  describe "a real viewer's tools" $ do

    it "bind a click's position, and the pixel height" $ do
      (a, b, h) <- runRealTool (RealCoordinates "x" "y")
        "on click:\n  a <- x\n  b <- y\n  h <- pixel_height\n" (Click (3, 4))
      (a, b, h) `shouldBe` (3, 4, 2)

    it "bind a drag's start and current positions, as real pairs" $ do
      (a, b, h) <- runRealTool (RealCoordinates "x" "y")
        "on drag from (p, q) to (u, v):\n  a <- u - p\n  b <- v - q\n  h <- pixel_width\n"
        (Drag (10, 20) (1, 2))
      (a, b, h) `shouldBe` (9, 18, 0.5)

    it "set the viewer's coordinates, when they're configuration variables" $ do
      (a, b, h) <- runRealTool (RealCoordinates "a" "b")
        "on click-or-drag:\n  h <- pixel_height\n" (Click (3, 4))
      (a, b, h) `shouldBe` (3, 4, 2)

-- | Run a tool's script in a real viewer with coordinate @coord@ and
-- pixels 0.5 wide and 2 high, with real variables @a@, @b@, and @h@
-- it can set, and give back their values after one event.
runRealTool :: Coordinate -> String -> Event -> IO (Double, Double, Double)
runRealTool coord src event = do
  va <- newVariable 0
  vb <- newVariable 0
  vh <- newVariable 0
  let ctx = Bind (Proxy @"a") RealType (mutableVar va)
          $ Bind (Proxy @"b") RealType (mutableVar vb)
          $ Bind (Proxy @"h") RealType (mutableVar vh)
          $ EmptyContext
      limits = (defaultIterLimit, defaultMaxRadius, defaultMinRadius)
  script <- either (fail . snd) pure
              (parseToolScript noCommonDefs coord limits (Just "pixel_size") src)
  dispatch <- buildHandlerWith (interpretedToolExec noDrawing) (SomeContext ctx) script
                >>= either fail pure
  mapM_ runHandlerNow (dispatch (0.5, 2) event)
  (,,) <$> getDynamic va <*> getDynamic vb <*> getDynamic vh

noDrawing :: DrawSink
noDrawing = DrawSink
  { dsClear = pure (), dsStroke = const (pure ()), dsFill = const (pure ())
  , dsPoint = const (pure ()), dsLine = \_ _ -> pure ()
  , dsCircle = \_ _ _ _ -> pure (), dsRect = \_ _ _ -> pure ()
  , dsWrite = \_ _ -> pure (), dsGeodesic = \_ _ -> pure () }
