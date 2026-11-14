-- | Groups acting on the hyperbolic plane (a hyperbolic viewer's
-- @group=@): folding into the fundamental polygon, reducing views, and the
-- tiles a drawing is repeated in.
module Actor.GroupSpec (spec) where

import Test.Hspec

import Actor.Script
import Actor.Ensemble (checkScript)
import Actor.Viewer.Disk
import Actor.Viewer.Group

import Data.Complex
import Data.List (isInfixOf)

close :: Double -> Double -> Bool
close a b = abs (a - b) < 1e-9

closeC :: Complex Double -> Complex Double -> Bool
closeC z w = magnitude (z - w) < 1e-8

-- | The genus-2 surface's group: the regular octagon with corner angles
-- pi/4, opposite sides glued by translations.
octagon :: Group
octagon = Group [ Mobius a b (conjugate b) (conjugate a) | k <- [0 .. 7 :: Int]
                , let t = 2 * pi * fromIntegral k / 8
                      a = cosh d :+ 0
                      b = negate (sinh d :+ 0) * cis t ]
                (0, 0)
  where d = acosh (cos (pi / 8) / sin (pi / 8))

ready :: HyperbolicModel -> Group -> FoldData
ready model g = either error id (prepareGroup model g)

-- | Whether a point (in fold coordinates) is in the polygon: not beyond
-- any side (in the Klein model).
inPolygon :: FoldData -> Complex Double -> Bool
inPolygon fd z =
  let k = (2 / (1 + magnitude z ^ (2 :: Int)) :+ 0) * z
  in and [ realPart k * realPart n + imagPart k * imagPart n <= t + 1e-9
         | Side { sideNormal = n, sideReach = t } <- fdSides fd ]

disk :: [Complex Double]
disk = [ (r :+ 0) * cis t | r <- [0, 0.3, 0.7, 0.95, 0.995], t <- [0, 0.7 .. 6] ]

spec :: Spec
spec = do
 describe "folding" $ do

  it "folds the half-plane into the modular group's familiar domain" $ do
    let fd = ready HalfPlaneModel (modularGroup HalfPlaneModel)
    sequence_ [ let (x, y) = foldModelPoint fd (x0, y0)
                in (abs x <= 0.5 + 1e-9 && x * x + y * y >= 1 - 1e-9) `shouldBe` True
              | x0 <- [-3, -1.2, -0.3, 0, 0.45, 2.7], y0 <- [0.01, 0.2, 0.9, 1.5, 7] ]
    -- (and a point already there stays)
    foldModelPoint fd (0.2, 1.5) `shouldSatisfy` (\(x, y) -> close x 0.2 && close y 1.5)

  it "folds the disk into the octagon, by the map it reports" $ do
    let fd = ready DiskModel octagon
    sequence_ [ do let (w, m, _) = foldDisk fd z
                   w `shouldSatisfy` inPolygon fd
                   applyMobius m z `shouldSatisfy` closeC w
              | z <- disk ]

  it "takes no steps inside the polygon, and one just beyond a side" $ do
    let fd = ready DiskModel octagon
        (_, _, n0) = foldDisk fd 0.1
        Side { sideNormal = n, sideReach = t } = case fdSides fd of { s : _ -> s; [] -> error "no sides" }
        -- (just beyond side 1, in the Klein model, back in the disk)
        k = ((t + 0.01) :+ 0) * n
        z = k / ((1 + sqrt (1 - magnitude k ^ (2 :: Int))) :+ 0)
        (_, _, n1) = foldDisk fd z
    n0 `shouldBe` 0
    n1 `shouldBe` 1

  it "rejects pairings that fix the center, or don't keep the plane" $ do
    either id (const "") (prepareGroup DiskModel (Group [Mobius (cis 1) 0 0 (cis (-1))] (0, 0)))
      `shouldSatisfy` isInfixOf "fixes the group's center"
    either id (const "") (prepareGroup DiskModel (Group [Mobius 2 0 0 0.5] (0, 0)))
      `shouldSatisfy` isInfixOf "take the hyperbolic plane to itself"

 describe "views" $ do

  let fd = ready DiskModel octagon
      -- (a view moved several tiles away, by translations)
      far = foldr (\t v -> composeIsometry (isometryFacing (0.8 * cos t, 0.8 * sin t)) v) identityIsometry
                  [0.3, 1.9, 0.3, 2.5]
      screens = [ (0, 0), (0.4, -0.2), (-0.7, 0.5) ]
      folded v s = foldModelPoint fd (maybe (0, 0) id (screenToModelH DiskModel DiskDisplay v s))

  it "reduces a view without changing the picture" $
    sequence_ [ folded (reduceView DiskModel fd far) s `shouldSatisfy`
                  (\(x, y) -> let (x', y') = folded far s in abs (x - x') < 1e-7 && abs (y - y') < 1e-7)
              | s <- screens ]

  it "reduces a view so that its center is in the polygon" $ do
    let Isometry _ b = reduceView DiskModel fd far
        Isometry a0 b0 = reduceView DiskModel fd far
        (_, _, steps) = foldDisk fd (b0 / conjugate a0)
    magnitude b `shouldSatisfy` (< 10)
    steps `shouldBe` 0

  it "keeps a view's numbers small however far it's dragged" $ do
    let step v = reduceView DiskModel fd (dragIsometry v (0.6, 0) (0, 0))
        Isometry a _ = iterate step identityIsometry !! 200
    magnitude a `shouldSatisfy` (< 10)

  it "finds the tiles to repeat drawings in: the octagon and its neighbors" $ do
    let visible (x, y) r = abs x <= 1 + r && abs y <= 1 + r
        tiles = tileViews DiskModel DiskDisplay fd identityIsometry visible (0.03, 2) 500
    take 1 tiles `shouldBe` [identityIsometry]
    length tiles `shouldSatisfy` (>= 9)

 describe "a viewer's header" $ do

  it "takes `group=` only in the hyperbolic plane" $ do
    case parseScript "@viewer \"V\" var=z group=modular\npass\n" of
      Left (_, err) -> err `shouldSatisfy` isInfixOf "Only a viewer of the hyperbolic plane"
      Right _ -> expectationFailure "expected an error"
    case parseScript "@viewer \"V\" var=z view=disk groupCenter=0.1\npass\n" of
      Left (_, err) -> err `shouldSatisfy` isInfixOf "needs a `group` too"
      Right _ -> expectationFailure "expected an error"

  it "gives a grouped viewer's code side_distance, side_index and fold_count" $
    checkScript "@viewer \"V\" view=halfplane group=modular\n\
                \color <- if side_distance < 0.1 and side_index > 0 and fold_count = 0 then red else blue\n"
      `shouldBe` Right ()

  it "checks that a group's table is a `Table 2 of C`" $ do
    checkScript "@config \"G\"\nT : Table 2 of C\n\n@viewer \"V\" view=disk group=T\ncolor <- red\n"
      `shouldBe` Right ()
    checkScript "@config \"G\"\nT : Table 1 of C\n\n@viewer \"V\" view=disk group=T\ncolor <- red\n"
      `shouldSatisfy` either (isInfixOf "must be a `Table 2 of C`" . snd) (const False)
    checkScript "@viewer \"V\" view=disk group=nothing\ncolor <- red\n"
      `shouldSatisfy` either (isInfixOf "is `modular`, or the name of a `Table 2 of C`" . snd) (const False)
