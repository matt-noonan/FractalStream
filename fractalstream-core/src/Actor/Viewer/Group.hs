{-# LANGUAGE OverloadedStrings #-}
-- | Groups acting on the hyperbolic plane, for a hyperbolic viewer's
-- @group=@ (see HYPERBOLIC_PLAN.md, phase 2).
--
-- A group is given by its /side pairings/: Möbius maps of the script's
-- model (the disk or the half-plane) and a /center/, a point no pairing
-- fixes. Side @k@ is the perpendicular bisector of the center and
-- @T_k⁻¹(center)@, and @T_k@ takes a point beyond that side back across
-- it, nearer the center. The sides bound the /fundamental polygon/ (the
-- Dirichlet polygon about the center, when the pairings generate the
-- group and their set is closed under inverses), and folding a point
-- means applying pairings until it's in the polygon.
--
-- Folding works in /fold coordinates/: the disk, with the center at 0.
-- There the polygon is convex in the Klein model, and contains 0, so the
-- ray from 0 through a point leaves the polygon through exactly one side:
-- the one whose /wedge/ (the angles between its ends, seen from 0)
-- contains the point's angle. A point beyond the polygon is beyond that
-- side, so each step of the fold finds its side by a binary search of
-- the wedges, however many sides there are.
module Actor.Viewer.Group
  ( Group(..)
  , FoldData(..)
  , Side(..)
  , modularGroup
  , groupFromTable
  , prepareGroup
  , foldDisk
  , foldModelPoint
  , reduceView
  , viewReduction
  , tileViews
  , foldLimit
  , noFolding
    -- * A grouped viewer's code
  , groupValueNames
  , groupMarkerName
  , groupEntries
  , groupValues
  , groupFoldCode
  , groupVariables
  ) where

import FractalStream.Prelude

import Actor.Viewer.Disk

import Language.Type
import Language.Table (readCell, tableDims)
import Language.Code.Parser (noCommonDefs, parseParsedCode)
import Language.Code.Procedures (renameStmt)
import Language.Code.Syntax (Stmt(..), StmtF(..))

import qualified Data.Map as Map
import qualified Data.Set as Set


-- | A group's side pairings, in a model, and its center.
data Group = Group
  { groupMaps   :: [Mobius]
  , groupCenter :: (Double, Double)
  }

-- | A side of the fundamental polygon, in fold coordinates.
data Side = Side
  { sideMap    :: Mobius          -- ^ the pairing, taking the outside back in
  , sideNormal :: Complex Double  -- ^ toward the side (a unit vector)
  , sideReach  :: Double          -- ^ the side's distance from 0, in the Klein model
  , sideCircle :: Complex Double  -- ^ the side, as a circle (center ...)
  , sideRadius :: Double          -- ^ (... and radius) in the disk
  }

-- | A group made ready for folding: its sides (one per pairing, in the
-- pairings' order), the polygon's wedges (the angle each starts at, in
-- increasing order, and its side, or 0 for a stretch of the circle at
-- infinity that no side bounds), and the maps from the model to fold
-- coordinates and back.
data FoldData = FoldData
  { fdSides    :: [Side]
  , fdWedges   :: [(Double, Int)]
  , fdToFold   :: Mobius
  , fdFromFold :: Mobius
  , fdRadius   :: Double
    -- ^ how far the polygon reaches from 0 (in the disk, at most 1)
  , fdCorners  :: [Complex Double]
    -- ^ the polygon's corners, in the disk (on the circle, for a corner at
    -- infinity)
  }

-- | The most steps a fold takes (a point that needs more, so near the
-- circle at infinity that its digits have run out, is left where it got).
foldLimit :: Int
foldLimit = 1000

-- | The modular group PSL(2, ℤ), by @z ↦ z + 1@, @z ↦ z - 1@ and
-- @z ↦ -1/z@, about @2i@ in the half-plane: its fundamental polygon is
-- the familiar one, |re z| ≤ 1/2 and |z| ≥ 1. (For a disk script, moved
-- to the disk by the Cayley map.)
modularGroup :: HyperbolicModel -> Group
modularGroup model = Group (map toModel maps) (toModelPt (0, 2))
  where
    maps = [ Mobius 1 1 0 1, Mobius 1 (-1) 0 1, Mobius 0 (-1) 1 0 ]
    -- (from the half-plane to the model)
    h2m = composeMobius (diskToModel model) cayley
    toModel m = composeMobius h2m (composeMobius m (inverseMobius h2m))
    toModelPt (x, y) = let u :+ w = applyMobius h2m (x :+ y) in (u, w)

-- | A group from a script's table (a @Table 2 of ℂ@): column @k@'s first
-- four rows are side @k@'s pairing @s ↦ (a s + b) / (c s + d)@, in the
-- model. (Further rows are the script's own.)
groupFromTable :: (Double, Double) -> Table -> Either String Group
groupFromTable center t = case tableDims t of
  [rows, cols]
    | rows < 4 -> Left "A group's table needs (at least) four rows: a, b, c, d of each side's pairing s ↦ (a s + b)/(c s + d)."
    | cols < 1 -> Left "A group's table has no sides yet."
    | otherwise -> Right (Group [ side k | k <- [1 .. cols] ] center)
  _ -> Left "A group's table should be a `Table 2 of C`."
  where
    cell r k = fromMaybe 0 (readCell ComplexType t [r, k])
    side k = Mobius (cell 1 k) (cell 2 k) (cell 3 k) (cell 4 k)

-- | Get a group ready to fold with, in a model.
prepareGroup :: HyperbolicModel -> Group -> Either String FoldData
prepareGroup model (Group maps center)
  | null maps = Left "A group needs at least one side pairing."
  | not (inModel model center) = Left "A group's center must be in the hyperbolic plane."
  | otherwise = do
      sides <- zipWithM side [1 :: Int ..] maps
      (wedges, corners) <- polygonWedges sides
      -- (the corners, from the Klein model to the disk)
      let toDisk k = let r = min 1 (magnitude k) in k / ((1 + sqrt (max 0 (1 - r * r))) :+ 0)
          diskCorners = map toDisk corners
          radius = maximum (0 : map magnitude diskCorners)
      pure (FoldData sides wedges toFold (inverseMobius toFold) radius diskCorners)
  where
    -- (the model to the disk, then the center to 0)
    toFold = composeMobius (isometryMobius (inverseIsometry (isometryFacing (modelToDisk model center))))
                           (inverseMobius (diskToModel model))
    side k m0 = do
      let det = let Mobius a b c d = m0 in a * d - b * c
      when (magnitude det < 1e-300 || isNaN (magnitude det)) $
        Left ("Side " ++ show k ++ "'s pairing isn't invertible (a d - b c = 0).")
      let m@(Mobius a b c d) = composeMobius toFold (composeMobius (normalizeMobius m0) (inverseMobius toFold))
          -- (a map of the disk to itself is ±(a, b, conj b, conj a))
          scale = max 1 (magnitude a)
          isDisk sg = magnitude (c - sg * conjugate b) < 1e-7 * scale && magnitude (d - sg * conjugate a) < 1e-7 * scale
      unless (isDisk 1 || isDisk (-1)) $
        Left ("Side " ++ show k ++ "'s pairing doesn't take the hyperbolic plane to itself.")
      -- (the point the pairing takes to the center: the side is halfway)
      let q = negate b / a
          r = magnitude q
      when (r < 1e-12) $ Left ("Side " ++ show k ++ "'s pairing fixes the group's center: choose another center.")
      when (r >= 1) $ Left ("Side " ++ show k ++ "'s pairing doesn't take the hyperbolic plane to itself.")
      pure Side { sideMap = m
                , sideNormal = q / (r :+ 0)
                , sideReach = r
                , sideCircle = q / ((r * r) :+ 0)
                , sideRadius = sqrt (1 / (r * r) - 1) }

-- | The polygon's wedges: the polygon (in the Klein model) is the square
-- of side 4 about 0, cut down by each side's half-plane; then each edge
-- runs from one corner to the next, counterclockwise.
polygonWedges :: [Side] -> Either String ([(Double, Int)], [Complex Double])
polygonWedges sides =
  case foldl' clip square (zip [1 ..] sides) of
    [] -> Left "A group's sides leave no fundamental polygon."
    poly ->
      let wedges = [ (atan2 y x, k) | ((x, y), k) <- poly ]
          -- (its corners in the Klein model, or for a stretch of the
          -- circle at infinity, where the square's corners point)
          corners = [ x :+ y | ((x, y), _) <- poly ]
      in Right (sortOn fst wedges, corners)
  where
    square = [ ((-2, -2), 0), ((2, -2), 0), ((2, 2), 0), ((-2, 2), 0) ]
    -- (Sutherland-Hodgman: a vertex, and the label of the edge from it to
    -- the next; the half-plane kept is P · n <= t)
    clip poly (k, Side { sideNormal = nx :+ ny, sideReach = t }) =
      let inside (x, y) = x * nx + y * ny <= t
          cross (x1, y1) (x2, y2) =
            let f1 = x1 * nx + y1 * ny - t
                f2 = x2 * nx + y2 * ny - t
                s = f1 / (f1 - f2)
            in (x1 + s * (x2 - x1), y1 + s * (y2 - y1))
          step ((p, lab), (q, _)) = case (inside p, inside q) of
            (True, True) -> [(p, lab)]
            (True, False) -> [(p, lab), (cross p q, k)]
            (False, True) -> [(cross p q, lab)]
            (False, False) -> []
      in if null poly then [] else concatMap step (zip poly (drop 1 poly ++ take 1 poly))

-- | The wedge whose angles contain a point's.
wedgeOf :: [(Double, Int)] -> Double -> Int
wedgeOf wedges ph = case [ k | (a, k) <- reverse wedges, a <= ph ] of
  k : _ -> k
  [] -> snd (last wedges)

-- | Fold a point of the disk (in fold coordinates) into the polygon: the
-- point there, the map that took it there, and the number of steps.
foldDisk :: FoldData -> Complex Double -> (Complex Double, Mobius, Int)
foldDisk fd = go (Mobius 1 0 0 1) 0
  where
    sides = fdSides fd
    go acc n z
      | n >= foldLimit = (z, acc, n)
      | otherwise =
          let r2 = magnitude z ^ (2 :: Int)
              kz = (2 / (1 + r2)) *: z
              k = wedgeOf (fdWedges fd) (phase kz)
          in case k of
               0 -> (z, acc, n)
               _ -> let Side { sideMap = m, sideNormal = nk, sideReach = t } = sides !! (k - 1)
                    in if realPart kz * realPart nk + imagPart kz * imagPart nk > t
                         then go (composeMobius m acc) (n + 1) (applyMobius m z)
                         else (z, acc, n)
    s *: z = (s :+ 0) * z

-- | A model point, folded into the polygon (in the model).
foldModelPoint :: FoldData -> (Double, Double) -> (Double, Double)
foldModelPoint fd (x, y) =
  let (z, _, _) = foldDisk fd (applyMobius (fdToFold fd) (x :+ y))
      u :+ w = applyMobius (fdFromFold fd) z
  in (u, w)

-- | A view (in the disk: see 'Actor.Viewer.Disk.viewMobius'), moved by the
-- group so that its center (the point at the display's middle) is in the
-- fundamental polygon. It shows the same picture; but its numbers stay
-- small, however far it's been moved, so they keep their precision.
reduceView :: HyperbolicModel -> FoldData -> Isometry -> Isometry
reduceView model fd v = viewReduction model fd v v

-- | The group's element that reduces a view (see 'reduceView'), as it
-- moves views: moving another view (the one before a move, say) by the
-- same element keeps the move between them the same.
viewReduction :: HyperbolicModel -> FoldData -> Isometry -> Isometry -> Isometry
viewReduction model fd v =
  let -- (from the model's disk to fold coordinates)
      k = composeMobius (fdToFold fd) (diskToModel model)
      c = applyMobius (composeMobius k (isometryMobius v)) 0
      (_, w, _) = foldDisk fd c
      g = composeMobius (inverseMobius k) (composeMobius w k)
  in \u -> mobiusIsometry (composeMobius g (isometryMobius u))

-- | A Möbius map of the disk to itself, as an isometry. (It's ±(a, b,
-- conj b, conj a); i times the second is the first's form.)
mobiusIsometry :: Mobius -> Isometry
mobiusIsometry (Mobius a b _ d)
  | magnitude (d - conjugate a) <= magnitude (d + conjugate a) = normalizeIsometry (Isometry a b)
  | otherwise = normalizeIsometry (Isometry (a * (0 :+ 1)) (b * (0 :+ 1)))

-- | The views to draw a grouped viewer's drawings with, so that they're
-- drawn in each tile of the polygon that's on the screen, not just the
-- polygon itself (which comes first): the tiles found by crossing sides
-- from tile to tile, as long as they cover at least @minArea@ of the
-- screen the polygon itself does (so that a drawing's copies aren't
-- crowded into slivers of tiles near the circle at infinity), and are
-- shown no more than @maxScale@ times as large as the polygon is,
-- anywhere (at its center and corners: a marker a tool sized for where it
-- is would be huge in a tile shown much larger), and @visible@ (given a tile's center
-- on the screen plane, and how far it reaches there), up to @maxTiles@ of
-- them. (Tiles too large are still crossed, to reach those beyond.)
tileViews :: HyperbolicModel -> HyperbolicDisplay -> FoldData -> Isometry
          -> ((Double, Double) -> Double -> Bool) -> (Double, Double) -> Int -> [Isometry]
tileViews model display fd v visible (minArea, maxScale) maxTiles = case fdSides fd of
  [] -> [v]
  _ ->
    let -- (how much a tile is magnified on the screen plane, at its center:
        -- |f'(0)| = 1 / |d|² for f = (a z + b) / (c z + d), of determinant 1)
        scaleOf h = stretch (composeMobius toScreen h) 0
        -- (how much larger than the polygon itself a tile is shown, at
        -- most, at its center and corners)
        points = 0 : fdCorners fd
        baseStretch = map (stretch toScreen) points
        largest h = let m = composeMobius toScreen h
                    in maximum (zipWith (\p s0 -> stretch m p / s0) points baseStretch)
        -- (a tile's area on the screen plane, near enough: its corners'
        -- polygon there)
        area h = let cs = [ applyMobius (composeMobius toScreen h) c | c <- fdCorners fd ]
                 in abs (sum (zipWith (\(a :+ b) (c :+ d) -> a * d - b * c) cs (drop 1 cs ++ take 1 cs))) / 2
        baseArea = area identityMobius
        go _ [] acc _ = reverse acc
        go seen (h : queue) acc n
          | n >= maxTiles = reverse acc
          | otherwise =
              let sc@(x, y) = screen (applyMobius h 0)
                  scale = scaleOf h
                  big = largest h
                  reached = n == 0 || (not (isNaN x || isNaN y || isNaN big) && area h >= minArea * baseArea
                                       && visible sc (scale * fdRadius fd))
                  drawn = n == 0 || big <= maxScale
              in if not reached then go seen queue acc n
                 else let next = [ h' | t <- crossings
                                      , let h' = composeMobius h t
                                      , key (applyMobius h' 0) `Set.notMember` seen ]
                          seen' = foldr (Set.insert . key . (`applyMobius` 0)) seen next
                      in go seen' (queue ++ next) (if drawn then viewFor h : acc else acc) (n + 1)
    in go (Set.singleton (key 0)) [identityMobius] [] (0 :: Int)
  where
    -- (from the model's disk to fold coordinates, and from those to the
    -- screen plane)
    k = composeMobius (fdToFold fd) (diskToModel model)
    toScreen = inverseMobius (composeMobius k (composeMobius (isometryMobius v) (displayToDisk display)))
    screen f = let u :+ w = applyMobius toScreen f in (u, w)
    -- (|f'(z)| = 1 / |c z + d|², for f of determinant 1)
    stretch (Mobius _ _ c d) z = 1 / (magnitude (c * z + d) ^ (2 :: Int))
    -- (from a tile to its neighbor across a side)
    crossings = map (inverseMobius . sideMap) (fdSides fd)
    key :: Complex Double -> (Integer, Integer)
    key (x :+ y) = (round (x * 1e7), round (y * 1e7))
    -- (the view that shows the polygon where the tile is)
    viewFor h = mobiusIsometry (composeMobius (inverseMobius k)
                                 (composeMobius (inverseMobius h) (composeMobius k (isometryMobius v))))

-- | Folding with no group (a group's table that's empty, say, or wrong):
-- every point is already in the "polygon".
noFolding :: FoldData
noFolding = FoldData [] [(negate pi, 0)] identityMobius identityMobius 1 []

------------------------------------------------------------------------
-- A grouped viewer's code

-- | The names under which a grouped viewer's code is given its group:
-- the wedges' angles and sides, each side's normal, reach, pairing (a, b,
-- c, d), circle and radius, and the map from fold coordinates to the
-- model (a, b, c, d). The code's own names for them (in 'foldSource').
groupValueNames :: [(String, String)]
groupValueNames =
  [ ("ga", "[internal] group angles"), ("gs", "[internal] group wedge sides")
  , ("gn", "[internal] group normals"), ("gt", "[internal] group reach")
  , ("gma", "[internal] group a"), ("gmb", "[internal] group b")
  , ("gmc", "[internal] group c"), ("gmd", "[internal] group d")
  , ("gc", "[internal] group circles"), ("gr", "[internal] group radii")
  , ("gk", "[internal] group from fold") ]

-- | The name whose presence says a viewer's code has a group.
groupMarkerName :: String
groupMarkerName = "[internal] group angles"

-- | Their types.
groupEntries :: [(String, SomeType)]
groupEntries = zip (map snd groupValueNames)
  [ SomeType (ListType RealType), SomeType (ListType IntegerType)
  , SomeType (ListType ComplexType), SomeType (ListType RealType)
  , SomeType (ListType ComplexType), SomeType (ListType ComplexType)
  , SomeType (ListType ComplexType), SomeType (ListType ComplexType)
  , SomeType (ListType ComplexType), SomeType (ListType RealType)
  , SomeType (ListType ComplexType) ]

-- | Their values, for a group ready to fold with: in the order of
-- 'groupValueNames', as (type, value) pairs.
groupValues :: FoldData -> ([Double], [Int], [Complex Double], [Double], [Complex Double], [Complex Double],
                            [Complex Double], [Complex Double], [Complex Double], [Double], [Complex Double])
groupValues FoldData{..} =
  ( map fst fdWedges, map snd fdWedges
  , map sideNormal fdSides, map sideReach fdSides
  , [ a | Side { sideMap = Mobius a _ _ _ } <- fdSides ]
  , [ b | Side { sideMap = Mobius _ b _ _ } <- fdSides ]
  , [ c | Side { sideMap = Mobius _ _ c _ } <- fdSides ]
  , [ d | Side { sideMap = Mobius _ _ _ d } <- fdSides ]
  , map sideCircle fdSides, map sideRadius fdSides
  , let Mobius a b c d = fdFromFold in [a, b, c, d] )

-- | The names a grouped viewer's code is given, and their types:
-- @side_distance@ (the hyperbolic distance from its point to the nearest
-- side of the polygon), @side_index@ (that side: the column of the group's
-- table), and @fold_count@ (how many sides the point was moved across to
-- get into the polygon).
groupVariables :: [(String, FSType)]
groupVariables = [ ("side_distance", RealT), ("side_index", IntegerT), ("fold_count", IntegerT) ]

-- | A grouped viewer's code, in place of setting its point and pixel size
-- from the screen point's homogeneous coordinates @[num : den]@ (see
-- 'Actor.Viewer.Disk.inTheHyperbolicPlane'): those are in fold
-- coordinates; the point is folded into the polygon, then taken to the
-- model; and the pixel size follows it along.
groupFoldCode :: String -> String -> String -> String -> [Stmt]
groupFoldCode z px num den = case renameStmt renames <$> parseParsedCode noCommonDefs foldSource of
  -- (its statements, so that its declarations scope over the viewer's code)
  Right (Stmt _ (SBlock stmts)) -> stmts
  Right stmt -> [stmt]
  Left err -> error ("internal error: the fold code doesn't parse: " ++ show err)
  where
    locals = [ "gz", "gdz", "gsteps", "gwedge", "gnw", "gaon", "gkz", "gph", "glo", "ghi", "gmid"
             , "gside", "gnk", "gden2", "gbest", "gbk", "gcand", "gcc", "grr", "gq", "gsd", "gfk" ]
    renames = Map.fromList $
      [ ("zout", z), ("pxv", px), ("gnum", num), ("gden", den) ]
      ++ groupValueNames
      ++ [ (n, "[internal] group." ++ n) | n <- locals ]

-- | The fold, in the language (with the names 'groupFoldCode' renames).
foldSource :: String
foldSource = unlines $
  [ "gz := gnum / gden"
  , "pxv <- pxv / |gden|^2"
  , "gdz := 1.0"
  , "gsteps := 0"
  , "gnw := length(ga)"
  , "gwedge := gnw"
  , "gaon := true"
  -- each step: the wedge the point's in (by its angle, in the Klein
  -- model), and if it's beyond that wedge's side, across it
  , "while gaon up to " ++ show foldLimit ++ " times:"
  , "  gkz := 2 gz / (1 + |gz|^2)"
  , "  gph := arg gkz"
  , "  gwedge <- gnw"
  , "  if gph >= ga @ 1:"
  , "    glo := 1"
  , "    ghi := gnw"
  , "    while ghi > glo up to 64 times:"
  , "      gmid := (glo + ghi + 1) // 2"
  , "      if ga @ gmid <= gph:"
  , "        glo <- gmid"
  , "      else:"
  , "        ghi <- gmid - 1"
  , "    gwedge <- glo"
  , "  gside := gs @ gwedge"
  , "  gaon <- false"
  , "  if gside > 0:"
  , "    gnk := gn @ gside"
  , "    if re(gkz) re(gnk) + im(gkz) im(gnk) > gt @ gside:"
  , "      gden2 := (gmc @ gside) gz + gmd @ gside"
  , "      gz <- ((gma @ gside) gz + gmb @ gside) / gden2"
  , "      gdz <- gdz / |gden2|^2"
  , "      gsteps <- gsteps + 1"
  , "      gaon <- true"
  -- the nearest side: its wedge's, or a neighbor's (near a corner)
  , "gbest := -1.0"
  , "gbk := 0"
  , "gcand := 0"
  ] ++ concatMap nearest
  [ "if gwedge = 1 then gnw else gwedge - 1", "gwedge", "if gwedge = gnw then 1 else gwedge + 1" ] ++
  [ "side_distance := if gbest < 0 then 1000000.0 else asinh(gbest)"
  , "side_index := gbk"
  , "fold_count := gsteps"
  , "gfk := (gk @ 3) gz + gk @ 4"
  , "zout <- ((gk @ 1) gz + gk @ 2) / gfk"
  , "pxv <- pxv gdz / |gfk|^2" ]
  where
    nearest w =
      [ "gcand <- gs @ (" ++ w ++ ")"
      , "if gcand > 0:"
      , "  gcc := gc @ gcand"
      , "  grr := gr @ gcand"
      , "  gq := |gz - gcc|^2 - grr^2"
      , "  gsd := |gq| / (grr (1 - |gz|^2))"
      , "  if gbest < 0 or gsd < gbest:"
      , "    gbest <- gsd"
      , "    gbk <- gcand" ]
