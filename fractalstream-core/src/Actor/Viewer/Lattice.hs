{-# LANGUAGE OverloadedStrings #-}
-- | A lattice acting on the plane, for a viewer's @lattice=@: the viewer
-- folds each pixel's point into the lattice's fundamental parallelogram
-- before its code sees it, so the code works on the torus (the plane
-- modulo the lattice), and the picture repeats over the plane.
--
-- The lattice is spanned by two periods: for a complex viewer, @lattice=τ@
-- (the lattice ℤ + τℤ) or @lattice="(ω1, ω2)"@; for a real viewer, two
-- vectors, @lattice="((1, 0), (0, 1))"@. They're expressions over the
-- configuration, worked out again when it changes. The fundamental
-- parallelogram is centered at 0, or at @latticeCenter=@: the points
-- @c + s a + t b@ with @s@ and @t@ between -1/2 and 1/2.
--
-- Folding is written in the language and put before the viewer's code
-- ('latticeFoldCode'), so the periods are worked out once per render
-- (see "Language.Code.Hoist"); tools' points are folded in Haskell
-- ('foldLatticePoint').
module Actor.Viewer.Lattice
  ( Lattice(..)
  , LatticeSpec(..)
  , latticeSpec
  , latticeFoldCode
  , foldLatticePoint
  , latticeShifts
  , tiledValueName
  , fundamentalDomainShade
  , latticeMoved
  ) where

import FractalStream.Prelude

import Actor.Event (Coordinate(..))
import Language.Type
import Language.Code.Parser (noCommonDefs, parseParsedCode)
import Language.Code.Procedures (renameStmt)
import Language.Code.Syntax (Stmt(..), StmtF(..))
import Language.Parser.SourceRange (SourceRange(..))
import Language.Value.Parser (parseParsedValue)
import Language.Value.Syntax (Syn(..), ExprF(..))
import Language.Value.Typecheck (ParsedValue)

import qualified Data.Map as Map

-- | A lattice of the plane, as numbers: its periods @a@ and @b@, and the
-- center of its fundamental parallelogram (each a complex number; a real
-- viewer's vector @(x, y)@ is @x + y i@ here).
data Lattice = Lattice
  { latA      :: Complex Double
  , latB      :: Complex Double
  , latCenter :: Complex Double
  } deriving (Eq, Show)

-- | A viewer's @lattice=@ and @latticeCenter=@, as expressions: its two
-- periods and its center, each of the viewer's kind of point (a complex
-- number, or a pair of reals).
data LatticeSpec = LatticeSpec
  { lsA      :: ParsedValue
  , lsB      :: ParsedValue
  , lsCenter :: ParsedValue
  , lsType   :: FSType
    -- ^ the type of each: 'ComplexT', or a pair of reals
  }

-- | A viewer's lattice, from its @lattice=@ (and @latticeCenter=@, if it
-- has one): for a complex viewer, @τ@ (meaning the lattice ℤ + τℤ) or a
-- pair of periods; for a real viewer, a pair of vectors.
latticeSpec :: Coordinate -> ParsedValue -> Maybe ParsedValue -> Either String LatticeSpec
latticeSpec coord periods mcenter = case coord of
  ComplexCoordinate _ -> case periods of
    Syn _ (EPair a b) -> Right (LatticeSpec a b (center "0") ComplexT)
    tau -> Right (LatticeSpec (literal "1") tau (center "0") ComplexT)
  RealCoordinates _ _ -> case periods of
    Syn _ (EPair a b) -> Right (LatticeSpec a b (center "(0, 0)") (Pair RealT RealT))
    _ -> Left "A real viewer's `lattice=` is a pair of vectors, like `lattice=\"((1, 0), (0, 1))\"`."
  where
    center dflt = fromMaybe (literal dflt) mcenter
    literal s = either (error ("internal error: " ++ s ++ " doesn't parse")) id (parseParsedValue s)

-- | The statements that fold a viewer's point into its lattice's
-- parallelogram, to go before its code: the periods and the center, then
-- the fold of the point, which also declares @cell@ (the pair of integers
-- @(m, n)@ of the translation @m a + n b@ the point was moved back by) and
-- @side_distance@ (the distance from the folded point to the nearest side
-- of the parallelogram).
latticeFoldCode :: Coordinate -> LatticeSpec -> [Stmt]
latticeFoldCode coord LatticeSpec{..} =
  [ decl latticeA lsA, decl latticeB lsB, decl latticeC lsCenter ] ++ case parseParsedCode noCommonDefs source of
    Right (Stmt _ (SBlock stmts)) -> map (renameStmt renames) stmts
    Right stmt -> [renameStmt renames stmt]
    Left err -> error ("internal error: the lattice fold doesn't parse: " ++ show err)
  where
    decl n v = Stmt NoSourceRange (SDecl n (Just lsType) v)
    (source, coordRenames) = case coord of
      ComplexCoordinate z -> (complexSource, [("zout", z)])
      RealCoordinates x y -> (realSource, [("xout", x), ("yout", y)])
    renames = Map.fromList $
      [ ("la", latticeA), ("lb", latticeB), ("lc", latticeC) ] ++ coordRenames
      ++ [ (n, "[internal] lattice." ++ n) | n <- locals ]
    locals = [ "ldet", "lw", "ls", "lt", "lm", "ln", "lda", "ldb", "lmoved"
             , "lax", "lay", "lbx", "lby", "lcx", "lcy", "lwx", "lwy" ]

-- | Whether a viewer with a lattice or a group shows all of its tiles
-- (@tiled=@, and the viewer's menu), as its code is given it.
tiledValueName :: String
tiledValueName = "[internal] tiled"

-- | Whether a lattice viewer's point was moved into the parallelogram
-- (it wasn't in it already), by the names 'latticeFoldCode' gives.
latticeMoved :: String
latticeMoved = "[internal] lattice.lmoved"

-- | The statement, to go after a folding viewer's code (and its
-- colorizer), that greys out a pixel outside the fundamental domain
-- while the viewer shows only that domain: @moved@ is the name of the
-- viewer's (internal) truth value saying the pixel's point was moved.
fundamentalDomainShade :: String -> Stmt
fundamentalDomainShade moved =
  case parseParsedCode noCommonDefs "if lmoved and not ltiled:\n  color <- blend(0.7, color, rgb(0.5, 0.5, 0.5))\n" of
    Right stmt -> renameStmt (Map.fromList [("lmoved", moved), ("ltiled", tiledValueName)]) stmt
    Left err -> error ("internal error: the fundamental domain's shading doesn't parse: " ++ show err)

latticeA, latticeB, latticeC :: String
latticeA = "[internal] lattice a"
latticeB = "[internal] lattice b"
latticeC = "[internal] lattice center"

-- (the coordinates s, t of the point in the basis a, b, about the center;
-- the nearest translation m a + n b; and how far the folded point is from
-- the sides s = ±1/2 and t = ±1/2, which are |det| / |b| and |det| / |a|
-- apart)
complexSource :: String
complexSource = unlines
  [ "ldet := re(la) * im(lb) - im(la) * re(lb)"
  , "lw := zout - lc"
  , "ls := (re(lw) * im(lb) - im(lw) * re(lb)) / ldet"
  , "lt := (re(la) * im(lw) - im(la) * re(lw)) / ldet"
  , "lm := round(ls)"
  , "ln := round(lt)"
  , "zout <- zout - lm * la - ln * lb"
  , "cell := (lm, ln)"
  , "lmoved := lm != 0 or ln != 0"
  , "lda := (0.5 - |ls - lm|) * |ldet| / |lb|"
  , "ldb := (0.5 - |lt - ln|) * |ldet| / |la|"
  , "side_distance := if lda < ldb then lda else ldb" ]

realSource :: String
realSource = unlines
  [ "(lax, lay) := la"
  , "(lbx, lby) := lb"
  , "(lcx, lcy) := lc"
  , "ldet := lax * lby - lay * lbx"
  , "lwx := xout - lcx"
  , "lwy := yout - lcy"
  , "ls := (lwx * lby - lwy * lbx) / ldet"
  , "lt := (lax * lwy - lay * lwx) / ldet"
  , "lm := round(ls)"
  , "ln := round(lt)"
  , "xout <- xout - lm * lax - ln * lbx"
  , "yout <- yout - lm * lay - ln * lby"
  , "cell := (lm, ln)"
  , "lmoved := lm != 0 or ln != 0"
  , "lda := (0.5 - |ls - lm|) * |ldet| / sqrt(lbx^2 + lby^2)"
  , "ldb := (0.5 - |lt - ln|) * |ldet| / sqrt(lax^2 + lay^2)"
  , "side_distance := if lda < ldb then lda else ldb" ]

-- | A point's coordinates in the lattice's basis, about its center.
basisCoordinates :: Lattice -> Complex Double -> (Double, Double)
basisCoordinates Lattice{..} p =
  let w = p - latCenter
      det = realPart latA * imagPart latB - imagPart latA * realPart latB
  in ( (realPart w * imagPart latB - imagPart w * realPart latB) / det
     , (realPart latA * imagPart w - imagPart latA * realPart w) / det )

-- | A point of the plane folded into the lattice's parallelogram, as the
-- viewer's code folds its pixels' points (for its tools' points).
foldLatticePoint :: Lattice -> (Double, Double) -> (Double, Double)
foldLatticePoint lat@Lattice{..} (x, y)
  | not (usable lat) = (x, y)
  | otherwise =
      let p = x :+ y
          (s, t) = basisCoordinates lat p
          m = fromIntegral (round s :: Integer)
          n = fromIntegral (round t :: Integer)
          q = p - (m :+ 0) * latA - (n :+ 0) * latB
      in (realPart q, imagPart q)

-- | The translations of the lattice whose tiles reach the screen (the
-- rectangle about @(cx, cy)@ with half-width and half-height @hw@ and
-- @hh@), the identity first: where a tool's drawings are copied to (see
-- 'Actor.Viewer.overlayProjections'). Only the identity, when there would
-- be more than @limit@ of them (tiles too small to draw in).
latticeShifts :: Lattice -> (Double, Double) -> (Double, Double) -> Int -> [(Double, Double)]
latticeShifts lat@Lattice{..} (cx, cy) (hw, hh) limit
  | not (usable lat) = [(0, 0)]
  | otherwise =
      let corners = [ basisCoordinates lat ((cx + sx * hw) :+ (cy + sy * hh)) | sx <- [-1, 1], sy <- [-1, 1] ]
          range f = [ floor (minimum (map f corners)) - 1 .. ceiling (maximum (map f corners)) + 1 ] :: [Integer]
          ms = range fst
          ns = range snd
          shift m n = let d = (fromIntegral m :+ 0) * latA + (fromIntegral n :+ 0) * latB in (realPart d, imagPart d)
      in if length ms * length ns > limit then [(0, 0)]
         else (0, 0) : [ shift m n | m <- ms, n <- ns, (m, n) /= (0, 0) ]

-- (periods that span the plane, and are numbers)
usable :: Lattice -> Bool
usable Lattice{..} =
  let det = realPart latA * imagPart latB - imagPart latA * realPart latB
  in det /= 0 && not (isNaN det) && not (isInfinite det)
