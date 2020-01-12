module Exec.Accelerate (
  computeMandel
  ) where

import           Color.Color
import           Color.Colorize
import           Exec.Region
import           Lang.Numbers                       (C (..))

import           Data.Array.Accelerate              (Exp, Int32, Vector,
                                                     constant, lift,
                                                     lift1)
import qualified Data.Array.Accelerate              as A
--import Data.Array.Accelerate.Interpreter (run1)
import           Data.Array.Accelerate.LLVM.Native  (run1)

import           Data.Array.Accelerate.Data.Complex

type C_acc = Complex Double

doIt :: Vector (Complex Double) -> Vector (Complex Double, Int32)
doIt = run1 (A.map iter)

computeMandel :: Colorizer C -> [C] -> IO [Color]
computeMandel col pts = do
  let zs = map (\(C x y) -> x :+ y) pts :: [Complex Double]
  let !vec_zs = A.fromList (A.Z A.:. length pts) zs :: Vector (Complex Double)
  let colorize = runColorizer col
  return $ map (colorize . classify) $ A.toList $ doIt vec_zs

classify :: (C_acc, Int32) -> Result C
classify (x :+ y, k) = Result region (C x y) (fromIntegral k)
  where region = if k == 100 then Interior else Exterior

iter :: Exp C_acc -> Exp (C_acc, Int32)
iter c = A.while (lift1 unsure) (lift1 $ step c) $ lift (c, constant 0)

norm2 :: Exp C_acc -> Exp Double
norm2 z = (x*x) + (y*y)
  where (x,y) = (real z, imag z)

unsure :: (Exp C_acc, Exp Int32) -> Exp Bool
unsure (z,k) = (norm2 z A.< 100) A.&& (k A.< 100)

step :: Exp C_acc
     -> (Exp C_acc, Exp Int32)
     -> (Exp C_acc, Exp Int32)
step c (z,k) = (c + z * z, k + 1)

