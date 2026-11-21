-- | The script language's @//@ and @mod@: Euclidean division. @mod(a, b)@
-- is the @r@ with @a = b k + r@ for a whole number @k@ and @0 <= r < |b|@,
-- and @a // b@ is that @k@. Dividing by 0 gives 0, both ways. Every backend
-- computes these (LANGUAGE.md, "Numbers").
module Language.Value.Euclidean
  ( euclidDiv
  , euclidMod
  , euclidModReal
  ) where

import Prelude
import Numeric.Extras (fmod)

-- | @a // b@: with 'euclidMod', @a = b (a // b) + mod(a, b)@ (wrapping, for
-- the smallest integer divided by -1). @a // 0@ is 0.
euclidDiv :: Integral a => a -> a -> a
euclidDiv a b
  | b == 0 = 0
  | b == -1 = 0 - a  -- (wraps for the smallest integer, where 'quot' throws)
  | r < 0 = if b > 0 then q - 1 else q + 1
  | otherwise = q
  where (q, r) = a `quotRem` b

-- | @mod(a, b)@, in @[0, |b|)@. @mod(a, 0)@ is 0.
euclidMod :: Integral a => a -> a -> a
euclidMod a b
  | b == 0 || b == -1 = 0
  | r < 0 = if b < 0 then r - b else r + b  -- (not |b|, which can overflow)
  | otherwise = r
  where r = a `rem` b

-- | @mod(a, b)@ for real numbers, in @[0, |b|)@, from @fmod@ (whose remainder
-- is exact, with @a@'s sign). A tiny negative remainder plus @|b|@ can round
-- to @|b|@ itself, which is taken as 0 (the same, modulo @b@). @mod(a, 0)@ is
-- 0; a NaN, or an infinite @a@, gives NaN, as @fmod@ does.
euclidModReal :: Double -> Double -> Double
euclidModReal a b
  | b == 0 = 0
  | m < 0 = let m' = m + abs b in if m' == abs b then 0 else m'
  | otherwise = m
  where m = fmod a b
