module Language.Value.Transform
  ( backendRewrites
  , integerPowers
  , avoidSqrt
  ) where

import Data.Indexed.Functor
import Language.Value

import Data.Word (Word64)

pattern ConstIntI :: forall env. KnownEnvironment env
                  => Int64 -> Value '(env, 'IntegerT)
pattern ConstIntI n = Const (Scalar IntegerType n)

pattern ConstIntF :: forall env. KnownEnvironment env
                  => Int64 -> Value '(env, 'RealT)
pattern ConstIntF n = I2R (Const (Scalar IntegerType n))

pattern ConstIntC :: forall env. KnownEnvironment env
                  => Int64 -> Value '(env, 'ComplexT)
pattern ConstIntC n = R2C (I2R (Const (Scalar IntegerType n)))

-- | The rewrites every backend applies to a viewer's or tool's code before
-- running it. They don't change what the code means, but they do change
-- its arithmetic (e.g. @z^2@ becomes @z * z@, not a general complex power),
-- so the compiled and interpreted backends must apply the same ones to
-- get identical results.
backendRewrites :: Value et -> Value et
backendRewrites = integerPowers . avoidSqrt

integerPowers :: Value et0 -> Value et0
integerPowers = indexedFold @(FIX ValueF) phi
  where
    phi :: forall et. Value et -> Value et
    phi = \case

      PowC x (ConstIntC n)
        | n == 0 -> ConstIntC 1
        | n < 0  -> DivC (ConstIntC 1)
                         (phi (PowC x (ConstIntC (-n))))
        | otherwise -> mkPow ComplexType MulC x (fromIntegral n)

      PowF x (ConstIntF n)
        | n == 0 -> ConstIntF 1
        | n < 0  -> DivF (ConstIntF 1)
                         (phi (PowF x (ConstIntF (-n))))
        | otherwise -> mkPow RealType MulF x (fromIntegral n)

      PowI x (ConstIntI n)
        | n == 0 -> ConstIntI 1
        | n < 0  -> DivI (ConstIntI 1)
                         (phi (PowI x (ConstIntI (-n))))
        | otherwise -> mkPow IntegerType MulI x (fromIntegral n)

      -- TODO: |z|^2k -> (x^2 + y^2)^k, |z|^(2k+1) -> |z| * (x^2 + y^2)^k

      etc -> etc

mkPow :: KnownEnvironment env
      => TypeProxy ty
      -> (Value '(env, ty) -> Value '(env, ty) -> Value '(env, ty))
      -> Value '(env, ty)
      -> Word64
      -> Value '(env, ty)
mkPow _ty mul x0 n0 = go Nothing (bits n0) x0
  where
    bits :: Word64 -> [Bool]
    bits = \case
      0 -> []
      n -> let (n', k') = n `divMod` 2
           in (k' == 1) : bits n'

    updateAcc Nothing v = Just v
    updateAcc (Just u) v = Just (mul u v)

    go acc [] _ = case acc of
      Nothing -> error "internal error, mkPow called with exponent 0"
      Just a  -> a

    go acc (b:bs) x =
      let x' = mul x x -- TODO: put a let binding here to share the two branches
          acc' = if b then updateAcc acc x else acc
      in go acc' bs x'

avoidSqrt :: Value et0 -> Value et0
avoidSqrt = indexedFold phi
  where
    phi :: forall et. Value et -> Value et
    phi = \case

      LTF (AbsC z) rhs ->
        LTF (AddF (MulF (ReC z) (ReC z)) (MulF (ImC z) (ImC z)))
            (MulF (AbsF rhs) rhs)

      LTF lhs (AbsC z) ->
        LTF (MulF (AbsF lhs) lhs)
            (AddF (MulF (ReC z) (ReC z)) (MulF (ImC z) (ImC z)))

      LTF (AbsF x) rhs ->
        LTF (MulF x x)
            (MulF (AbsF rhs) rhs)

      LTF lhs (AbsF x) ->
        LTF (MulF (AbsF lhs) lhs)
            (MulF x x)

      etc -> etc
