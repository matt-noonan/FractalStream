module Language.Value.Transform
  ( integerPowers
  , avoidSqrt
  ) where

import Data.Indexed.Functor
import Language.Value

import Data.Word (Word64)
import Fcf

pattern ConstIntI :: forall env. KnownEnvironment env
                  => Int64 -> Value '(env, 'IntegerT)
pattern ConstIntI n = Fix (Const (Scalar IntegerType n))

pattern ConstIntF :: forall env. KnownEnvironment env
                  => Int64 -> Value '(env, 'RealT)
pattern ConstIntF n = Fix (I2R (Fix (Const (Scalar IntegerType n))))

pattern ConstIntC :: forall env. KnownEnvironment env
                  => Int64 -> Value '(env, 'ComplexT)
pattern ConstIntC n = Fix (R2C (Fix (I2R (Fix (Const (Scalar IntegerType n))))))

integerPowers :: Value et0 -> Value et0
integerPowers = indexedFold phi
  where
    phi :: forall et. ValueF (Pure1 Value) et -> Value et
    phi = \case

      PowC x (ConstIntC n)
        | n == 0 -> ConstIntC 1
        | n < 0  -> Fix (DivC (ConstIntC 1)
                              (phi (PowC x (ConstIntC (-n)))))
        | otherwise -> mkPow ComplexType (Fix .: MulC) x (fromIntegral n)

      PowF x (ConstIntF n)
        | n == 0 -> ConstIntF 1
        | n < 0  -> Fix (DivF (ConstIntF 1)
                              (phi (PowF x (ConstIntF (-n)))))
        | otherwise -> mkPow RealType (Fix .: MulF) x (fromIntegral n)

      PowI x (ConstIntI n)
        | n == 0 -> ConstIntI 1
        | n < 0  -> Fix (DivI (ConstIntI 1)
                              (phi (PowI x (ConstIntI (-n)))))
        | otherwise -> mkPow IntegerType (Fix .: MulI) x (fromIntegral n)

      -- TODO: |z|^2k -> (x^2 + y^2)^k, |z|^(2k+1) -> |z| * (x^2 + y^2)^k

      etc -> Fix etc

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \x -> f . g x

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
    phi :: forall et. ValueF (Pure1 Value) et -> Value et
    phi = \case

      LTF (Fix (AbsC z)) rhs ->
        Fix (LTF (Fix (AddF (Fix (MulF (Fix (ReC z)) (Fix (ReC z)))) (Fix (MulF (Fix (ImC z)) (Fix (ImC z))))))
                 (Fix (MulF (Fix (AbsF rhs)) rhs)))

      LTF lhs (Fix (AbsC z)) ->
        Fix (LTF (Fix (MulF (Fix (AbsF lhs)) lhs))
                 (Fix (AddF (Fix (MulF (Fix (ReC z)) (Fix (ReC z)))) (Fix (MulF (Fix (ImC z)) (Fix (ImC z)))))))

      LTF (Fix (AbsF x)) rhs ->
        Fix (LTF (Fix (MulF x x))
                 (Fix (MulF (Fix (AbsF rhs)) rhs)))

      LTF lhs (Fix (AbsF x)) ->
        Fix (LTF (Fix (MulF (Fix (AbsF lhs)) lhs))
                 (Fix (MulF x x)))

      LEF (Fix (AbsC z)) rhs ->
        Fix (LEF (Fix (AddF (Fix (MulF (Fix (ReC z)) (Fix (ReC z)))) (Fix (MulF (Fix (ImC z)) (Fix (ImC z))))))
                 (Fix (MulF (Fix (AbsF rhs)) rhs)))

      LEF lhs (Fix (AbsC z)) ->
        Fix (LEF (Fix (MulF (Fix (AbsF lhs)) lhs))
                 (Fix (AddF (Fix (MulF (Fix (ReC z)) (Fix (ReC z)))) (Fix (MulF (Fix (ImC z)) (Fix (ImC z)))))))

      LEF (Fix (AbsF x)) rhs ->
        Fix (LEF (Fix (MulF x x))
                 (Fix (MulF (Fix (AbsF rhs)) rhs)))

      LEF lhs (Fix (AbsF x)) ->
        Fix (LEF (Fix (MulF (Fix (AbsF lhs)) lhs))
                 (Fix (MulF x x)))

      GTF (Fix (AbsC z)) rhs ->
        Fix (GTF (Fix (AddF (Fix (MulF (Fix (ReC z)) (Fix (ReC z)))) (Fix (MulF (Fix (ImC z)) (Fix (ImC z))))))
                 (Fix (MulF (Fix (AbsF rhs)) rhs)))

      GTF lhs (Fix (AbsC z)) ->
        Fix (GTF (Fix (MulF (Fix (AbsF lhs)) lhs))
                 (Fix (AddF (Fix (MulF (Fix (ReC z)) (Fix (ReC z)))) (Fix (MulF (Fix (ImC z)) (Fix (ImC z)))))))

      GTF (Fix (AbsF x)) rhs ->
        Fix (GTF (Fix (MulF x x))
                 (Fix (MulF (Fix (AbsF rhs)) rhs)))

      GTF lhs (Fix (AbsF x)) ->
        Fix (GTF (Fix (MulF (Fix (AbsF lhs)) lhs))
                 (Fix (MulF x x)))

      GEF (Fix (AbsC z)) rhs ->
        Fix (GEF (Fix (AddF (Fix (MulF (Fix (ReC z)) (Fix (ReC z)))) (Fix (MulF (Fix (ImC z)) (Fix (ImC z))))))
                 (Fix (MulF (Fix (AbsF rhs)) rhs)))

      GEF lhs (Fix (AbsC z)) ->
        Fix (GEF (Fix (MulF (Fix (AbsF lhs)) lhs))
                 (Fix (AddF (Fix (MulF (Fix (ReC z)) (Fix (ReC z)))) (Fix (MulF (Fix (ImC z)) (Fix (ImC z)))))))

      GEF (Fix (AbsF x)) rhs ->
        Fix (GEF (Fix (MulF x x))
                 (Fix (MulF (Fix (AbsF rhs)) rhs)))

      GEF lhs (Fix (AbsF x)) ->
        Fix (GEF (Fix (MulF (Fix (AbsF lhs)) lhs))
                 (Fix (MulF x x)))

      etc -> Fix etc
