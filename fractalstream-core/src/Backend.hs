module Backend
  (
  ) where
{-
import Language.Value
import Language.Code
import Language.Effect

data Backend where
  Backend :: forall allowedEffects
           . effs `AreAllElementsOf` allowedEffects
          =>
  { allowedEffects :: Proxy allowedEffects
  , compile :: forall env effs
             . effs `IsSubsetOf` allowedEffects
            => Code effs env -> IO (ToFunction env (IO ()))
  }

type family IsSubsetOf (xs :: [k]) (ys :: [k]) :: Constraint where
  IsSubsetOf '[] xs = ()
  IsSubsetOf ( x ': xs) ys = IfIsMemberOf x ys (IsSubsetOf xs ys)

type family IfIsMemberOf (x :: k) (ys :: [k]) (c :: Constraint) :: Constraint where
  IfIsMemberOf x (x ': ys) thn = thn
  IfIsMemberOf x (y ': ys) thn = IfIsMemberOf x ys thn
--IfIsMemberOf x '[] thn = TypeError ...

type family ToFunction (env :: [(Symbol, Type)]) (output :: *) :: * where
  ToFunction '[] output = output
  ToFunction ( '(name, ty) ': env) output =
    ScalarType ty -> ToFunction env output
-}
