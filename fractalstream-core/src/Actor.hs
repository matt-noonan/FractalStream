{-# language UndecidableInstances #-}
module Actor
  ( -- Actor(..)
  -- , SomeActor(..)
  type ToFun(..)
  ) where

--import Event
import Language.Code
import Language.Value.Evaluator (HaskellTypeOfBinding)
import GHC.TypeLits

{-
data SomeActor where
  SomeActor :: forall actor effs. Actor actor effs => actor -> Proxy effs -> SomeActor

class Actor actor effs where

  handle :: actor -> Proxy effs -> Event args -> Maybe (Code effs args 'VoidT)
  handle _ _ _ = Nothing
-}

class ToFun (env :: Environment) (result :: *) where
  type ToFunction env result :: *
  toFunction :: (Context HaskellTypeOfBinding env -> result)
             -> ToFunction env result

instance ToFun '[] result where
  type ToFunction '[] result = result
  toFunction k = k EmptyContext

instance (KnownSymbol name, KnownType ty, NotPresent name env, ToFun env result)
    => ToFun ( '(name, ty) ': env ) result where
  type ToFunction ( '(name, ty) ': env ) result
    = HaskellType ty -> ToFunction env result
  toFunction k = \x ->
    toFunction @env @result (k . Bind (Proxy @name) (typeProxy @ty) x)
