module Language.Effect.Output
  ( Output(..)
  ) where

import Language.Value

data Output (outputs :: Environment) (env :: Environment) (t :: Type) where
  Output :: forall name ty outputs env
          . Required name outputs ~ ty
         => Proxy name
         -> Value env ty
         -> Output outputs env 'VoidT
