module Language.Effect.Draw
  ( Draw(..)
  ) where

import Language.Value

data Draw (env :: Environment) (t :: Type) where
  DrawPoint :: forall env
             . Value env 'ColorT
            -> Value env ('Pair 'RealT 'RealT)
            -> Draw env 'VoidT
  DrawCircle :: forall env
              . Value env 'ColorT
             -> Value env 'RealT
             -> Value env ('Pair 'RealT 'RealT)
             -> Draw env 'VoidT
  DrawLine :: forall env
            . Value env 'ColorT
           -> Value env ('Pair 'RealT 'RealT)
           -> Value env ('Pair 'RealT 'RealT)
           -> Draw env 'VoidT
  Clear :: forall env. Draw env 'VoidT
