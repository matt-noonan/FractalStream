module Language.Effect.Render
  ( Render(..)
  ) where

import Language.Code
import Language.Effect.Output

data Render (env :: Environment) (ty :: Type) where
  Render :: forall env
        . ( NotPresent "inputX" env
          , NotPresent "inputY" env )
       => Value env 'IntegerT -- Width of the image
       -> Value env 'IntegerT -- Height of the image
       -> Value env 'RealT -- Starting X coordinate
       -> Value env 'RealT -- Starting Y coordinate
       -> Value env 'RealT -- dx
       -> Value env 'RealT -- dy
       -> Code '[ Output '[ '("color", 'ColorT) ] ]
                ( '("inputX", 'RealT) ': '("inputY", 'RealT) ': env)
               'VoidT
       -> Render env 'IntegerT -- returns an identifier for the result

  HaltRender :: forall env
              . Value env 'IntegerT
             -> Render env 'VoidT

  Blit :: forall env
        . Value env 'IntegerT -- identifier for bitmap
       -> Value env 'IntegerT -- upper-left X coordinate
       -> Value env 'IntegerT -- upper-left Y coordinate
       -> Value env 'RealT -- scale factor
       -> Value env 'RealT -- alpha (1.0 = fully opaque, 0.0 = fully transparent)
       -> Render env 'VoidT
