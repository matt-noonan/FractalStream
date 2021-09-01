{-# language AllowAmbiguousTypes, UndecidableInstances #-}

module Lang.Component where
{-
import Data.Int
import GHC.TypeLits
import Data.Proxy

import Language.Type
import Language.Value
import Language.Effect.Output
import Language.Code

data Component (inType :: Type) (outType :: Type) where
  IndexX :: Component 'ViewportT 'IntegerT
  IndexY :: Component 'ViewportT 'IntegerT
  ViewX :: Component 'ViewportT 'RealT
  ViewY :: Component 'ViewportT 'RealT
  Setting' :: forall t. String -> Scalar t -> Component 'SettingsT t
  PairC :: forall src a b
         . Component src a
        -> Component src b
        -> Component src ('Pair a b)
  Proj1 :: forall a b. Component ('Pair a b) a
  Proj2 :: forall a b. Component ('Pair a b) b
  Swap :: forall a b. Component ('Pair a b) ('Pair b a)
  Compose :: forall a b c
           . Component b c
          -> Component a b
          -> Component a c
  Constant :: forall t. Scalar t -> Component 'VoidT t
  IfThen :: forall src tgt
          . Component src tgt
         -> Component src tgt
         -> Component ('Pair 'BooleanT src) tgt
  Select :: forall src tgt
          . Component src tgt
         -> [Component src tgt]
         -> Component ('Pair 'IntegerT src) tgt
  RGB :: Component ('Pair 'RealT ('Pair 'RealT 'RealT)) 'ColorT
  RunCode :: forall inputs outputs
           . Code inputs '[Output outputs] 'VoidT
          -> Component (ToNestedTuple inputs) (ToNestedTuple outputs)


type family Pairing (p :: Type) (q :: Type) :: Type where
  Pairing t 'VoidT = t
  Pairing p q = 'Pair p q

type family ToNestedTuple (env :: [(Symbol, Type)]) :: Type where
  ToNestedTuple '[] = 'VoidT
  ToNestedTuple ('(_, t) ': env) = Pairing t (ToNestedTuple env)

-- Compute the Mandelbrot set, FractalStream-1.0-style.
program :: Code '[ -- Inputs
                   '("initialX",  'RealT)
                 , '("initialY",  'RealT)
                 , '("maxIter",   'IntegerT)
                 , '("maxRadius", 'RealT)
                 , '("minRadius", 'RealT) ]
                 '[ Output '[  -- Outputs
                   '("finalX",    'RealT)
                 , '("finalY",    'RealT)
                 , '("iterCount", 'IntegerT)
                 , '("flag",      'IntegerT) ]]
           'VoidT
program =
  let_ @"Zx" zero $
  let_ @"Zy" zero $
  let_ @"Cx" (get @"initialX") $
  let_ @"Cy" (get @"initialY") $
  let_ @"maxRadius2" (MulF (get @"maxRadius") (get @"maxRadius")) $
  let_ @"i" zeroI $ Block
    [ output @"iterCount" zeroI
    , output @"flag" zeroI
    , DoWhile $
      let_ @"oldZx" (get @"Zx") $
      let_ @"oldZy" (get @"Zy") $
      let_ @"oldZxy" (MulF (get @"Zx") (get @"Zy")) $ Block
        [ set @"Zx" (AddF (SubF (MulF (get @"oldZx") (get @"oldZx"))
                                (MulF (get @"oldZy") (get @"oldZy")))
                          (get @"Cx"))
        , set @"Zx" (AddF (AddF (get @"oldZxy") (get @"oldZxy")) (get @"Cy"))
        , set @"i" (AddI (get @"i") (Const (Integer_ 1)))
        ]
        (Pure ((AddF (MulF (get @"Zx") (get @"Zx")) (MulF (get @"Zy") (get @"Zy"))
                 `LEF` get @"maxRadius2")
                `And`
                (get @"i" `LTI` get @"maxIter")))
    , IfThenElse
        (get @"i" `LTI` get @"maxIter")
        (output @"flag" (Const (Integer_ 1)))
        NoOp
    , output @"iterCount" (get @"i")
    , output @"finalX" (get @"Zx")
    , output @"finalY" (get @"Zy")
    ] NoOp

 where
  zero  = Const (Real_ 0)
  zeroI = Const (Integer_ 0)

output :: forall name outputs env ty
        . Required name outputs ~ ty
       => Value env ty
       -> Code env '[Output outputs] 'VoidT
output = Effect . Output @name @ty @outputs @env Proxy

type C = 'Pair 'RealT 'RealT
type ClassicInput  = 'Pair C ('Pair 'IntegerT ('Pair 'RealT 'RealT))
type ClassicOutput = 'Pair C ('Pair 'IntegerT 'IntegerT)

type ComplexDynamics1D = Component ClassicInput ClassicOutput
type Colorizer = Component ('Pair C 'IntegerT) ('Pair 'RealT ('Pair 'RealT 'RealT))
type RenderTask = Component ('Pair 'ViewportT 'SettingsT) 'ColorT

buildDynamics :: ComplexDynamics1D -> Colorizer -> [Colorizer] -> RenderTask
buildDynamics dynamics defaultColor colorizers =
    Compose (Compose RGB colorize)
            (Compose dynamics prepare)
  where
    maxIter   = Setting' "Max. iterations"  (Integer_ 100)
    maxRadius = Setting' "Escape radius"    (Real_ 10)
    minRadius = Setting' "Closeness radius" (Real_ 0.001)
    colorize :: Component ('Pair C ('Pair 'IntegerT 'IntegerT))
                          ('Pair 'RealT ('Pair 'RealT 'RealT))
    colorize =
      Compose (Select defaultColor colorizers)
              (PairC (Compose Proj2 Proj2) (PairC Proj1 (Compose Proj1 Proj2)))
    prepare :: Component ('Pair 'ViewportT 'SettingsT)
                         ('Pair C ('Pair 'IntegerT ('Pair 'RealT 'RealT)))
    prepare = PairC (Compose (PairC ViewX ViewY) Proj1)
                    (Compose (PairC maxIter (PairC maxRadius minRadius)) Proj2)

data BasicTool where
  BasicTool :: forall result
             . String
            -> ScalarProxy result
            -> Component ('Pair 'ViewportT 'SettingsT) result
            -> BasicTool

data Setting'
  = ComplexSetting String Double Double
  | RealSetting String Double
  | IntegerSetting String Int64
  | BooleanSetting String Bool

extractSettings :: Component src tgt -> [Setting']
extractSettings = \case
  Setting' name v -> case v of
    Boolean_ x   -> [BooleanSetting name x]
    Integer_ x   -> [IntegerSetting name x]
    Real_ x      -> [RealSetting name x]
    Complex_ x y -> [ComplexSetting name x y]
    _ -> []
  PairC   c1 c2  -> extractSettings c1 <> extractSettings c2
  Compose c1 c2  -> extractSettings c1 <> extractSettings c2
  _              -> []
-}
