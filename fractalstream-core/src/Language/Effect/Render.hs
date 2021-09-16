{-# language OverloadedStrings #-}
module Language.Effect.Render
  ( Render(..)
  , renderEffectParser
  ) where

import Language.Code
import Language.Parser
import Language.Value.Parser
import Language.Code.Parser
import Data.Indexed.Functor
import Fcf (Exp)
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits

data Render (code :: (Environment, Type) -> Exp *) (et :: (Environment, Type)) where
  Render :: forall env code inputX inputY
        . ( KnownSymbol inputX, KnownSymbol inputY )
       => EnvironmentProxy env
       -> Proxy inputX
       -> Proxy inputY
       -> NameIsAbsent inputX ( '(inputY, 'RealT) ': env)
       -> NameIsAbsent inputY env
       -> Value env ('Pair 'IntegerT 'IntegerT) -- Width and height of the image
       -> Value env ('Pair 'RealT 'RealT) -- Upper-left X, Y coordinates
       -> Value env ('Pair 'RealT 'RealT) -- dx, dy
       -> Code '[] ( '(inputX, 'RealT) ': '(inputY, 'RealT) ': env) 'ColorT
          -- ^ NOTE: this does *not* use the recursive 'code' type! The
          -- rendering effect specifically requires effect-free code. Any
          -- handler for a Render effect will have to be able to handle
          -- this embedded blob of code.
       -> Render code '(env, 'ImageT) -- returns an identifier for the result

  HaltRender :: forall env code
              . EnvironmentProxy env
             -> Value env 'ImageT
             -> Render code '(env, 'VoidT)

  Blit :: forall env code
        . EnvironmentProxy env
       -> Value env 'ImageT -- identifier for bitmap
       -> Value env ('Pair 'IntegerT 'IntegerT) -- upper-left X,Y coordinates
       -> Value env 'RealT -- scale factor
       -> Value env 'RealT -- alpha (1.0 = fully opaque, 0.0 = fully transparent)
       -> Render code '(env, 'VoidT)

  ClearTo :: forall code env
           . EnvironmentProxy env
          -> Value env 'ColorT
          -> Render code '(env, 'VoidT)

instance IFunctor Render where
  type IndexProxy Render = EnvTypeProxy
  imap _ = \case
    Render env nX nY pfX pfY wh ul delta c -> Render env nX nY pfX pfY wh ul delta c
    HaltRender env v -> HaltRender env v
    Blit env iD ul scale alpha -> Blit env iD ul scale alpha
    ClearTo env v -> ClearTo env v
  toIndex = \case
    Render env _ _ _ _ _ _ _ _ -> envTypeProxy env ImageProxy
    HaltRender env _   -> envTypeProxy env VoidProxy
    Blit env _ _ _ _   -> envTypeProxy env VoidProxy
    ClearTo env _      -> envTypeProxy env VoidProxy

instance ITraversable Render where
  isequence = \case
    Render env nX nY pfX pfY wh ul delta c -> pure (Render env nX nY pfX pfY wh ul delta c)
    HaltRender env v -> pure (HaltRender env v)
    Blit env iD ul scale alpha -> pure (Blit env iD ul scale alpha)
    ClearTo env v -> pure (ClearTo env v)

renderEffectParser :: EffectParser Render
renderEffectParser = EffectParser Proxy $
  \(et :: EnvTypeProxy et) _code -> case lemmaEnvTy @et of
    Refl -> withEnvType et $ \env t ->  withEnvironment env $ case t of
      ImageProxy -> do
        tok_ "render" >> tok_ "in"
        Identifier inputXstr <- satisfy (\case { Identifier _ -> True; _ -> False })
        Identifier inputYstr <- satisfy (\case { Identifier _ -> True; _ -> False })
        tok_ "plane"
        dim <- value_
        corner <- value_
        pixel <- value_
        eol >> lookAhead (tok_ Indent)
        case (someSymbolVal inputXstr, someSymbolVal inputYstr) of
          (SomeSymbol inputX, SomeSymbol inputY) ->
            case lookupEnv' inputY env of
              Found' {} -> fail ("a variable named " <> inputYstr <> " is already in scope")
              Absent' pfY -> do
                let env' = bindNameEnv inputY RealProxy pfY env
                case lookupEnv' inputX env' of
                  Found' {} -> fail ("a variable named " <> inputXstr <> " is already in scope")
                  Absent' pfX -> do
                    let env'' = bindNameEnv inputX RealProxy pfX env'
                    Render env inputX inputY pfX pfY dim corner pixel
                      <$> pCode (EP NoEffs) env'' ColorProxy
      VoidProxy -> pHalt env <|> pBlit env <|> pClear env
      _ -> mzero

 where
  pHalt env = withEnvironment env $do
    tok_ "stop" >> tok_ "work" >> tok_ "on"
    HaltRender env <$> value_

  pClear env = do
    tok_ "clear" >> tok_ "to"
    ClearTo env <$> value_

  pBlit env = do
    tok_ "blit"
    handle <- value_
    tok_ "at"
    pt <- value_
    tok_ "with" >> tok_ "scale" >> tok_ Equal
    scale <- value_
    tok_ "opacity" >> tok_ Equal
    opacity <- value_
    pure (Blit env handle pt scale opacity)

{-
initialize x0 to centerX
set oldBitmap to bitmap
set bitmap <- render in x y plane with dim=(width, height) corner=(x0, y0) pixel=(pixelSize, pixelSize)
  init c to x + y i
  init z to 0
  init i to 0
  loop
    set z to z^2 + c
    set i to i + 1
    |z| < maxRadius and i < maxIters
  if i >= maxIters
  then
    black
  else
    if im z > 0
    then red
    else yellow

lookup |z - proj_1 item| < 0.001 in autocolors
  if found then
    set c to proj_2 item
  else
    set c <- random
    insert (z, c) into autocolors

on refresh
  if animating then
    blit bitmap at _ with scale=s, opacity=t
  else
    blit bitmap at (0,0) with scale=100% opacity=100%

-}
