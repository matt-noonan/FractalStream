{-# language OverloadedStrings #-}
module Language.Effect.Draw
  ( Draw(..)
  , drawEffectParser
  ) where

import Language.Value
import Language.Parser
import Language.Value.Parser
import Language.Effect
import Data.Indexed.Functor

import Data.Functor (($>))
import Fcf (Exp)
import Data.Type.Equality ((:~:)(..))

data Draw (code :: (Environment, Type) -> Exp *) (et :: (Environment, Type)) where

  -- | draw point at POINT
  DrawPoint :: forall env code
             . EnvironmentProxy env
            -> Value env ('Pair 'RealT 'RealT)
            -> Draw code '(env, 'VoidT)

  -- | draw circle at POINT with radius VALUE
  -- | draw filled circle at POINT with radius VALUE
  DrawCircle :: forall env code
              . EnvironmentProxy env
             -> Bool
             -> Value env 'RealT
             -> Value env ('Pair 'RealT 'RealT)
             -> Draw code '(env, 'VoidT)

  -- | draw line from POINT to POINT
  DrawLine :: forall env code
            . EnvironmentProxy env
           -> Value env ('Pair 'RealT 'RealT)
           -> Value env ('Pair 'RealT 'RealT)
           -> Draw code '(env, 'VoidT)

  -- | draw rectangle from POINT to POINT
  -- | draw filled rectangle from POINT to POINT
  DrawRect :: forall env code
            . EnvironmentProxy env
           -> Bool
           -> Value env ('Pair 'RealT 'RealT)
           -> Value env ('Pair 'RealT 'RealT)
           -> Draw code '(env, 'VoidT)

  -- | use COLOR for stroke
  SetStroke :: forall env code
             . EnvironmentProxy env
            -> Value env 'ColorT
            -> Draw code '(env, 'VoidT)

  -- | use COLOR for fill
  SetFill :: forall env code
             . EnvironmentProxy env
            -> Value env 'ColorT
            -> Draw code '(env, 'VoidT)

  -- | erase
  Clear :: forall env code
         . EnvironmentProxy env
        -> Draw code '(env, 'VoidT)

instance IFunctor Draw where
  type IndexProxy Draw = EnvTypeProxy
  imap _ = \case
    DrawPoint e p -> DrawPoint e p
    DrawCircle e f r p -> DrawCircle e f r p
    DrawLine e f t -> DrawLine e f t
    DrawRect e f c1 c2 -> DrawRect e f c1 c2
    SetStroke e c -> SetStroke e c
    SetFill e c -> SetFill e c
    Clear e -> Clear e
  toIndex = \case
    DrawPoint e _ -> envTypeProxy e VoidProxy
    DrawCircle e _ _ _ -> envTypeProxy e VoidProxy
    DrawLine e _ _ -> envTypeProxy e VoidProxy
    DrawRect e _ _ _ -> envTypeProxy e VoidProxy
    SetStroke e _ -> envTypeProxy e VoidProxy
    SetFill e _ -> envTypeProxy e VoidProxy
    Clear e -> envTypeProxy e VoidProxy

instance ITraversable Draw where
  isequence = \case
    DrawPoint e p -> pure (DrawPoint e p)
    DrawCircle e f r p -> pure (DrawCircle e f r p)
    DrawLine e f t -> pure (DrawLine e f t)
    DrawRect e f c1 c2 -> pure (DrawRect e f c1 c2)
    SetStroke e c -> pure (SetStroke e c)
    SetFill e c -> pure (SetFill e c)
    Clear e -> pure (Clear e)

drawEffectParser :: EffectParser Draw
drawEffectParser = EffectParser Proxy $
  \(et :: EnvTypeProxy et) _code -> case lemmaEnvTy @et of
    Refl -> withEnvType et pDrawEffect

pDrawEffect
  :: EnvironmentProxy env
  -> ScalarProxy t
  -> Parser (Draw code '(env, t))
pDrawEffect env = \case
  VoidProxy -> pErase env
           <|> pDrawCommand env
           <|> pUseCommand env
           <?> "draw command"
  _ -> mzero

pErase :: EnvironmentProxy env -> Parser (Draw code '(env, 'VoidT))
pErase env = do
  tok_ "erase"
  pure (Clear env)

pUseCommand :: EnvironmentProxy env
            -> Parser (Draw code '(env, 'VoidT))
pUseCommand env = do
  tok_ "use"
  c <- withEnvironment env value_
  tok_ "for"
  ((tok_ "fill" $> SetFill env c)
   <|> (tok_ "line" $> SetStroke env c)
   <|> (tok_ "stroke" $> SetStroke env c)
   <?> "color target")

pDrawCommand :: EnvironmentProxy env
             -> Parser (Draw code '(env, 'VoidT))
pDrawCommand env = do
  tok_ "draw"
  pDrawPoint
    <|> pDrawLine
    <|> pDrawCircle False
    <|> pDrawRect False
    <|> pDrawFilled
    <?> "draw command"

  where

    pDrawPoint = do
      tok_ "point" >> tok_ "at"
      DrawPoint env <$> withEnvironment env value_

    pDrawLine = do
      tok_ "line" >> tok_ "from"
      p1 <- withEnvironment env value_
      tok_ "to"
      p2 <- withEnvironment env value_
      pure (DrawLine env p1 p2)

    pDrawCircle fill = do
      tok_ "circle" >> tok_ "at"
      center <- withEnvironment env value_
      tok_ "with" >> tok_ "radius"
      r <- withEnvironment env value_
      pure (DrawCircle env fill center r)

    pDrawRect fill = do
      tok_ "rectangle" >> tok_ "from"
      p1 <- withEnvironment env value_
      tok_ "to"
      p2 <- withEnvironment env value_
      pure (DrawRect env fill p1 p2)

    pDrawFilled = do
      tok_ "filled"
      pDrawCircle True <|> pDrawRect True
