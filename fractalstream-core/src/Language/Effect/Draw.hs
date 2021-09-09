{-# language OverloadedStrings #-}
module Language.Effect.Draw
  ( Draw(..)
  , pDrawEffect
  ) where

import Language.Value
import Language.Parser
import Language.Value.Parser
import Data.Functor (($>))
import Fcf (Exp)

data Draw (code :: (Environment, Type) -> Exp *) (et :: (Environment, Type)) where

  -- | draw point at POINT
  DrawPoint :: forall env code
             . Value env ('Pair 'RealT 'RealT)
            -> Draw code '(env, 'VoidT)

  -- | draw circle at POINT with radius VALUE
  -- | draw filled circle at POINT with radius VALUE
  DrawCircle :: forall env code
              . Bool
             -> Value env 'RealT
             -> Value env ('Pair 'RealT 'RealT)
             -> Draw code '(env, 'VoidT)

  -- | draw line from POINT to POINT
  DrawLine :: forall env code
            . Value env ('Pair 'RealT 'RealT)
           -> Value env ('Pair 'RealT 'RealT)
           -> Draw code '(env, 'VoidT)

  -- | draw rectangle from POINT to POINT
  -- | draw filled rectangle from POINT to POINT
  DrawRect :: forall env code
            . Bool
           -> Value env ('Pair 'RealT 'RealT)
           -> Value env ('Pair 'RealT 'RealT)
           -> Draw code '(env, 'VoidT)

  -- | use COLOR for stroke
  SetStroke :: forall env code
             . Value env 'ColorT
            -> Draw code '(env, 'VoidT)

  -- | use COLOR for fill
  SetFill :: forall env code
             . Value env 'ColorT
            -> Draw code '(env, 'VoidT)

  -- | erase
  Clear :: forall env code. Draw code '(env, 'VoidT)

pDrawEffect
  :: EnvironmentProxy env
  -> ScalarProxy t
  -> Parser (Draw code '(env, t))
pDrawEffect env = \case
  VoidProxy -> pErase
           <|> pDrawCommand env
           <|> pUseCommand env
           <?> "draw command"
  _ -> mzero

pErase :: Parser (Draw code '(env, 'VoidT))
pErase = do
  tok_ "erase"
  pure Clear

pUseCommand :: EnvironmentProxy env
            -> Parser (Draw code '(env, 'VoidT))
pUseCommand env = do
  tok_ "use"
  c <- withEnvironment env value_
  tok_ "for"
  ((tok_ "fill" $> SetFill c)
   <|> (tok_ "line" $> SetStroke c)
   <|> (tok_ "stroke" $> SetStroke c)
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
      DrawPoint <$> withEnvironment env value_

    pDrawLine = do
      tok_ "line" >> tok_ "from"
      p1 <- withEnvironment env value_
      tok_ "to"
      p2 <- withEnvironment env value_
      pure (DrawLine p1 p2)

    pDrawCircle fill = do
      tok_ "circle" >> tok_ "at"
      center <- withEnvironment env value_
      tok_ "with" >> tok_ "radius"
      r <- withEnvironment env value_
      pure (DrawCircle fill center r)

    pDrawRect fill = do
      tok_ "rectangle" >> tok_ "from"
      p1 <- withEnvironment env value_
      tok_ "to"
      p2 <- withEnvironment env value_
      pure (DrawRect fill p1 p2)

    pDrawFilled = do
      tok_ "filled"
      pDrawCircle True <|> pDrawRect True
