{-# language OverloadedStrings #-}
module Language.Effect.Draw
  ( type Draw
  , Draw_(..)
  , type DrawCommand
  , drawEffectParser
  ) where

import Language.Value
import Language.Parser
import Language.Value.Parser
import Language.Effect
import Data.Indexed.Functor

import Fcf (Exp, Eval)
import Data.Type.Equality ((:~:)(..))
import Data.Kind

type Draw = Draw_ Value_

type DrawCommand = Draw_ ConcreteValue NoCode '( '[], 'VoidT )

data ConcreteValue :: Environment -> FSType -> Exp Type
type instance Eval (ConcreteValue env t) = HaskellType t

data NoCode :: (Environment, FSType) -> Exp Type
type instance Eval (NoCode et) = ()

data Draw_ (value :: Environment -> FSType -> Exp Type)
           (code :: (Environment, FSType) -> Exp Type)
           (et :: (Environment, FSType)) where

  -- | draw point at POINT
  DrawPoint :: forall env code value
             . EnvironmentProxy env
            -> Eval (value env  ('Pair 'RealT 'RealT))
            -> Draw_ value code '(env, 'VoidT)

  -- | draw circle at POINT with radius VALUE
  -- | draw filled circle at POINT with radius VALUE
  DrawCircle :: forall env code value
              . EnvironmentProxy env
             -> Bool
             -> Eval (value env 'RealT)
             -> Eval (value env ('Pair 'RealT 'RealT))
             -> Draw_ value code '(env, 'VoidT)

  -- | draw line from POINT to POINT
  DrawLine :: forall env code value
            . EnvironmentProxy env
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Draw_ value code '(env, 'VoidT)

  -- | draw rectangle from POINT to POINT
  -- | draw filled rectangle from POINT to POINT
  DrawRect :: forall env code value
            . EnvironmentProxy env
           -> Bool
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Draw_ value code '(env, 'VoidT)

  -- | use COLOR for stroke
  SetStroke :: forall env code value
             . EnvironmentProxy env
            -> Eval (value env 'ColorT)
            -> Draw_ value code '(env, 'VoidT)

  -- | use COLOR for fill
  SetFill :: forall env code value
             . EnvironmentProxy env
            -> Eval (value env 'ColorT)
            -> Draw_ value code '(env, 'VoidT)

  -- | erase
  Clear :: forall env code value
         . EnvironmentProxy env
        -> Draw_ value code '(env, 'VoidT)

instance IFunctor (Draw_ value) where
  type IndexProxy (Draw_ value) = EnvTypeProxy
  imap _ = \case
    DrawPoint e p -> DrawPoint e p
    DrawCircle e f r p -> DrawCircle e f r p
    DrawLine e f t -> DrawLine e f t
    DrawRect e f c1 c2 -> DrawRect e f c1 c2
    SetStroke e c -> SetStroke e c
    SetFill e c -> SetFill e c
    Clear e -> Clear e
  toIndex = \case
    DrawPoint e _ -> envTypeProxy e VoidType
    DrawCircle e _ _ _ -> envTypeProxy e VoidType
    DrawLine e _ _ -> envTypeProxy e VoidType
    DrawRect e _ _ _ -> envTypeProxy e VoidType
    SetStroke e _ -> envTypeProxy e VoidType
    SetFill e _ -> envTypeProxy e VoidType
    Clear e -> envTypeProxy e VoidType

instance ITraversable (Draw_ value) where
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
  -> TypeProxy t
  -> Parser (Draw code '(env, t))
pDrawEffect env = \case
  VoidType -> ((pErase env
           <|> pDrawCommand env
           <|> pUseCommand env) <* eol)
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
  valueTokens <- manyTill anyToken (tok_ "for")
  c <- nest (parseValueFromTokens env EmptyContext ColorType valueTokens)
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

    pDrawPoint = withEnvironment env $ do
      tok_ "point" >> tok_ "at"
      DrawPoint env . Fix . C2R2 <$> value_ EmptyContext

    pDrawLine = withEnvironment env $ do
      tok_ "line" >> tok_ "from"
      valueTokens <- manyTill anyToken (tok_ "to")
      p1 <- Fix . C2R2 <$> nest (parseValueFromTokens env EmptyContext ComplexType valueTokens)
      p2 <- Fix . C2R2 <$> value_ EmptyContext
      pure (DrawLine env p1 p2)

    pDrawCircle fill = withEnvironment env $ do
      tok_ "circle" >> tok_ "at"
      valueTokens <- manyTill anyToken (tok_ "with")
      center <- Fix . C2R2 <$> nest (parseValueFromTokens env EmptyContext ComplexType valueTokens)
      tok_ "radius"
      r <- value_ EmptyContext
      pure (DrawCircle env fill r center)

    pDrawRect fill = withEnvironment env $ do
      tok_ "rectangle" >> tok_ "from"
      valueTokens <- manyTill anyToken (tok_ "to")
      p1 <- Fix . C2R2 <$> nest (parseValueFromTokens env EmptyContext ComplexType valueTokens)
      p2 <- Fix . C2R2 <$> value_ EmptyContext
      pure (DrawRect env fill p1 p2)

    pDrawFilled = do
      tok_ "filled"
      pDrawCircle True <|> pDrawRect True
