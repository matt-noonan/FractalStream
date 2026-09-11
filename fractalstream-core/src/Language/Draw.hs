{-# language OverloadedStrings, RecursiveDo #-}
module Language.Draw
  ( type Draw
  , Draw_(..)
  , type DrawCommand
  , transformDrawValues
  , transformDrawValuesM
  , DrawHandler(..)
  , runDrawHandler
  ) where

import FractalStream.Prelude

import Language.Value
import Control.Monad.Identity (runIdentity)

type Draw = Draw_ Value_

type DrawCommand = Draw_ ConcreteValue '[]

data ConcreteValue :: Environment -> FSType -> Exp Type
type instance Eval (ConcreteValue env t) = HaskellType t

data DrawHandler (code :: Environment -> Exp Type) where
  DrawHandler :: forall code
               . (forall env. Draw env -> Eval (code env))
              -> DrawHandler code

runDrawHandler :: forall env code
                . DrawHandler code
               -> Draw env
               -> Eval (code env)
runDrawHandler (DrawHandler f) d = f d

data Draw_ (value :: Environment -> FSType -> Exp Type)
           (env :: Environment) where

  -- | draw point at POINT
  DrawPoint :: forall env value
             . EnvironmentProxy env
            -> Eval (value env  ('Pair 'RealT 'RealT))
            -> Draw_ value env

  -- | draw circle at POINT with radius VALUE
  -- | draw filled circle at POINT with radius VALUE
  DrawCircle :: forall env value
              . EnvironmentProxy env
             -> Bool
             -> Eval (value env 'RealT)
             -> Eval (value env ('Pair 'RealT 'RealT))
             -> Draw_ value env

  -- | draw line from POINT to POINT
  DrawLine :: forall env value
            . EnvironmentProxy env
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Draw_ value env

  -- | draw rectangle from POINT to POINT
  -- | draw filled rectangle from POINT to POINT
  DrawRect :: forall env value
            . EnvironmentProxy env
           -> Bool
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Eval (value env ('Pair 'RealT 'RealT))
           -> Draw_ value env

  -- | use COLOR for stroke
  SetStroke :: forall env value
             . EnvironmentProxy env
            -> Eval (value env 'ColorT)
            -> Draw_ value env

  -- | use COLOR for fill
  SetFill :: forall env value
             . EnvironmentProxy env
            -> Eval (value env 'ColorT)
            -> Draw_ value env

  -- | erase
  Clear :: forall env value
         . EnvironmentProxy env
        -> Draw_ value env

  -- | write TEXT at POINT
  Write :: forall env value
         . EnvironmentProxy env
        -> Eval (value env 'TextT)
        -> Eval (value env ('Pair 'RealT 'RealT))
        -> Draw_ value env

deriving instance Show DrawCommand

transformDrawValues :: forall env
                     . (forall et. Value et -> Value et)
                    -> Draw env
                    -> Draw env
transformDrawValues f = runIdentity . transformDrawValuesM (pure . f)

transformDrawValuesM :: forall env m
                      . Monad m
                     => (forall et. Value et -> m (Value et))
                     -> Draw env
                     -> m (Draw env)
transformDrawValuesM f = \case
  DrawPoint env pt -> DrawPoint env <$> f pt
  DrawCircle env fill r c -> DrawCircle env fill <$> f r <*> f c
  DrawLine env pt1 pt2 -> DrawLine env <$> f pt1 <*> f pt2
  DrawRect env fill pt1 pt2 -> DrawRect env fill <$> f pt1 <*> f pt2
  SetStroke env c -> SetStroke env <$> f c
  SetFill env c -> SetFill env <$> f c
  Clear env -> pure $ Clear env
  Write env txt pt -> Write env <$> f txt <*> f pt
