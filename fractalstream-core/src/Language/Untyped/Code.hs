{-# language TemplateHaskell #-}
module Language.Untyped.Code
  ( type Code
  , CodeF(..)
  , attachScope
  ) where

import Language.Untyped.Value
import Data.Recursive
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Except
import Data.Bifunctor
import Data.Bifunctor.TH

type Code = Fix (CodeF Value)

data CodeF value code
  = Let String value code
  | LetBind String code code
  | Set String value
  | SetBind String code
  | Call code
  | Block [code]
  | Pure value
  | DoWhile code
  | IfThenElse value code code
  -- Render effect
  | Render String String value value value code
  | HaltRender value
  | Blit value value value value
  | ClearTo value
  -- Draw effect
  | DrawPoint value
  | DrawCircle Bool value value
  | DrawLine value value
  | DrawRect Bool value value
  | SetStroke value
  | SetFill value
  | Clear
  -- List effect
  | Insert String value
  | Lookup String String value code code
  | ClearList String
  | Remove String String value
  | ForEach String String code
  -- Output effect
  | Output String value
  -- Provide effect
  | Provide (Set String) code
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

$(deriveBifunctor ''CodeF)

data BindingError
  = Shadowing String
  | Unbound String
  deriving Show

type CodeWithEnv  = Ann  (CodeF (Set String, Value)) (Set String)
type CodeWithEnvF = AnnF (CodeF (Set String, Value)) (Set String)

-- | Annotate each 'Code' AST node and @value@ with its environment.
withBindings :: (Set String, Code)
             -> Either BindingError (CodeWithEnvF (Set String, Code))
withBindings (env, Fix code) = AnnF env <$> case code of
  Let n v body -> do
    when (Set.member n env) (throwError (Shadowing n))
    pure (Let n (env, v) (Set.insert n env, body))

  LetBind n c body -> do
    when (Set.member n env) (throwError (Shadowing n))
    pure (LetBind n (env, c) (Set.insert n env, body))

  Set n v -> do
    unless (Set.member n env) (throwError (Unbound n))
    pure (Set n (env, v))

  SetBind n c -> do
    unless (Set.member n env) (throwError (Unbound n))
    pure (SetBind n (env, c))

  Render x y dim pos delta body -> do
    when (Set.member x env) (throwError (Shadowing x))
    when (Set.member y env) (throwError (Shadowing y))
    let env' = Set.insert x (Set.insert y env)
    pure (Render x y (env, dim) (env, pos) (env, delta) (env', body))

  Lookup lst n p yes no -> do
    when (Set.member n env) (throwError (Shadowing n))
    let env' = Set.insert n env
    pure (Lookup lst n (env', p) (env', yes) (env, no))

  Remove lst n p -> do
    when (Set.member n env) (throwError (Shadowing n))
    pure (Remove lst n (Set.insert n env, p))

  ForEach lst n body -> do
    when (Set.member n env) (throwError (Shadowing n))
    pure (ForEach lst n (Set.insert n env, body))

  Provide ns c -> do
    case Set.toList (Set.intersection ns env) of
      (n:_) -> throwError (Shadowing n)
      []    -> pure (Provide ns (env `Set.union` ns, c))

  etc -> pure (bimap (env,) (env,) etc)

attachScope :: Set String
            -> Code
            -> Either BindingError CodeWithEnv
attachScope = curry (unfoldM withBindings)
