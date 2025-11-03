module Language.Typecheck
  ( TC(..)
  , Expected(..)
  , TCError(..)
  , internal
  , tryEach
  , tryEachType
  , isTypeError
  , catchTypeError
  , advise
  , ppError

  -- * Utility functions and types
  , declareVar
  , DeclaredVar(..)
  , findVar
  , FoundVar(..)
  , findVarAtType
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Environment
import Language.Parser.SourceRange

----------------------------------------------------
-- A monad for the type- and environment-checking
-- stage after the immediate parse.
----------------------------------------------------

newtype TC a = TC (Either TCError a)
  deriving (Show, Functor, Applicative, Monad, MonadError TCError)

newtype Expected t = Expected t
  deriving Show

data TCError
  = Surprise SourceRange String String (Expected String)
  | MissingName SourceRange String
  | AlreadyDefined SourceRange String
  | BadConversion SourceRange SomeType (Expected SomeType)
  | Advice SourceRange String
  | Internal TCError
  deriving Show

internal :: TCError -> TCError
internal e = case e of
  Internal _ -> e
  _ -> Internal e

tryEach :: TCError -> [TC a] -> TC a
tryEach failure = go
  where
    go = \case
      [] -> throwError failure
      (x:xs) -> catchError x (\_ -> go xs)

tryEachType :: TCError -> [TC a] -> TC a
tryEachType failure = go
  where
    go = \case
      [] -> throwError failure
      (x:xs) -> catchError x (\e -> if isTypeError e then go xs else throwError e)

catchTypeError :: TC a -> (TCError -> TC a) -> TC a
catchTypeError x action =
  catchError x (\e -> if isTypeError e then action e else throwError e)

isTypeError :: TCError -> Bool
isTypeError = \case
  Surprise{} -> True
  BadConversion{} -> True
  _ -> False

instance HasErrorLocation TCError where
  errorLocation = \case
    Surprise sr _ _ _    -> sr
    MissingName sr _     -> sr
    AlreadyDefined sr _  -> sr
    BadConversion sr _ _ -> sr
    Advice sr _          -> sr
    Internal e -> errorLocation e

instance PrettyPrint TCError where
  pp = (:[]) . ppError

ppError :: TCError -> String
ppError = concat .  \case
  Surprise _ thing got (Expected wanted) ->
    ["I expected ", wanted, " here, but ", thing, " is ", got, "."]
  MissingName _ name ->
    ["No variable named ", name, " is defined here."]
  AlreadyDefined _ name ->
    ["The variable ", name, " is already defined, and would be re-defined here."]
  BadConversion _ ty (Expected ety) ->
    ["Conversion to ", an ty, " was used here, but ", an ety, " was expected."]
  Advice _ advice -> [advice]
  Internal e ->
    ["INTERNAL ERROR, please report at ",
      "https://github.com/matt-noonan/FractalStream/issues:\n",
      ppError e]

advise :: SourceRange -> String -> TC a
advise sr = throwError . Advice sr

data DeclaredVar name ty env where
  DeclaredVar :: forall name ty env
               . (NotPresent name env, KnownType ty, KnownSymbol name)
              => NameIsAbsent name env
              -> EnvironmentProxy ( '(name, ty) ': env )
              -> DeclaredVar name ty env

declareVar :: forall name ty env
            . (KnownSymbol name, KnownType ty)
           => SourceRange
           -> Proxy name
           -> TypeProxy ty
           -> EnvironmentProxy env
           -> TC (DeclaredVar name ty env)
declareVar sr name ty env = case lookupEnv' name env of
  Absent' pf -> pure $
    withKnownType ty $ recallIsAbsent pf $ DeclaredVar pf (declare ty env)
  Found'{} -> throwError (AlreadyDefined sr (symbolVal name))

data FoundVar name env where
  FoundVar :: forall name ty env
            . (KnownType ty, KnownSymbol name)
           => TypeProxy ty
           -> NameIsPresent name ty env
           -> FoundVar name env

findVar :: forall name env
         . (KnownSymbol name)
        => SourceRange
        -> Proxy name
        -> EnvironmentProxy env
        -> TC (FoundVar name env)
findVar sr name env = case lookupEnv' name env of
  Found' ty pf -> withKnownType ty $ pure (FoundVar ty pf)
  Absent'{} -> throwError (MissingName sr (symbolVal name))

findVarAtType :: forall name ty env
         . (KnownSymbol name)
        => SourceRange
        -> Proxy name
        -> TypeProxy ty
        -> EnvironmentProxy env
        -> TC (NameIsPresent name ty env)
findVarAtType sr name ty env = case lookupEnv name ty env of
  Found pf -> pure pf
  Absent{} -> throwError (MissingName sr (symbolVal name))
  WrongType ty' -> withKnownType ty $
    throwError (Surprise sr (symbolVal name) (an ty') (Expected $ an ty))
