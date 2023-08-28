{-# language OverloadedStrings #-}

module Language.Effect.List
  ( List(..)
  , MaybeIfVoid(..)
  , listEffectParser
  ) where

import Language.Value
import Data.Indexed.Functor
import Language.Parser
import Language.Code.Parser
import Language.Value.Parser

import Fcf
import GHC.TypeLits
import Data.Type.Equality ((:~:)(..))
import Data.Kind

-- | A type that is like @a@ if @t@ is non-void,
-- or like @Maybe a@ when @t@ is @'VoidT@.
data MaybeIfVoid (t :: FSType) a where
  VNothing :: forall a. MaybeIfVoid 'VoidT a
  VJust :: forall t a. a -> MaybeIfVoid t a

instance Functor (MaybeIfVoid t) where
  fmap f = \case
    VNothing -> VNothing
    VJust x  -> VJust (f x)

instance Foldable (MaybeIfVoid t) where
  foldMap f = \case
    VNothing -> mempty
    VJust x  -> f x

instance Traversable (MaybeIfVoid t) where
  sequenceA = \case
    VNothing -> pure VNothing
    VJust mx -> VJust <$> mx

data List (listName :: Symbol) (listType :: FSType) (code :: (Environment, FSType) -> Exp Type) (et :: (Environment, FSType)) where

  Insert :: forall name ty env code
          . Proxy name
         -> TypeProxy ty
         -> EnvTypeProxy '(env, 'VoidT)
         -> Value '(env, ty)
         -> List name ty code '(env, 'VoidT)

  Lookup :: forall name ty env t code item
          . KnownSymbol item
         => Proxy name
         -> TypeProxy ty
         -> Proxy item
         -> NameIsAbsent item env
         -> EnvTypeProxy '( '(item, ty) ': env, t)
         -> EnvTypeProxy '(env, t)
         -> Value '( '(item, ty) ': env, 'BooleanT)
         -- ^ predicate to test for
         -> Eval (code '( '(item, ty) ': env, t))
         -- ^ action to run on the first item matching the predicate
         -> MaybeIfVoid t (Eval (code '(env, t)))
         -- ^ fallback action to run if there was no match; optional
         -- if the result type is void.
         -> List name ty code '(env, t)

  ClearList :: forall name ty env code
             . Proxy name
            -> TypeProxy ty
            -> EnvTypeProxy '(env, 'VoidT)
            -> List name ty code '(env, 'VoidT)

  Remove :: forall name ty env code item
          . KnownSymbol item
         => Proxy name
         -> TypeProxy ty
         -> Proxy item
         -> NameIsAbsent item env
         -> EnvTypeProxy '(env, 'VoidT)
         -> Value '( '(item, ty) ': env, 'BooleanT)
         -> List name ty code '(env, 'VoidT)

  ForEach :: forall name ty env code item
           . KnownSymbol item
          => Proxy name
          -> TypeProxy ty
          -> Proxy item
          -> NameIsAbsent item env
          -> EnvTypeProxy '(env, 'VoidT)
          -> EnvTypeProxy '( '(item, ty) ': env, 'VoidT)
          -> Eval (code '( '( item, ty) ': env, 'VoidT))
          -> List name ty code '(env, 'VoidT)

instance IFunctor (List name ty) where
  type IndexProxy (List name ty) = EnvTypeProxy

  imap :: forall a b i
        . (forall j. EnvTypeProxy j -> Eval (a j) -> Eval (b j))
       -> List name ty a i
       -> List name ty b i
  imap f = \case
    Insert name ty et v -> Insert name ty et v
    Lookup name ty item pf et et' test match miss ->
      Lookup name ty item pf et et' test (f et match) (f et' <$> miss)
    ClearList name ty et -> ClearList name ty et
    Remove name ty item pf et v -> Remove name ty item pf et v
    ForEach name ty item pf et et' body -> ForEach name ty item pf et et' (f et' body)

  toIndex = \case
    Insert _ _ et _           -> et
    Lookup _ _ _ _ _ et _ _ _ -> et
    ClearList _ _ et          -> et
    Remove _ _ _ _ et _       -> et
    ForEach _ _ _ _ et _ _    -> et

instance ITraversable (List name ty) where

  isequence = \case
    Insert name ty et v -> pure (Insert name ty et v)
    Lookup name ty item pf et et' test mmatch mmiss ->
      Lookup name ty item pf et et' test <$> mmatch <*> sequenceA mmiss
    ClearList name ty et -> pure (ClearList name ty et)
    Remove name ty item pf et v -> pure (Remove name ty item pf et v)
    ForEach name ty item pf et et' mbody -> ForEach name ty item pf et et' <$> mbody

listEffectParser :: forall name ty
                  . (KnownSymbol name, KnownType ty)
                 => EffectParser (List name ty)
listEffectParser = EffectParser Proxy $ \(et :: EnvTypeProxy et) code_ -> do
  -- All list effect commands have the form of a line followed by the list name,
  -- e.g. "insert 42 at start of myList"
  -- Check if the first few tokens are the start of a list command. If so,
  -- scan ahead to the token before the last end-of-line and check that it
  -- is the name of this list.
  lookAhead (foldr1 (<|>) (map (foldr1 (>>) . map tok_) listStarts))
  lookAhead $ do
    many (satisfy (\t -> t /= Identifier name && t /= Newline && t /= "do"))
    tok_ (Identifier name)
    (eol <|> tok_ "do")

  case lemmaEnvTy @et of
    Refl -> withEnvType et $ \env -> \case
      VoidType ->  pInsert env code_
                <|> pRemoveSome env code_
                <|> pRemoveAll env code_
                <|> pFor env code_
                <|> pWith env VoidType code_
                <?> ("list command for " ++ name)
      ty ->
        (pWith env ty code_ <?> ("list command for " ++ name))

  where
    name = symbolVal (Proxy @name)
    listStarts = [ ["insert"]
                 , ["remove"]
                 , ["for", "each"]
                 , ["with", "first"]
                 ]

    -- insert VALUE at start of LISTNAME
    -- insert VALUE at end of LISTNAME
    pInsert :: EnvironmentProxy env
            -> (forall et. EnvTypeProxy et -> Parser (Eval (code et)))
            -> Parser (List name ty code '(env, 'VoidT))
    pInsert (env :: EnvironmentProxy env) _ = withEnvironment env $ do
      tok_ "insert"
      v <- value_ @ty @env EmptyContext
      tok_ "at"
      let start = do
            tok_ "start" >> tok_ "of" >> tok_ (Identifier name) >> eol
            pure (Insert Proxy typeProxy (envTypeProxy env VoidType) v)
          end = do
            tok_ "end" >> tok_ "of" >> tok_ (Identifier name) >> eol
            pure (Insert Proxy typeProxy (envTypeProxy env VoidType) v) -- FIXME
      (start <|> end <?> "insertion style")

    -- remove each item matching VALUE from LISTNAME
    pRemoveSome :: EnvironmentProxy env
                -> (forall et. EnvTypeProxy et -> Parser (Eval (code et)))
                -> Parser (List name ty code '(env, 'VoidT))
    pRemoveSome (env :: EnvironmentProxy env) _ = withEnvironment env $ do
      -- 'try' because this has a prefix overlap with pRemoveAll
      try (tok_ "remove" >> tok_ "each")
      Identifier itemStr <- satisfy (\case { Identifier _ -> True; _ -> False })
      tok_ "matching"
      case someSymbolVal itemStr of
        SomeSymbol (item :: Proxy item) -> case lookupEnv' item env of
          Found' {} -> fail ("a variable named '" <> itemStr <> "' is already defined")
          Absent' pf -> recallIsAbsent pf $ do
            vtoks <- manyTill anyToken (tok_ "from")
            let env' = bindNameEnv item (typeProxy @ty) pf env
            v <- nest (parseValueFromTokens env' EmptyContext BooleanType vtoks)
            tok_ (Identifier name) >> eol
            pure (Remove Proxy (typeProxy @ty) item pf (envTypeProxy env VoidType) v)

    -- remove all items from LISTNAME
    pRemoveAll :: EnvironmentProxy env
               -> (forall et. EnvTypeProxy et -> Parser (Eval (code et)))
               -> Parser (List name ty code '(env, 'VoidT))
    pRemoveAll env _ = do
      tok_ "remove" >> tok_ "all" >> tok_ "items"
        >> tok_ "from" >> tok_ (Identifier name) >> eol
      pure (ClearList Proxy (typeProxy @ty) (envTypeProxy env VoidType))

    -- for each item in LISTNAME do CODE
    pFor :: forall env code
          . EnvironmentProxy env
         -> (forall et. EnvTypeProxy et -> Parser (Eval (code et)))
         -> Parser (List name ty code '(env, 'VoidT))
    pFor env code_ = do
      tok_ "for" >> tok_ "each"
      Identifier itemStr <- satisfy (\case { Identifier _ -> True; _ -> False })
      tok_ "in" >> tok_ (Identifier name) >> tok_ "do"
      many eol
      lookAhead (tok_ Indent)
      case someSymbolVal itemStr of
        SomeSymbol (item :: Proxy item) -> case lookupEnv' item env of
          Found' {} -> fail "a variable named 'item' is already defined"
          Absent' pf -> recallIsAbsent pf $ do
            let env' = bindNameEnv item (typeProxy @ty) pf env
            body <- code_ (envTypeProxy env' VoidType)
            pure (ForEach Proxy (typeProxy @ty) item pf
                   (envTypeProxy env VoidType)
                   (envTypeProxy env' VoidType)
                   body)

    -- with first item matching VALUE in LISTNAME do CODE
    -- with first item matching VALUE in LISTNAME do CODE else CODE
    pWith :: forall env t code
           . EnvironmentProxy env
          -> TypeProxy t
          -> (forall et. EnvTypeProxy et -> Parser (Eval (code et)))
          -> Parser (List name ty code '(env, t))
    pWith env t code_ = withEnvironment env $ do
      tok_ "with" >> tok_ "first"
      Identifier itemStr <- satisfy (\case { Identifier _ -> True; _ -> False })
      tok_ "matching"
      case someSymbolVal itemStr of
        SomeSymbol (item :: Proxy item) -> case lookupEnv' item env of
          Found' {} -> fail "a variable named 'item' is already defined"
          Absent' pf -> recallIsAbsent pf $ do
            let env' = bindNameEnv item (typeProxy @ty) pf env
            v <- value_ @'BooleanT @( '(item, ty) ': env) EmptyContext
            tok_ "in" >> tok_ (Identifier name)
            tok_ "do"
            many eol
            lookAhead (tok_ Indent)
            case t of
              VoidType -> do
                match <- code_ (envTypeProxy env' t)
                -- Else branch is optional for void return type
                let elseParser = do
                      tok_ "else" >> many eol >> lookAhead (tok_ Indent)
                      VJust <$> code_ (envTypeProxy env t)
                miss <- elseParser <|> pure VNothing
                pure (Lookup Proxy (typeProxy @ty) item pf
                      (envTypeProxy env' VoidType)
                      (envTypeProxy env  VoidType)
                      v match miss)
              _ -> do
                match <- code_ (envTypeProxy env' t)
                many eol >> tok_ "else" >> many eol >> lookAhead (tok_ Indent)
                miss <- VJust <$> code_ (envTypeProxy env t)
                pure (Lookup Proxy (typeProxy @ty) item pf
                      (envTypeProxy env' t)
                      (envTypeProxy env t)
                      v match miss)
