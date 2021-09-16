module Language.Code.Parser
  ( parseCode
  , pCode
  , pBlock
  , pLine
  , pVoidLine
  , pNonVoidLine
  , EffectParser(..)
  , EffectParsers(..)
  , EffectParsers_(..)
  ) where

import Language.Type
import Language.Parser
import Language.Value
import Language.Value.Parser
import Language.Code

import GHC.TypeLits
import Data.Type.Equality ((:~:)(..))

parseCode :: forall effs env t
           . EffectParsers effs
          -> EnvironmentProxy env
          -> ScalarProxy t
          -> String
          -> Either (Int, BadParse) (Code effs env t)
parseCode eps env t input
  = parse (pCode eps env t <* eof) (tokenizeWithIndentation input)

-- | The code grammar is a tiny bit monadic, because the allowed parses
-- depend on which variables are in scope, and this can change as new 'var'
-- bindings are introduced. That's enough to prevent the use of a pure
-- applicative parser like Text.Earley, so we'll use Megaparsec's monadic
-- parsing here. The code grammar is simple enough that we can do this fairly
-- easily.
pCode :: forall effs env t
       . EffectParsers effs
      -> EnvironmentProxy env
      -> ScalarProxy t
      -> Parser (Code effs env t)
pCode eps env t
  = dbg "pCode" ( pBlock   eps env t
  <|> pLine    eps env t
  <|> pEffects eps env t)

pCode' :: forall effs et
        . EffectParsers effs
       -> EnvTypeProxy et
       -> Parser (Code effs (Env et) (Ty et))
pCode' eps et = withEnvType et (pCode eps)

-- | An indented block of statements
pBlock :: EffectParsers effs
          ->  EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pBlock eps env = \case
  VoidProxy -> dbg "void block" $ do
    tok_ Indent
    body <- some (pCode eps env VoidProxy)
    tok_ Dedent
    withEnvironment env (pure (Fix (Block VoidProxy (init body) (last body))))
  t -> dbg ("pBlock @" ++ showType t) $ do
    tok_ Indent
    -- This is kind of stupid
    (body, final) <- manyTill_ (pCode eps env VoidProxy) (try (pCode eps env t))
    tok_ Dedent
    withEnvironment env (pure (Fix (Block t body final)))

-- | Parse a single line; in practice, this is more complex because
-- let-bindings are syntactically a single line, but they introduce a
-- variable that is scoped over the remainder of the current block.
pLine :: EffectParsers effs
          -> EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pLine eps env = \case
  VoidProxy -> try (pAnyLine eps env VoidProxy) <|> pVoidLine eps env
  t         -> try (pAnyLine eps env t)         <|> pNonVoidLine eps env t

pVoidLine :: EffectParsers effs
          ->  EnvironmentProxy env -> Parser (Code effs env 'VoidT)
pVoidLine eps env
  = dbg "void line" ( pLoop eps env
  <|> pSet  eps env
  <|> pPass     env
  <?> "statement")

pNonVoidLine :: EffectParsers effs
          -> EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pNonVoidLine _eps env t
  = dbg "non-void line" ( pPure env t
  <?> ("statement of type " <> showType t))

pAnyLine :: EffectParsers effs
          ->  EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pAnyLine eps env t
  = dbg "anyLine" ( pIfThenElse eps env t
  <|> pEffects    eps env t
  <|> pVar        eps env t)

-- |
-- if TEST then
--   BLOCK
-- else
--   BLOCK
--
-- if TEST then
--   BLOCK
-- else if TEST then
--   BLOCK
-- else
--   BLOCK
--
-- if TEST then
--   VOIDBLOCK
--
-- if TEST then
--   VOIDBLOCK
-- else if TEST then
--   VOIDBLOCK
--
pIfThenElse :: forall effs env t
             . EffectParsers effs
            -> EnvironmentProxy env
            -> ScalarProxy t
            -> Parser (Code effs env t)
pIfThenElse eps env t = dbg "if/then/else statement" $ withEnvironment env $ do
  tok_ If
  (toks, _) <- manyTill_ anyToken (satisfy (== Then))
  eol
  cond <- nest (parseValueFromTokens env BooleanProxy toks)
  yes <- pBlock eps env t
  no <- case t of
    VoidProxy -> try (tok_ Else >> pIfThenElse eps env t)
                 <|> (tok_ Else >> eol >> pBlock eps env t)
                 <|> pure (Fix (NoOp @env))
    _         -> try (tok_ Else >> pIfThenElse eps env t)
                 <|> (tok_ Else >> eol >> pBlock eps env t)
  pure (Fix (IfThenElse t cond yes no))

-- | pass
pPass :: EnvironmentProxy env -> Parser (Code effs env 'VoidT)
pPass env = do
  tok_ (Identifier "pass")
  eol
  pure (withEnvironment env (Fix NoOp))

-- | pure VALUE
pPure :: forall effs env t
       . EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pPure env t = dbg ("pure @" <> showType t) $ do
  toks <- manyTill anyToken eol
  withEnvironment env $ do
    result <- Fix . Pure t <$> nest (parseValueFromTokens env t toks)
    pure result

-- |
-- repeat
--   BLOCK
-- while TEST
pLoop :: forall effs env. EffectParsers effs
          ->  EnvironmentProxy env -> Parser (Code effs env 'VoidT)
pLoop eps env = dbg "loop" $ do
  {-
  tok_ (Identifier "repeat")
  eol
  tok_ Indent
  body <- some (pCode @effs env VoidProxy)
  tok_ Dedent
-}
  tok_ (Identifier "loop") >> eol
  withEnvironment env (Fix . DoWhile <$> pBlock eps env BooleanProxy)

-- |
-- set VAR to VALUE
-- set VAR <- CODE
pSet :: EffectParsers effs -> EnvironmentProxy env -> Parser (Code effs env 'VoidT)
pSet effs env = dbg "set" $ do
  tok_ (Identifier "set")
  Identifier n <- satisfy (\case { (Identifier _) -> True; _ -> False })
  (    (tok_ (Identifier "to") *> pTo n)
   <|> (tok_ LeftArrow *> pBind n)
   <?> "binding style")
 where
  pTo n = do
    toks <- manyTill anyToken eol

    -- Figure out what type the RHS should have, and try to parse a
    -- value at that type.
    case someSymbolVal n of
      SomeSymbol name -> case lookupEnv' name env of
        Absent' _ -> mzero
        Found' t pf -> do
          v <- nest (parseValueFromTokens env t toks)
          withEnvironment env (pure (Fix (Set pf name v)))

  pBind n = do
    -- Figure out what type the RHS should have, and try to parse some
    -- code at that type.
    case someSymbolVal n of
      SomeSymbol name -> case lookupEnv' name env of
        Absent' _ -> mzero
        Found' t pf -> do
          code <- pCode effs env t
          withEnvironment env (pure (Fix (SetBind pf name t code)))

-- | Variable initialization
pVar :: forall effs env t
      . EffectParsers effs
     -> EnvironmentProxy env
     -> ScalarProxy t
     -> Parser (Code effs env t)
pVar eps env t = dbg "init" $ withKnownType t $ do
  tok_ (Identifier "init")
  Identifier n <- satisfy (\case { (Identifier _) -> True; _ -> False })
  tok_ Colon
  someVt <- pTypeName
  case someSymbolVal n of
    SomeSymbol name -> case lookupEnv' name env of
      Found' {} -> mzero -- name is already bound
      Absent' pf -> case someVt of
       SomeType vt ->  pVarValue eps env t pf name vt -- init x : T to VALUE
                   <|> pVarCode  eps env t pf name vt -- init x : T <- CODE

pVarValue :: forall effs env t ty name
           . KnownSymbol name
          => EffectParsers effs
          -> EnvironmentProxy env
          -> ScalarProxy t
          -> NameIsAbsent name env
          -> Proxy name
          -> ScalarProxy ty
          -> Parser (Code effs env t)
pVarValue eps env t pf name vt = do
  tok_ (Identifier "to")
  toks <- manyTill anyToken eol
  v <- nest (parseValueFromTokens env vt toks)
  withEnvironment env $ withKnownType vt $ recallIsAbsent pf $ do
    let pf' = bindName name vt pf
        env' = BindingProxy name vt env
    (body, final) <- case t of
      VoidProxy -> (\xs -> (init xs, last xs))
                     <$> some (pCode eps env' VoidProxy)
      _ -> manyTill_ (pCode eps env' VoidProxy) (try (pCode eps env' t))
    -- peek at the next token, which should be a dedent; we should have parsed the
    -- remainder of the current scope's block.
    lookAhead (tok_ Dedent)
    pure (Fix (Let pf' name vt v t (Fix (Block t body final))))

pVarCode  :: forall effs env t ty name
           . KnownSymbol name
          => EffectParsers effs
          -> EnvironmentProxy env
          -> ScalarProxy t
          -> NameIsAbsent name env
          -> Proxy name
          -> ScalarProxy ty
          -> Parser (Code effs env t)
pVarCode eps env t pf name vt = do
  tok_ LeftArrow
  c <- pCode eps env vt
  many eol
  withEnvironment env $ withKnownType vt $ recallIsAbsent pf $ do
    let pf' = bindName name vt pf
        env' = BindingProxy name vt env
    (body, final) <- case t of
      VoidProxy -> (\xs -> (init xs, last xs))
                     <$> some (pCode eps env' VoidProxy)
      _ -> manyTill_ (pCode eps env' VoidProxy) (try (pCode eps env' t))
    -- peek at the next token, which should be a dedent; we should have parsed the
    -- remainder of the current scope's block.
    lookAhead (tok_ Dedent)
    pure (Fix (LetBind pf' name vt c t (Fix (Block t body final))))


-- | Parse type name
pTypeName :: Parser SomeType
pTypeName
  =   (SomeType RealProxy    <$ tok_ (Identifier "R"))
  <|> (SomeType IntegerProxy <$ tok_ (Identifier "Z"))
  <|> (SomeType ComplexProxy <$ tok_ (Identifier "C"))
  <|> (SomeType ColorProxy   <$ tok_ (Identifier "Color"))
  <?> "type"

-- | Parse embedded effect languages
pEffects :: forall effs env t
          . EffectParsers effs
         -> EnvironmentProxy env
         -> ScalarProxy t
         -> Parser (Code effs env t)
pEffects (EP eps) env t = dbg "effects" $ go eps
  where
    go :: forall effs'. EffectParsers_ effs' effs -> Parser (Code effs env t)
    go NoEffs = mzero
    go (ParseEff (EffectParser eff p) etc) =
      withEnvironment env (Fix . Effect eff Proxy t
                           <$> p (envTypeProxy env t) (\(et' :: EnvTypeProxy et') ->
                                                         case lemmaEnvTy @et' of
                                                           Refl -> pCode' (EP eps) et'))
      <|> go etc
