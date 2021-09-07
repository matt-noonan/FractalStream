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

import Control.Monad
import GHC.TypeLits


data EffectParser (eff :: Effect) where
  EffectParser :: forall eff
                . Proxy eff
               -> (forall env t
                   . EnvironmentProxy env
                  -> ScalarProxy t
                  -> Parser (eff env t))
               -> EffectParser eff

newtype EffectParsers effs = EP (EffectParsers_ effs effs)

data EffectParsers_ (effs :: [Effect]) (effs0 :: [Effect]) where
  ParseEff :: forall eff effs effs0
            . HasEffect eff effs0
           => EffectParser eff
           -> EffectParsers_ effs effs0
           -> EffectParsers_ (eff ': effs) effs0
  NoEffs :: forall effs0. EffectParsers_ '[] effs0

parseCode :: forall effs env t
           . EffectParsers effs
          -> EnvironmentProxy env
          -> ScalarProxy t
          -> String
          -> Either (Int, BadParse) (Code effs env t)
parseCode eps env t input
  = parse (pCode eps env t) (tokenizeWithIndentation input)

-- | The code grammar is a tiny bit monadic, because the allowed parses
-- depend on which variables are in scope, and this can change as new 'var'
-- bindings are introduced. That's enough to prevent the use of a pure
-- applicative parser like Text.Earley, so we'll use Megaparsec's monadic
-- parsing here. The code grammar is simple enough that we can do this fairly
-- easily.
pCode :: forall effs env t. EffectParsers effs
          -> EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pCode eps env t
  =   pBlock   eps env t
  <|> pLine    eps env t
  <|> pEffects eps env t

-- | End of line (including end of input)
eol :: Parser ()
eol = eof <|> tok_ Newline

-- | An indented block of statements
pBlock :: EffectParsers effs
          ->  EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pBlock eps env = \case
  VoidProxy -> do
    tok_ Indent
    body <- some (pCode eps env VoidProxy)
    tok_ Dedent
    withEnvironment env (pure (Fix (Block VoidProxy (init body) (last body))))
  t -> do
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
  VoidProxy -> try (pVoidLine eps env)      <|> pAnyLine eps env VoidProxy
  t         -> try (pNonVoidLine eps env t) <|> pAnyLine eps env t

pVoidLine :: EffectParsers effs
          ->  EnvironmentProxy env -> Parser (Code effs env 'VoidT)
pVoidLine eps env
  =   pLoop eps env
  <|> pSet      env
  <|> pPass     env
  <?> "statement"

pNonVoidLine :: EffectParsers effs
          -> EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pNonVoidLine _eps env t
  =   pPure env t
  <?> ("statement of type " <> showType t)

pAnyLine :: EffectParsers effs
          ->  EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pAnyLine eps env t
  =   pIfThenElse eps env t
  <|> pEffects    eps env t
  <|> pVar        eps env t

-- |
-- if TEST
-- then
--   BLOCK
-- else
--   BLOCK
pIfThenElse :: EffectParsers effs
          ->  EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pIfThenElse eps env t = do
  tok_ If
  (toks, _) <- manyTill_ anyToken (satisfy (== Then))
  eol
  cond <- nest (parseValueFromTokens env BooleanProxy toks)
  yes <- pBlock eps env t
  tok_ Else
  eol
  no <- pBlock eps env t
  withEnvironment env (pure (Fix (IfThenElse t cond yes no)))

-- | pass
pPass :: EnvironmentProxy env -> Parser (Code effs env 'VoidT)
pPass env = do
  tok_ (Identifier "pass")
  eol
  pure (withEnvironment env (Fix NoOp))

-- | pure VALUE
pPure :: forall effs env t
       . EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pPure env t = do
  toks <- manyTill anyToken eol
  withEnvironment env (Fix . Pure <$> nest (parseValueFromTokens env t toks))

-- |
-- repeat
--   BLOCK
-- while TEST
pLoop :: forall effs env. EffectParsers effs
          ->  EnvironmentProxy env -> Parser (Code effs env 'VoidT)
pLoop eps env = do
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
pSet :: EnvironmentProxy env -> Parser (Code effs env 'VoidT)
pSet env = do
  tok_ (Identifier "set")
  Identifier n <- satisfy (\case { (Identifier _) -> True; _ -> False })
  tok_ (Identifier "to")
  toks <- manyTill anyToken eol

  -- Figure out what type the RHS should have, and try to parse a
  -- value at that type.
  case someSymbolVal n of
    SomeSymbol name -> case lookupEnv' name env of
      Absent' _ -> mzero
      Found' t pf -> do
        v <- nest (parseValueFromTokens env t toks)
        withEnvironment env (pure (Fix (Set pf name v)))

-- | Variable
pVar :: forall effs env t
      . EffectParsers effs
     -> EnvironmentProxy env
     -> ScalarProxy t
     -> Parser (Code effs env t)
pVar eps env t = withKnownType t $ do
  tok_ (Identifier "init")
  Identifier n <- satisfy (\case { (Identifier _) -> True; _ -> False })
  tok_ (Identifier "to")
  toks <- manyTill anyToken eol
  sv <- parseSomeValue env toks
  case someSymbolVal n of
    SomeSymbol name -> case lookupEnv' name env of
      Found' {} -> mzero -- name is already bound
      Absent' pf -> case sv of
        SomeValue vt v -> withEnvironment env $ withKnownType vt $ recallIsAbsent pf $ do
          let pf' = bindName name vt pf
              env' = BindingProxy name vt (Proxy @env)
          (body, final) <- case t of
                             VoidProxy -> (\xs -> (init xs, last xs)) <$> some (pCode eps env' VoidProxy)
                             _ -> manyTill_ (pCode eps env' VoidProxy) (try (pCode eps env' t))
          -- peek at the next token, which should be a dedent; we should have parsed the
          -- remainder of the current scope's block.
          lookAhead (tok_ Dedent)
          pure (Fix (Let pf' name v t (Fix (Block t body final))))

-- | Parse embedded effect languages
pEffects :: forall effs env t
          . EffectParsers effs
          ->  EnvironmentProxy env -> ScalarProxy t -> Parser (Code effs env t)
pEffects (EP eps) env t = go eps
  where
    go :: forall effs'. EffectParsers_ effs' effs -> Parser (Code effs env t)
    go NoEffs = mzero
    go (ParseEff (EffectParser eff p) etc) =
      withEnvironment env (Fix . Effect eff Proxy t <$> p env t) <|> go etc

-- | Parse a value with an unknown type
parseSomeValue :: forall env. EnvironmentProxy env -> [Token] -> Parser (SomeValue env)
parseSomeValue env toks =
  let parseValueAt :: ScalarProxy t -> Parser (SomeValue env)
      parseValueAt t = SomeValue t <$> nest (parseValueFromTokens env t toks)
  in  parseValueAt BooleanProxy
  <|> parseValueAt IntegerProxy
  <|> parseValueAt RealProxy
  <?> "value of unknown type"

data SomeValue env where
  SomeValue :: forall env t. ScalarProxy t -> Value env t -> SomeValue env
