module Language.Code.Parser
  ( parseCode
  ) where

import Language.Type
import Language.Parser
import Language.Value
import Language.Value.Parser
import Language.Code

import Control.Monad
import GHC.TypeLits


parseCode :: forall eff env t
           . EnvironmentProxy env
          -> ScalarProxy t
          -> String
          -> Either (Int, BadParse) (Code eff env t)
parseCode env t input = parse (pCode env t) (tokenizeWithIndentation input)

-- | The code grammar is a tiny bit monadic, because the allowed parses
-- depend on which variables are in scope, and this can change as new 'var'
-- bindings are introduced. That's enough to prevent the use of a pure
-- applicative parser like Text.Earley, so we'll use Megaparsec's monadic
-- parsing here. The code grammar is simple enough that we can do this fairly
-- easily.

pCode :: EnvironmentProxy env -> ScalarProxy t -> Parser (Code eff env t)
pCode env t = pBlock env t <|> pLine env t

eol :: Parser ()
eol = eof <|> tok_ Newline

pBlock :: EnvironmentProxy env -> ScalarProxy t -> Parser (Code eff env t)
pBlock env = \case
  VoidProxy -> do
    tok_ Indent
    body <- some (pCode env VoidProxy)
    tok_ Dedent
    withEnvironment env (pure (Fix (Block VoidProxy (init body) (last body))))
  t -> do
    tok_ Indent
    -- This is kind of stupid
    (body, final) <- manyTill_ (pCode env VoidProxy) (try (pCode env t))
    tok_ Dedent
    withEnvironment env (pure (Fix (Block t body final)))

pLine :: EnvironmentProxy env -> ScalarProxy t -> Parser (Code eff env t)
pLine env = \case
  VoidProxy -> try (pVoidLine env)      <|> pAnyLine env VoidProxy
  t         -> try (pNonVoidLine env t) <|> pAnyLine env t

pVoidLine :: EnvironmentProxy env -> Parser (Code eff env 'VoidT)
pVoidLine env
  =   pLoop env
  <|> pSet  env
  <|> pPass env
  <?> "statement"

pNonVoidLine :: EnvironmentProxy env -> ScalarProxy t -> Parser (Code eff env t)
pNonVoidLine env t
  =   pPure env t
  <?> ("statement of type " <> showType t)

pAnyLine :: EnvironmentProxy env -> ScalarProxy t -> Parser (Code eff env t)
pAnyLine env t
  =   pIfThenElse env t
  <|> pVar env t

pIfThenElse :: EnvironmentProxy env -> ScalarProxy t -> Parser (Code eff env t)
pIfThenElse env t = do
  tok_ If
  (toks, _) <- manyTill_ anyToken (satisfy (== Then))
  eol
  cond <- nest (parseValueFromTokens env BooleanProxy toks)
  yes <- pBlock env t
  tok_ Else
  eol
  no <- pBlock env t
  withEnvironment env (pure (Fix (IfThenElse t cond yes no)))

pPass :: EnvironmentProxy env -> Parser (Code eff env 'VoidT)
pPass env = do
  tok_ (Identifier "pass")
  eol
  pure (withEnvironment env (Fix NoOp))

pPure :: forall eff env t. EnvironmentProxy env -> ScalarProxy t -> Parser (Code eff env t)
pPure env t = do
  toks <- manyTill anyToken eol
  withEnvironment env (Fix . Pure <$> nest (parseValueFromTokens env t toks))

pLoop :: EnvironmentProxy env -> Parser (Code eff env 'VoidT)
pLoop env = do
  tok_ (Identifier "repeat")
  eol
  tok_ Indent
  body <- some (pCode env VoidProxy)
  tok_ Dedent

  withEnvironment env (Fix . DoWhile <$> pBlock env BooleanProxy)

pSet :: EnvironmentProxy env -> Parser (Code eff env 'VoidT)
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

pVar :: forall eff env t. EnvironmentProxy env -> ScalarProxy t -> Parser (Code eff env t)
pVar env t = withKnownType t $ do
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
                             VoidProxy -> (\xs -> (init xs, last xs)) <$> some (pCode env' VoidProxy)
                             _ -> manyTill_ (pCode env' VoidProxy) (try (pCode env' t))
          -- peek at the next token, which should be a dedent; we should have parsed the
          -- remainder of the current scope's block.
          lookAhead (tok_ Dedent)
          pure (Fix (Let pf' name v t (Fix (Block t body final))))


data SomeValue env where
  SomeValue :: forall env t. ScalarProxy t -> Value env t -> SomeValue env

parseSomeValue :: forall env. EnvironmentProxy env -> [Token] -> Parser (SomeValue env)
parseSomeValue env toks =
  let parseValueAt :: ScalarProxy t -> Parser (SomeValue env)
      parseValueAt t = SomeValue t <$> nest (parseValueFromTokens env t toks)
  in  parseValueAt BooleanProxy
  <|> parseValueAt IntegerProxy
  <|> parseValueAt RealProxy
  <?> "value of unknown type"
