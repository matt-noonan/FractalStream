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
  , uParseCode
  , uCode
  ) where

import Prelude hiding (words)
import qualified Prelude as Prelude

import Language.Type
import Language.Parser
import Language.Value
import Language.Value.Parser
import Language.Code
import qualified Language.Untyped.Code as U
import qualified Data.Recursive as U

import GHC.TypeLits
import Data.Type.Equality ((:~:)(..))

parseCode :: forall effs env t
           . EffectParsers effs
          -> EnvironmentProxy env
          -> TypeProxy t
          -> String
          -> Either (Int, BadParse) (Code effs env t)
parseCode eps env t input
  = parse (pCode eps env t <* eof) (tokenizeWithIndentation input)

uParseCode :: String
          -> Either (Int, BadParse) U.Code
uParseCode = parse (uCode <* eof) . tokenizeWithIndentation

-- | The code grammar is a tiny bit monadic, because the allowed parses
-- depend on which variables are in scope, and this can change as new 'var'
-- bindings are introduced. That's enough to prevent the use of a pure
-- applicative parser like Text.Earley, so we'll use Megaparsec's monadic
-- parsing here.
pCode :: forall effs env t
       . EffectParsers effs
      -> EnvironmentProxy env
      -> TypeProxy t
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
          ->  EnvironmentProxy env -> TypeProxy t -> Parser (Code effs env t)
pBlock eps env = \case
  VoidType -> dbg "void block" $ do
    tok_ Indent
    body <- some (pCode eps env VoidType)
    tok_ Dedent
    withEnvironment env (pure (Fix (Block VoidType (init body) (last body))))
  t -> dbg ("pBlock @" ++ showType t) $ do
    tok_ Indent
    -- This is kind of stupid
    (body, final) <- manyTill_ (pCode eps env VoidType) (try (pCode eps env t))
    tok_ Dedent
    withEnvironment env (pure (Fix (Block t body final)))

-- | Parse a single line; in practice, this is more complex because
-- let-bindings are syntactically a single line, but they introduce a
-- variable that is scoped over the remainder of the current block.
pLine :: EffectParsers effs
          -> EnvironmentProxy env -> TypeProxy t -> Parser (Code effs env t)
pLine eps env = \case
  VoidType -> try (pAnyLine eps env VoidType) <|> pVoidLine eps env
  t         -> try (pAnyLine eps env t)         <|> pNonVoidLine eps env t

pVoidLine :: EffectParsers effs
          ->  EnvironmentProxy env -> Parser (Code effs env 'VoidT)
pVoidLine eps env
  = dbg "void line" ( pLoop eps env
  <|> pSet  eps env
  <|> pPass     env
  <?> "statement")

pNonVoidLine :: EffectParsers effs
          -> EnvironmentProxy env -> TypeProxy t -> Parser (Code effs env t)
pNonVoidLine _eps env t
  = dbg "non-void line" ( pPure env t
  <?> ("statement of type " <> showType t))

pAnyLine :: EffectParsers effs
          ->  EnvironmentProxy env -> TypeProxy t -> Parser (Code effs env t)
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
            -> TypeProxy t
            -> Parser (Code effs env t)
pIfThenElse eps env t = dbg "if/then/else statement" $ withEnvironment env $ do
  tok_ If
  (toks, _) <- manyTill_ anyToken (satisfy (== Then))
  eol
  cond <- nest (parseValueFromTokens env BooleanType toks)
  yes <- pBlock eps env t
  no <- case t of
    VoidType -> try (tok_ Else >> pIfThenElse eps env t)
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
       . EnvironmentProxy env -> TypeProxy t -> Parser (Code effs env t)
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
  body <- some (pCode @effs env VoidType)
  tok_ Dedent
-}
  tok_ (Identifier "loop") >> eol
  withEnvironment env (Fix . DoWhile <$> pBlock eps env BooleanType)

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
     -> TypeProxy t
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
          -> TypeProxy t
          -> NameIsAbsent name env
          -> Proxy name
          -> TypeProxy ty
          -> Parser (Code effs env t)
pVarValue eps env t pf name vt = do
  tok_ (Identifier "to")
  toks <- manyTill anyToken eol
  v <- nest (parseValueFromTokens env vt toks)
  withEnvironment env $ withKnownType vt $ recallIsAbsent pf $ do
    let pf' = bindName name vt pf
        env' = BindingProxy name vt env
    (body, final) <- case t of
      VoidType -> (\xs -> (init xs, last xs))
                     <$> some (pCode eps env' VoidType)
      _ -> manyTill_ (pCode eps env' VoidType) (try (pCode eps env' t))
    -- peek at the next token, which should be a dedent; we should have parsed the
    -- remainder of the current scope's block.
    lookAhead (tok_ Dedent)
    pure (Fix (Let pf' name vt v t (Fix (Block t body final))))

pVarCode  :: forall effs env t ty name
           . KnownSymbol name
          => EffectParsers effs
          -> EnvironmentProxy env
          -> TypeProxy t
          -> NameIsAbsent name env
          -> Proxy name
          -> TypeProxy ty
          -> Parser (Code effs env t)
pVarCode eps env t pf name vt = do
  tok_ LeftArrow
  c <- pCode eps env vt
  many eol
  withEnvironment env $ withKnownType vt $ recallIsAbsent pf $ do
    let pf' = bindName name vt pf
        env' = BindingProxy name vt env
    (body, final) <- case t of
      VoidType -> (\xs -> (init xs, last xs))
                     <$> some (pCode eps env' VoidType)
      _ -> manyTill_ (pCode eps env' VoidType) (try (pCode eps env' t))
    -- peek at the next token, which should be a dedent; we should have parsed the
    -- remainder of the current scope's block.
    lookAhead (tok_ Dedent)
    pure (Fix (LetBind pf' name vt c t (Fix (Block t body final))))


-- | Parse type name
pTypeName :: Parser SomeType
pTypeName
  =   (SomeType RealType    <$ tok_ (Identifier "R"))
  <|> (SomeType IntegerType <$ tok_ (Identifier "Z"))
  <|> (SomeType ComplexType <$ tok_ (Identifier "C"))
  <|> (SomeType ColorType   <$ tok_ (Identifier "Color"))
  <?> "type"

-- | Parse embedded effect languages
pEffects :: forall effs env t
          . EffectParsers effs
         -> EnvironmentProxy env
         -> TypeProxy t
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

uCode :: Parser U.Code
uCode = U.Fix <$> (uBlock <|> uLine <?> "code")
  where
    value = untypedValue_
    word = tok_ . Identifier
    words = mapM_ word . Prelude.words

    blockConcat (U.Block xs) (U.Block ys) = U.Block (xs ++ ys)
    blockConcat (U.Block xs)  y           = U.Block (xs ++ [U.Fix y])
    blockConcat  x           (U.Block ys) = U.Block (U.Fix x : ys)
    blockConcat  x            y           = U.Block [U.Fix x, U.Fix y]

    ident = do
      Identifier n <- satisfy (\case { (Identifier _) -> True; _ -> False })
      pure n

    uLine
      =   uLoop
      <|> uSet
      <|> uPass
      <|> uIfThenElse
      <|> uDraw
      <|> uOutput
      <|> uList
      <|> (U.Pure <$> value)
      <?> "line of code"

    uBlock = (do
      tok_ Indent
      body <- some uCode
      tok_ Dedent
      pure (U.Block body)) <?> "block of code"

    uPass = (do
      word "pass"
      pure (U.Block [])) <?> "pass"

    uLoop = (do
      word "repeat" >> eol
      body <- uBlock
      test <- word "while" *> value
      pure (U.DoWhile (U.Fix (body `blockConcat` U.Pure test))))
      <?> "repeat"

    uSet = do
      word "set"
      n <- ident
      (    (word "to" *> (U.Set n <$> (value <* eol)))
        <|> (tok_ LeftArrow *> (U.SetBind n <$> uCode))
        <?> "binding style")

    uIfThenElse = do
      tok_ If
      cond <- value <* (tok_ Then >> eol)
      yes  <- U.Fix <$> uBlock
      no   <- (tok_ Else >> ((eol >> U.Fix <$> uBlock)
                         <|> (U.Fix <$> uIfThenElse)))
           <|> pure (U.Fix $ U.Block [])
      pure (U.IfThenElse cond yes no)

    -- Draw commands
    uDraw
      =   (word "draw" *> drawCmd)
      <|> (word "use"  *> useCmd)
      <|> (U.Clear <$ word "erase")
      <?> "drawing command"

    drawCmd
      =   (do
            word "filled"
            (    (U.DrawCircle True <$> (words "circle at" *> value)
                  <*> (words "with radius" *> value))
             <|> (U.DrawRect True <$> (words "rectangle from" *> value)
                  <*> (word "to" *> value))))
      <|> (U.DrawCircle False <$> (words "circle at" *> value)
            <*> (words "with radius" *> value))
      <|> (U.DrawRect False <$> (words "rectangle from" *> value)
            <*> (word "to" *> value))
      <|> (U.DrawLine <$> (words "line from" *> value)
            <*> (word "to" *> value))
      <|> (U.DrawPoint <$> (words "point at" *> value))
      <?> "draw command"

    useCmd = do
      c <- value
      word "for"
      ( (word "fill" $> U.SetFill c) <|> (word "stroke" $> U.SetStroke c) )

    -- Output commands
    uOutput = do
      v <- word "output" *> value
      n <- word "to" *> ident
      pure (U.Output n v)

    -- List commands
    uList
      =   (word "insert" *> insertCmd)
      <|> (word "with" *> lookupCmd)
      <|> (word "remove" *> removeCmd)
      <|> (words "for each" *> forEachCmd)

    insertCmd = do
      v <- value
      words "at start of"
      U.Insert <$> ident <*> pure v

    lookupCmd = do
      word "first"
      item <- ident <* word "matching"
      test <- value <* word "in"
      list <- ident <* eol
      yes <- uBlock
      -- Parse an else block, or else make a default no-op else case
      no <- (tok_ Else >> eol >> uBlock) <|> pure (U.Block [])
      pure (U.Lookup list item test (U.Fix yes) (U.Fix no))

    removeCmd =   (try (words "all items from") >> (U.ClearList <$> ident))
              <|> (do
                      item <- word "each" *> ident
                      test <- word "matching" *> value
                      list <- word "from" *> ident
                      pure (U.Remove list item test))

    forEachCmd = do
      item <- ident <* word "in"
      list <- ident <* eol
      U.ForEach list item . U.Fix <$> uBlock
