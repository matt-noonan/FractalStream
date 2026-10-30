{-# language RecursiveDo, ImpredicativeTypes #-}
module Language.Value.Parser
  ( parseValue
  , parseParsedValue
  , parseInputValue
  , parseType
  , parseEnvironment
  , parseConstant
  , valueGrammar
  , valueGrammarWithFunctions
  , arithGrammar
  , atType
  , typeGrammar
  , envGrammar
  , ParsedValue
  -- * Re-exports
  , TCError
  , ParseError
  , ppFullError
  ) where

import Prelude hiding (LT)
import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Value
import Language.Value.Typecheck
import Language.Value.Syntax
import Language.Parser
import Language.Typecheck
import Language.Parser.Tokenizer
import Language.Parser.SourceRange
import Language.Value.Evaluator (evaluateInContext)

------------------------------------------------------
-- Main functions for parsing Values
------------------------------------------------------

parseType :: String -> Either ParseError SomeType
parseType = fmap (`withType` SomeType) . parse typeGrammar . tokenize

parseEnvironment :: String -> Either ParseError (Map String SomeType)
parseEnvironment = fmap (Map.fromList . map (fmap (`withType` SomeType)))
                 . parse envGrammar . tokenize

parseConstant :: forall ty
               . TypeProxy ty
              -> String
              -> Either (Either ParseError TCError) (HaskellType ty)
parseConstant ty input = withKnownType ty $
  parseValue @'[] @ty input >>= \v ->
    case sameHaskellType (typeOfValue v) ty of
      Just Refl -> Right (evaluateInContext EmptyContext v)
      Nothing   -> Left . Right $ Surprise NoSourceRange
        "this expression" (an $ typeOfValue v) (Expected $ an ty)

parseValue :: forall env ty. (KnownEnvironment env, KnownType ty)
           => String
           -> Either (Either ParseError TCError) (Value '(env, ty))
parseValue input = do
  tv <- first Left (parseParsedValue input)
  case atType tv typeProxy of TC x -> first Right x

parseParsedValue :: String -> Either ParseError ParsedValue
parseParsedValue = parse valueGrammar . tokenize

parseInputValue :: forall env ty. (KnownEnvironment env, KnownType ty)
                => String
                -> Either (Either ParseError TCError) (Value '(env, ty))
parseInputValue input = case typeProxy @ty of
  -- a text box's contents are the text itself
  TextType     -> pure (Const (Scalar TextType input))
  ListType ity -> case filter (not . (flip (elem @[]) " \t\n\r")) input of
    "" -> pure (List ity [])
    _  -> parseValue ("[" ++ input ++ "]")
  _ -> parseValue input

------------------------------------------------------
-- Type and Value grammars
------------------------------------------------------

envGrammar :: forall r. Grammar r (Prod r [(String, FSType)])
envGrammar = mdo

  toplevel <- ruleChoice
    [ pure []
    , (:) <$> oneVar <*> many (token Comma *> oneVar)
    ]

  oneVar <- rule ((,) <$> (ident <* token Colon) <*> typ)

  typ <- typeGrammar

  pure toplevel

typeGrammar :: forall r. Grammar r (Prod r FSType)
typeGrammar = mdo

  toplevel <- ruleChoice
    [ productType
    , listType
    ]

  listType <- rule
    (ListT <$> (token (Identifier "List") *> token (Identifier "of") *> listType'))

  listType' <- ruleChoice [ listType, baseType ]

  productType <- ruleChoice
    [ Pair <$> (baseType <* token (Identifier "x")) <*> productType
    , baseType
    ]

  baseType <- ruleChoice
    [ scalarType
    , (token OpenParen *> toplevel <* token CloseParen)
    ]

  scalarType <- rule $ tokenMatch $ \case
    Identifier t -> Map.lookup t types
    _ -> Nothing

  pure toplevel

valueGrammar :: forall r. Grammar r (Prod r ParsedValue)
valueGrammar = valueGrammarWithFunctions Set.empty

-- | The value grammar, also accepting calls to the named user-defined
-- functions. Those names are reserved: they can't be used as variables.
valueGrammarWithFunctions :: forall r
                           . Set String
                          -> Grammar r (Prod r ParsedValue)
valueGrammarWithFunctions funs = fst <$> valueAndArithGrammar funs

-- | A self-contained arithmetic\/complex-valued expression grammar --
-- @+ - * / ^@, unary minus, implicit-multiplication concatenation, named
-- functions and constants, and grouping parentheses\/braces at any depth
-- (so e.g. @e^(2 pi i / 5)@ still works) -- deliberately missing two
-- things 'valueGrammar' has: comparisons\/booleans (not
-- meaningful for a bare numeric value), and 'Pair's\/'List's. The latter
-- is what makes this safe to offer as an unparenthesized alternative
-- alongside a @(expr, expr)@ Cartesian-pair syntax without introducing a
-- grammar ambiguity: this whole family is closed under "never builds a
-- 'Pair'" -- its own grouping-paren alternative recurses back into this
-- same family rather than out to the shared, pair-including 'toplevel'
-- -- so @(expr, expr)@ can only ever be read as a Cartesian pair, never
-- also as this grammar's own grouped expression. Used by "Scenario"'s
-- point syntax, where @(x, y)@ is Cartesian and a bare complex literal
-- like @1 + 2i@ needs to parse unambiguously as a single value.
--
-- This necessarily duplicates the handful of precedence levels between
-- 'valueGrammar'\'s @arith@ and @atom@ productions (see the @*Np@ rules
-- below, \"no parens\"), since Earley rules are nominal -- there's no way
-- to parameterize an existing rule over which lower rule it bottoms out
-- at. 'funAp' and 'simpleAtom' are reused unchanged, since neither can
-- ever start with a bare @(@.
arithGrammar :: forall r. Grammar r (Prod r ParsedValue)
arithGrammar = snd <$> valueAndArithGrammar Set.empty

valueAndArithGrammar :: forall r
                       . Set String
                      -> Grammar r (Prod r ParsedValue, Prod r ParsedValue)
valueAndArithGrammar funs = mdo

  let toplevel = cast

  cast <- ruleChoice
    [ node (ECast <$> ite <*> (token Colon *> typ))
    , ite
    ]

  ite <- ruleChoice
    [ node (EITE <$> (token If *> scalar)
                             <*> (token Then *> scalar)
                             <*> (token Else *> ite))
    , scalar
    ]

  scalar <- ruleChoice [ list, pair ]

  -- One item in a comma-separated list, like text(a, b, c) or [a, b, c]:
  -- any expression except an unparenthesized pair, which would make
  -- `a, b, c` ambiguous.
  argument <- ruleChoice
    [ node (ECast <$> argumentIte <*> (token Colon *> typ))
    , argumentIte
    ]

  argumentIte <- ruleChoice
    [ node (EITE <$> (token If *> scalar)
                 <*> (token Then *> scalar)
                 <*> (token Else *> argumentIte))
    , list
    , pOr
    ]

  list <- rule (node (EList <$> (token OpenBracket *> list' <* token CloseBracket)))

  list' <- ruleChoice
    [ (:) <$> argument <*> many (token Comma *> argument)
    , pure []
    ]

  pair <- ruleChoice
    [ node (EPair <$> (ite <* token Comma) <*> pair)
    , pOr
    ]

  pOr <- ruleChoice
    [ node (EBin OpOr <$> (pAnd <* token Or_) <*> pOr <?> "disjunction")
    , pAnd
    ]

  pAnd <- ruleChoice
    [ node (EBin OpAnd <$> (pNot <* token And_) <*> pAnd <?> "conjunction")
    , pNot
    ]

  pNot <- ruleChoice
    [ node (EUn OpNot <$> (token Not_ *> arith))
    , comparison
    ]

  comparison <- ruleChoice
    [ node (EBin OpExactEq <$> (arith <* token ExactlyEqual) <*> arith)
    , node (EBin OpApproxEq <$> (arith <* token Equal) <*> arith)
    , node (EBin OpApproxNEq <$> (arith <* token NotEqual) <*> arith)
    , node (EBin OpLt  <$> (arith <* token LessThan) <*> arith)
    , node (EBin OpGt  <$> (arith <* token GreaterThan) <*> arith)
    , node (EBin OpGe <$> (arith <* token GreaterThanOrEqual) <*> arith)
    , node (EBin OpLe <$> (arith <* token LessThanOrEqual) <*> arith)
    , node (EUn OpEscapes <$> (arith <* token Escapes))
    , node (EUn OpVanishes <$> (arith <* token Vanishes))
    , arith
    ]

  let arith = addOrSub

  addOrSub <- ruleChoice
    [ node (EBin OpAdd <$> (addOrSub <* token Plus)  <*> mulOrDiv)
    , node (EBin OpSub <$> (addOrSub <* token Minus) <*> mulOrDiv)
    , mulOrDiv
    ]

  -- List indexing (`@`, and cyclic `@@`) binds like multiplication, so
  -- `xs @ k = 3` and `xs @ k + 1` index first.
  mulOrDiv <- ruleChoice
    [ node (EBin OpMul <$> (mulOrDiv <* token Times) <*> negated)
    , node (EIndex False <$> (mulOrDiv <* token At) <*> negated)
    , node (EIndex True  <$> (mulOrDiv <* token AtAt) <*> negated)
    , node (EBin OpMul <$> concatenatedFunAps <*> funAp)
    , node (EBin OpMul <$> concatenatedAtoms <*> noFunPower)
    , node (EUn OpNeg <$> (token Minus *> concatenatedAtoms'))
    , node (EBin OpDiv <$> (mulOrDiv <* token Divide) <*> negated)
    , node (EBin OpIDiv <$> (mulOrDiv <* token IntegerDivide) <*> negated)
    , negated
    ]

  concatenatedAtoms' <- rule $ node
    (EBin OpMul <$> concatenatedAtoms <*> noFunPower)

  concatenatedAtoms <- ruleChoice
    [ node (EBin OpMul <$> concatenatedAtoms <*> noFunPower)
    , noFunPower
    , node (funAp $> EAdvice "To avoid ambiguity when using implicit multiplication, functions must go to the left of other values")
    ]

  concatenatedFunAps <- ruleChoice
    [ node (EBin OpMul <$> concatenatedFunAps <*> funAp)
    , node (EBin OpMul <$> concatenatedAtoms <*> atom)
    , funAp
    , atom
    ]

  funAp <- ruleChoice
    [ node (EApp . fst
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n commonFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , node (EApp . fst
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n realFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , node (EApp . fst
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n roundingFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , node (EApp . fst
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n complexFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)
    , node (ECall
        <$> funName
        <*> ((:[]) <$> atomOrFunAp))
    , node (EDerivCall
        <$> funName
        <*> (length <$> some (token Prime))
        <*> ((:[]) <$> atomOrFunAp))
    , node (EBin OpMod
        <$> (token (Identifier "mod") *> token OpenParen *> arith)
        <*> (token Comma *> arith <* token CloseParen))
    ]

  negated <- ruleChoice
    [ node (EUn OpNeg <$> (token Minus *> negated))
    , power
    ]

  power <- ruleChoice
    [ node (EBin OpPow <$> (atomOrFunAp <* token Caret) <*> power)
    , atomOrFunAp
    ]

  noFunPower <- ruleChoice
    [ node (EBin OpPow <$> (atom <* token Caret) <*> power)
    , atom
    ]

  atomOrFunAp <- ruleChoice [funAp, atom]

  funName <- rule $
    tokenMatch (\case { Identifier n | Set.member n funs -> Just n; _ -> Nothing })

  atom <- ruleChoice
    [ simpleAtom
    , token OpenParen   *> toplevel <* token CloseParen
    , token OpenBrace   *> toplevel <* token CloseBrace
    ]

  simpleAtom <- ruleChoice
    [ node (literal "`true`"  BooleanType True  <$ token True_)
    , node (literal "`false`" BooleanType False <$ token False_)
    , node (literal "𝑖" ComplexType (0.0 :+ 1.0) <$ token I)
    , node (literal "𝑒" RealType (exp 1.0) <$ token Euler)
    , node (literal "π" RealType pi <$ token Pi)
    , node (literal "text" TextType <$> quoted)
    , node (EVar internalIterations <$ token Iterations)
    , node (EVar internalStuck <$ token Stuck)
    , node (
        (\n -> literal (show n) IntegerType (fromIntegral n))
        <$> tokenMatch (\case { NumberI n -> Just n; _ -> Nothing }))
    , node (
        (\n -> literal (show n) RealType n)
        <$> tokenMatch (\case { NumberF n -> Just n; _ -> Nothing }))
    , node (EUn OpAbs <$> (token Bar *> arith <* token Bar))


    , textConcat
    , var
    , color
    , listOp
    , diff
    ]

  quoted <- rule $ tokenMatch (\case { Quoted txt -> Just txt; _ -> Nothing })

  var <- rule $ node (
    fmap EVar . tokenMatch $ \case
        Identifier n | not (reserved n) -> Just n;
        _ -> Nothing)

  color <- ruleChoice
    [ node (
        fmap (\(n,c) -> literal n ColorType c) . tokenMatch $ \case
            Identifier n -> ("`" ++ n ++ "`",) <$> Map.lookup n colors
            _ -> Nothing)

    , node (EUn OpDark <$> (token (Identifier "dark") *> atom))
    , node (EUn OpLight <$> (token (Identifier "light") *> atom))
    , node (EUn OpInvert <$> (token (Identifier "invert") *> atom))
    , node (
        EColor3 OpBlend <$> (token (Identifier "blend") *> (token OpenParen *> arith))
                <*> (token Comma *> atom)
                <*> (token Comma *> atom <* token CloseParen))
    , node (
        EColor3 OpCycle <$> (token (Identifier "cycle") *> (token OpenParen *> arith))
                <*> (token Comma *> atom)
                <*> (token Comma *> atom <* token CloseParen))
    , node (
        EApp <$> tokenMatch (\case { Identifier n | Map.member n wheels -> Just n; _ -> Nothing })
             <*> (token OpenParen *> arith <* token CloseParen))
    , node (
        EColor3 OpRGB <$> (token (Identifier "rgb") *> (token OpenParen *> arith))
              <*> (token Comma *> arith)
              <*> (token Comma *> arith <* token CloseParen))
    ]

  textConcat <- rule $ node $
    EText <$> ((:) <$> (token TextKeyword *> token OpenParen *> argument)
                    <*> (some (token Comma *> argument) <* token CloseParen))

  listOp <- ruleChoice
    [ node (EJoin <$>
             ((:) <$> (token JoinKeyword *> token OpenParen *> argument)
                  <*> (some (token Comma *> argument) <* token CloseParen)))
    , node (EAppend <$> (token AppendKeyword *> token OpenParen *> toplevel)
                      <*> (token Comma *> toplevel <* token CloseParen))
    , node (EPrepend <$> (token PrependKeyword *> token OpenParen *> toplevel)
                       <*> (token Comma *> toplevel <* token CloseParen))
    , node (ERemove <$> (token RemoveKeyword *> token OpenParen *> toplevel)
                      <*> (token Comma *> ident <* token RightArrow)
                      <*> (toplevel <* token CloseParen))
    , node (EFind <$> (token FindKeyword *> token OpenParen *> toplevel)
                    <*> (token Comma *> ident <* token RightArrow)
                    <*> toplevel
                    <*> (token Comma *> toplevel <* token CloseParen))
    , node (ETransform <$> (token TransformKeyword *> token OpenParen *> toplevel)
                         <*> (token Comma *> ident <* token RightArrow)
                         <*> (toplevel <* token CloseParen))
    , node (ERange <$> (token RangeKeyword *> token OpenParen *> toplevel)
                     <*> (token Comma *> toplevel <* token CloseParen))
    , node (ELength <$>
             (token LengthKeyword *> token OpenParen *> toplevel <* token CloseParen))
    ]

  diff <- rule $ node $
    EDiff <$> (token DiffKeyword *> (token OpenParen *> toplevel))
           <*> (token Comma *> toplevel <* token CloseParen)

  ------------------------------------------------------
  -- A duplicate of the precedence levels from 'arith' down to 'atom';
  -- see 'arithGrammar'. Each rule here otherwise mirrors its unsuffixed
  -- counterpart exactly, except 'atomNp': its grouping-paren\/brace
  -- alternative recurses into 'addOrSubNp' (this family) rather than the
  -- shared, pair\/list-including 'toplevel'. That keeps grouping parens
  -- usable at any depth (@e^(2 pi i / 5)@) while keeping this whole
  -- family -- including everything reachable through nested parens --
  -- closed under "never builds a 'Pair' or 'List'", since nothing in it
  -- ever recurses back out to the shared 'toplevel'\/'pair' rules.
  ------------------------------------------------------

  addOrSubNp <- ruleChoice
    [ node (EBin OpAdd <$> (addOrSubNp <* token Plus)  <*> mulOrDivNp)
    , node (EBin OpSub <$> (addOrSubNp <* token Minus) <*> mulOrDivNp)
    , mulOrDivNp
    ]

  mulOrDivNp <- ruleChoice
    [ node (EBin OpMul <$> (mulOrDivNp <* token Times) <*> negatedNp)
    , node (EBin OpMul <$> concatenatedFunApsNp <*> funAp)
    , node (EBin OpMul <$> concatenatedAtomsNp <*> noFunPowerNp)
    , node (EUn OpNeg <$> (token Minus *> concatenatedAtomsNp'))
    , node (EBin OpDiv <$> (mulOrDivNp <* token Divide) <*> negatedNp)
    , node (EBin OpIDiv <$> (mulOrDivNp <* token IntegerDivide) <*> negatedNp)
    , negatedNp
    ]

  concatenatedAtomsNp' <- rule $ node
    (EBin OpMul <$> concatenatedAtomsNp <*> noFunPowerNp)

  concatenatedAtomsNp <- ruleChoice
    [ node (EBin OpMul <$> concatenatedAtomsNp <*> noFunPowerNp)
    , noFunPowerNp
    , node (funAp $> EAdvice "To avoid ambiguity when using implicit multiplication, functions must go to the left of other values")
    ]

  concatenatedFunApsNp <- ruleChoice
    [ node (EBin OpMul <$> concatenatedFunApsNp <*> funAp)
    , node (EBin OpMul <$> concatenatedAtomsNp <*> atomNp)
    , funAp
    , atomNp
    ]

  negatedNp <- ruleChoice
    [ node (EUn OpNeg <$> (token Minus *> negatedNp))
    , powerNp
    ]

  powerNp <- ruleChoice
    [ node (EBin OpPow <$> (atomOrFunApNp <* token Caret) <*> powerNp)
    , atomOrFunApNp
    ]

  noFunPowerNp <- ruleChoice
    [ node (EBin OpPow <$> (atomNp <* token Caret) <*> powerNp)
    , atomNp
    ]

  atomOrFunApNp <- ruleChoice [funAp, atomNp]

  atomNp <- ruleChoice
    [ simpleAtom
    , token OpenParen *> addOrSubNp <* token CloseParen
    , token OpenBrace *> addOrSubNp <* token CloseBrace
    ]

  typ <- typeGrammar

  let reserved = (`Set.member` reservedWords)
      reservedWords = Set.fromList
        [ "dark", "light", "invert", "blend", "cycle", "rainbow", "arenberg"
        , "romao", "bamo", "broco", "corko", "viko"
        , "ice", "fire", "rose", "wheat", "forest", "ocean"
        , "winter", "spring", "summer", "fall"
        , "rgb", "mod", "diff"]
        `Set.union` Map.keysSet colors
        `Set.union` Map.keysSet commonFunctions
        `Set.union` Map.keysSet realFunctions
        `Set.union` Map.keysSet complexFunctions
        `Set.union` Map.keysSet roundingFunctions
        `Set.union` funs

  pure (toplevel, addOrSubNp)

node :: Prod r (ExprF ParsedValue) -> Prod r ParsedValue
node = withSourceRange . fmap (flip mkExpr)

literal :: String -> TypeProxy t -> HaskellType t -> ExprF ParsedValue
literal d ty v = ELit d (SomeHaskellType ty v)
