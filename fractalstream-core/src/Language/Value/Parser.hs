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
  , argumentGrammarWithFunctions
  , arithGrammar
  , atType
  , typeGrammar
  , envGrammar
  , ParsedValue
  -- * Re-exports
  , TCError
  , ParseError
  , ppFullError
  -- * The standard library
  , stdlibDefinitions
  , stdlibBlockFunctions
  , stdlibInternalNames
  , stdlibFuns
  , expandStdlibCalls
  ) where

import Language.Table (isTableElementType)
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
import Language.Stdlib (isStdlibFunction, stdlibName, stdlibSource, fromStdlibName)
import Language.Code.Typecheck.Functions (Funs, FunDef(..), expandCalls, rename)
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
  case expandStdlibCalls tv >>= (`atType` typeProxy) of TC x -> first Right x

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
-- The standard library
------------------------------------------------------

-- | The standard library's functions with a single expression for a body
-- (see "Language.Stdlib"): each function's name (as scripts write it),
-- parameters, and body. These are the ones a value outside any script's
-- code can call (see 'expandStdlibCalls'); the standard library's
-- functions with a body ('stdlibBlockFunctions') are only for scripts (see
-- 'Language.Code.Parser.parseStdlib').
--
-- The standard library is parsed as no script's own: so where it uses
-- one of its own names, it means its own definition. Its @internal_@
-- names are the values no script can name ('stdlibInternalNames').
--
-- Each definition's body has no source ranges (which would point into the
-- standard library's source, not the script's): so where a script calls
-- one, anything said about its body is said about the call (see
-- 'Language.Code.Typecheck.Functions.expandCalls').
stdlibDefinitions :: [(String, [String], ParsedValue)]
stdlibDefinitions = [ d | Right d <- stdlibItems ]

-- | The names (as scripts write them) of the standard library's functions
-- with a body, @function f(x):@.
stdlibBlockFunctions :: Set String
stdlibBlockFunctions = Set.fromList [ f | Left f <- stdlibItems ]

-- | The standard library's definitions, in order: a function with a body
-- (by its name), or one with a single expression for a body.
stdlibItems :: [Either String (String, [String], ParsedValue)]
stdlibItems = map item (topLevelLines (tokenizeWithIndentation stdlibSource))
  where
    item = \case
      SRToken (Identifier "function") _ : SRToken (Identifier f) _ : _ -> Left f
      line -> Right (definition line)

    definition line = case break ((== ColonEquals) . baseToken) line of
      (SRToken (Identifier f) _ : header, _ : body)
        | Just params <- parameters (map baseToken header)
        , Right e <- parse (valueGrammarWithFunctions Set.empty Set.empty) body
        -> (f, params, rename internals (NoSourceRange <$ e))
      _ -> error ("internal error: in the standard library, this isn't a definition \
                  \like `f(x) := ...` or `function f(x):`: " ++ unwords (map (show . baseToken) line))

    parameters = \case
      OpenParen : rest -> go rest
      _ -> Nothing
      where go = \case
              [Identifier p, CloseParen] -> Just [p]
              Identifier p : Comma : rest -> (p :) <$> go rest
              _ -> Nothing

    internals = Map.fromList stdlibInternalNames

-- | The lines at the top level of some code (each with any block that
-- follows it, as a function's body follows its header).
topLevelLines :: [SRToken] -> [[SRToken]]
topLevelLines toks0 = go (0 :: Int) [] (unwrap toks0)
  where
    -- (the whole code is one indented block)
    unwrap = \case
      SRToken Indent _ : rest | not (null rest), baseToken (last rest) == Dedent -> init rest
      toks -> toks
    go depth line = \case
      [] -> emit line []
      t : rest -> case baseToken t of
        -- (a header's line ends before the block that follows it, as in
        -- @function f(x):@, Newline, Indent, ...: that block is part of
        -- the same top-level line)
        Newline | depth == 0, not (startsBlock rest) -> emit line (go 0 [] rest)
        Indent -> go (depth + 1) (t : line) rest
        Dedent | depth == 1 -> emit (t : line) (go 0 [] rest)
               | otherwise -> go (depth - 1) (t : line) rest
        _ -> go depth (t : line) rest
    emit line more = if null line then more else reverse line : more
    startsBlock = \case
      t : _ -> baseToken t == Indent
      [] -> False

-- | The values no script can name, under the names the standard library
-- uses for them.
stdlibInternalNames :: [(String, String)]
stdlibInternalNames =
  [ ("internal_view_kind", viewKindName)
  , ("internal_view_pixel_size", viewPixelSizeName) ]
  ++ zip [ "internal_view_" ++ n | n <- ["a_re", "a_im", "c_re", "c_im"] ] viewMobiusNames

-- | The standard library's functions, for expanding calls to them in a
-- value that isn't part of a script's code (see 'expandStdlibCalls').
stdlibFuns :: Funs
stdlibFuns = foldl' define Map.empty stdlibDefinitions
  where define funs (f, params, body) = Map.insert (stdlibName f) (FunDef params body funs) funs

-- | Expand the calls to the standard library's functions in a value that
-- isn't part of a script's code, like a configuration value's default.
-- (Its functions with a body can't be called there: only in a script's
-- code, where their calls are lifted into statements of their own.)
expandStdlibCalls :: ParsedValue -> TC ParsedValue
expandStdlibCalls e = do
  forM_ (calls e) $ \(sr, f) -> case fromStdlibName f of
    Just n | Set.member n stdlibBlockFunctions -> advise sr (blockFunctionCallAdvice f)
    _ -> pure ()
  expandCalls stdlibFuns e
  where
    calls (Syn sr n) = here ++ foldMap calls n
      where here = case n of
              ECall f _ -> [(sr, f)]
              EDerivCall f _ _ -> [(sr, f)]
              _ -> []

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
    , tableType
    ]

  -- `Table 2 of C`: a table of scalars, or tuples of them (`Table 2 of
  -- C x Z`, or `Table 2 of (C x Z)`), with some number of dimensions
  tableType <- rule
    (TableT <$> (token (Identifier "Table") *> dimension) <*> (token (Identifier "of") *> tableElementType))

  tableElementType <- ruleChoice
    [ Pair <$> (tableScalar <* (token (Identifier "x") <|> token Times)) <*> tableElementType
    , tableScalar
    ]

  tableScalar <- ruleChoice
    [ tokenMatch $ \case
        Identifier t | Just ft <- Map.lookup t types, isTableElementType ft -> Just ft
        _ -> Nothing
    , token OpenParen *> tableElementType <* token CloseParen
    ]

  dimension <- rule $ tokenMatch $ \case
    NumberI n | n >= 1 -> Just (fromIntegral n)
    _ -> Nothing

  listType <- rule
    (ListT <$> (token (Identifier "List") *> token (Identifier "of") *> listType'))

  listType' <- ruleChoice [ listType, baseType ]

  productType <- ruleChoice
    -- `R x C`, or `R × C` (× is the times sign, which `*` also is)
    [ Pair <$> (baseType <* (token (Identifier "x") <|> token Times)) <*> productType
    , baseType
    ]

  baseType <- ruleChoice
    [ scalarType
    , (token OpenParen *> toplevel <* token CloseParen)
    ]

  scalarType <- ruleChoice
    [ tokenMatch $ \case
        Identifier t -> Map.lookup t types
        _ -> Nothing
    -- `ℙ¹`, or `P^1` (the tokenizer makes a superscript a power)
    , ProjectiveT <$ (tokenMatch (\case { Identifier t | t `elem` ["ℙ", "P"] -> Just (); _ -> Nothing })
                      *> token Caret *> token (NumberI 1))
    ]

  pure toplevel

valueGrammar :: forall r. Grammar r (Prod r ParsedValue)
valueGrammar = valueGrammarWithFunctions Set.empty Set.empty

-- | The value grammar, also accepting calls to the named user-defined
-- functions (the first set: those names are reserved, and can't be used
-- as variables), and taking the built-in names in the second set for the
-- script's own (see 'Language.Parser.ownBuiltinNames'): they're
-- variables there, not the built-ins.
valueGrammarWithFunctions :: forall r
                           . Set String
                          -> Set String
                          -> Grammar r (Prod r ParsedValue)
valueGrammarWithFunctions funs own = (\(v, _, _) -> v) <$> valueAndArithGrammar funs own

-- | Like 'valueGrammarWithFunctions', but without unparenthesized pairs
-- (@a, b@) at the top level: for one of several comma-separated
-- arguments.
argumentGrammarWithFunctions :: forall r
                              . Set String
                             -> Set String
                             -> Grammar r (Prod r ParsedValue)
argumentGrammarWithFunctions funs own = (\(_, _, a) -> a) <$> valueAndArithGrammar funs own

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
arithGrammar = (\(_, a, _) -> a) <$> valueAndArithGrammar Set.empty Set.empty

valueAndArithGrammar :: forall r
                       . Set String
                      -> Set String
                      -> Grammar r (Prod r ParsedValue, Prod r ParsedValue, Prod r ParsedValue)
valueAndArithGrammar funs own = mdo

  let -- a built-in name, unless it's the script's own
      free n = not (Set.member n own)
      builtinWord n = tokenMatch (\case { Identifier m | m == n, free m -> Just (); _ -> Nothing })

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

  -- (an item with a type, `x : R`, needs parentheses here, `[(x : R)]`,
  -- since `[z : w]` is a point of ℙ¹)
  list' <- ruleChoice
    [ (:) <$> argumentIte <*> many (token Comma *> argumentIte)
    , pure []
    ]

  -- `[a : b]`: a point of ℙ¹, by its homogeneous coordinates (more of
  -- them are for ℙⁿ, which typing reports as not there yet)
  homogeneous <- rule (node (EHomogeneous <$> (token OpenBracket *> ((:) <$> arith <*> some (token Colon *> arith)) <* token CloseBracket)))

  -- A pair's first part can't be an unparenthesized pair or if/then/else,
  -- so that tuples nest to the right only (`a, b, c` is `a, (b, c)`), and
  -- `if c then a else b, d` is `if c then a else (b, d)`.
  pair <- ruleChoice
    [ node (EPair <$> (pairItem <* token Comma) <*> pair)
    , pOr
    , pointIndex
    , sliceAll
    , funRef
    , lambda
    , compose
    ]

  pairItem <- ruleChoice [ list, pOr, pointIndex, sliceAll, funRef, lambda, compose ]

  -- A function given as an argument (see "Language.Code.FunctionArgs"):
  -- one of the script's, by its name alone, or one written in place,
  -- `w -> e` or `(u, v) -> e`.
  funRef <- rule (node (EFunRef <$> funName))
  lambda <- rule (node (ELambda <$> lambdaParams <*> (token RightArrow *> pOr)))

  -- `f ∘ g`, a function made of two: each a function by its name, a
  -- parameter (given a function at each call), or, in parentheses, a
  -- function written in place or another composition. `f ∘ g ∘ h` is
  -- `f ∘ (g ∘ h)`.
  compose <- rule (node (ECompose <$> composeOperand <*> (token Compose *> composeRest)))
  composeRest <- ruleChoice [compose, composeOperand]
  composeOperand <- ruleChoice
    [ funRef
    , node (EVar <$> paramName)
    -- a built-in function, `sin`, as the function `w -> sin(w)`
    , node ((\n -> ELambda [builtinParam] (Syn NoSourceRange (EApp n (Syn NoSourceRange (EVar builtinParam)))))
            <$> builtinFunName)
    , token OpenParen *> lambda <* token CloseParen
    , token OpenParen *> compose <* token CloseParen
    ]

  -- a name that may be a parameter given a function: one that isn't
  -- already a function's (see "Language.Code.FunctionArgs")
  paramName <- rule $ tokenMatch (\case
    Identifier n | not (reserved n), not (Set.member n funs)
                 , not (isStdlibFunction n && free n), not (isBuiltinFun n) -> Just n
    _ -> Nothing)
  builtinFunName <- rule $ tokenMatch (\case
    Identifier n | isBuiltinFun n -> Just n
    _ -> Nothing)
  let isBuiltinFun n = free n && any (Map.member n)
        [ Map.map (const ()) commonFunctions, Map.map (const ()) realFunctions
        , Map.map (const ()) roundingFunctions, Map.map (const ()) complexFunctions ]
      builtinParam = "[internal] built-in argument"
  lambdaParams <- ruleChoice
    [ (:[]) <$> lambdaParam
    , token OpenParen *> ((:) <$> lambdaParam <*> some (token Comma *> lambdaParam)) <* token CloseParen
    ]
  lambdaParam <- rule $ tokenMatch $ \case
    Identifier n | not (reserved n) -> Just n
    _ -> Nothing

  -- In a table's index, `point z` stands for its first two indices, and
  -- `*` for every index along a dimension: `T @ (point z, *)`.
  pointIndex <- rule (node (EPointIndex <$> (builtinWord "point" *> negated)))
  sliceAll <- rule (node (ESliceAll <$ token Times))
  -- `(point z, ...)`, for `T @ (point z, m) else v`
  pointTuple <- rule (token OpenParen *> node (EPair <$> (pointIndex <* token Comma) <*> pair) <* token CloseParen)

  -- `and`s and `or`s can each be chained, but not mixed without
  -- parentheses: `a and b or c` is accepted only to say so. (The mixed
  -- chain's first operator picks its branch, and its first switch to the
  -- other operator is where the run of the first one ends, so it parses
  -- only one way.)
  pOr <- ruleChoice
    [ orChain
    , andChain
    , node (mixedAndOr $> EAdvice "`and` and `or` can't be mixed without parentheses, \
                                  \which say which goes first: write `(a and b) or c` \
                                  \or `a and (b or c)`")
    , pNot
    ]

  orChain <- rule (node (EBin OpOr <$> (pNot <* token Or_) <*> orTail <?> "disjunction"))
  orTail <- ruleChoice [ orChain, pNot ]

  andChain <- rule (node (EBin OpAnd <$> (pNot <* token And_) <*> andTail <?> "conjunction"))
  andTail <- ruleChoice [ andChain, pNot ]

  let -- operands separated by @sep@s
      runOf' sep = pNot *> many (sep *> pNot)
      runOf op = runOf' (token op)
      anyOp = token And_ <|> token Or_
  mixedAndOr <- rule
    (   (pNot *> token And_ *> runOf And_ *> token Or_ *> runOf' anyOp)
    <|> (pNot *> token Or_ *> runOf Or_ *> token And_ *> runOf' anyOp) )

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
    , node ((\p tbl -> ETableHas tbl p) <$> (arith <* builtinWord "in") <*> arith)
    , node (EUn OpEscapes <$> (arith <* token Escapes))
    , node (EUn OpVanishes <$> (arith <* token Vanishes))
    , node (EUn OpStops <$> (arith <* token Stops))
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
    -- `T @ point z`: the cell nearest a point
    , node (EIndex False <$> (mulOrDiv <* token At) <*> pointIndex)
    -- `T @ *`: all of a 1-dimensional table, as a list
    , node (EIndex False <$> (mulOrDiv <* token At) <*> sliceAll)
    -- `T @ point z else v`, or `T @ (point z, m) else v`: the cell's
    -- value, or `v` off the table
    , node (ETablePointOr <$> (mulOrDiv <* token At) <*> ((pointIndex <|> pointTuple) <* token Else) <*> negated)
    , node (EIndex False <$> (mulOrDiv <* token At) <*> negated)
    , node (EIndex True  <$> (mulOrDiv <* token AtAt) <*> negated)
    -- (a list literal isn't an atom, so it needs its own rule)
    , node (EIndex False <$> (list <* token At) <*> negated)
    , node (EIndex True  <$> (list <* token AtAt) <*> negated)
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

  -- (what comes before a function in an implicit product can end in a
  -- power: `ω² sin(θ)`)
  concatenatedFunAps <- ruleChoice
    [ node (EBin OpMul <$> concatenatedFunAps <*> funAp)
    , node (EBin OpMul <$> concatenatedAtoms <*> noFunPower)
    , funAp
    , noFunPower
    ]

  funAp <- ruleChoice
    [ node (EApp . fst
        <$> (tokenMatch (\case { Identifier n | free n -> (n,) <$> Map.lookup n commonFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , node (EApp . fst
        <$> (tokenMatch (\case { Identifier n | free n -> (n,) <$> Map.lookup n realFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , node (EApp . fst
        <$> (tokenMatch (\case { Identifier n | free n -> (n,) <$> Map.lookup n roundingFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , node (EApp . fst
        <$> (tokenMatch (\case { Identifier n | free n -> (n,) <$> Map.lookup n complexFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)
    , node (ECall
        <$> funName
        <*> ((:[]) <$> atomOrFunAp))
    , node (EDerivCall
        <$> funName
        <*> (length <$> some (token Prime))
        <*> ((:[]) <$> atomOrFunAp))
    -- `g'(x)` for a parameter `g`, which is a function where it's given
    -- one (see "Language.Code.FunctionArgs")
    , node (EDerivCall
        <$> paramName
        <*> (length <$> some (token Prime))
        <*> ((:[]) <$> atomOrFunAp))
    -- `(f ∘ g)'(x)`
    , node (EDerivOf
        <$> (token OpenParen *> compose <* token CloseParen)
        <*> (length <$> some (token Prime))
        <*> atomOrFunAp)
    , node (EBin OpMod
        <$> (builtinWord "mod" *> token OpenParen *> argument)
        <*> (token Comma *> argument <* token CloseParen))
    ]

  negated <- ruleChoice
    [ node (EUn OpNeg <$> (token Minus *> negated))
    , power
    ]

  power <- ruleChoice
    [ node (EBin OpPow <$> (atomOrFunAp <* token Caret) <*> power)
    , atomOrFunAp
    , fraction
    ]

  noFunPower <- ruleChoice
    [ node (EBin OpPow <$> (atom <* token Caret) <*> power)
    , atom
    , fraction
    ]

  atomOrFunAp <- ruleChoice [funAp, atom]

  -- (a standard library function, unless the script defines that name
  -- itself, is the standard library's: see "Language.Stdlib")
  funName <- rule $
    tokenMatch (\case
      Identifier n | Set.member n funs -> Just n
                   | isStdlibFunction n, free n -> Just (stdlibName n)
      _ -> Nothing)

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
    , node (EUn OpPrevious <$> (token PreviousKeyword *> var))
    , node (EInfinity <$ (void (token Infinity) <|> builtinWord "infinity"))
    , homogeneous

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
            Identifier n | free n -> ("`" ++ n ++ "`",) <$> Map.lookup n colors
            _ -> Nothing)

    , node (EUn OpInvert <$> (builtinWord "invert" *> atomOrFunAp))
    -- `blend(t, c1, c2)` is c1 at t = 0 and c2 at t = 1: the surface
    -- 'OpBlend' (like 'Language.Value.Blend') takes t of its first color
    , node (
        (\t c1 c2 -> EColor3 OpBlend t c2 c1)
          <$> (builtinWord "blend" *> (token OpenParen *> argument))
          <*> (token Comma *> argument)
          <*> (token Comma *> argument <* token CloseParen))
    , node (
        EColor3 OpRGB <$> (builtinWord "rgb" *> (token OpenParen *> argument))
              <*> (token Comma *> argument)
              <*> (token Comma *> argument <* token CloseParen))
    , node (
        EColor3 OpHSV <$> (builtinWord "hsv" *> (token OpenParen *> argument))
              <*> (token Comma *> argument)
              <*> (token Comma *> argument <* token CloseParen))
    , node (
        EColor3 OpHSL <$> (builtinWord "hsl" *> (token OpenParen *> argument))
              <*> (token Comma *> argument)
              <*> (token Comma *> argument <* token CloseParen))
    -- `autocolor(n, p)` and `get_autocolor(n, p)` (typing checks that
    -- the group number is a plain number)
    , node (
        EAutocolor <$> autocolorName
                   <*> (token OpenParen *> argument)
                   <*> (token Comma *> argument <* token CloseParen))
    ]

  autocolorName <- ruleChoice
    [ mode <$ builtinWord (autocolorModeName mode) | mode <- [minBound .. maxBound] ]

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
    , node (ETableSize <$>
             (builtinWord "table_size" *> token OpenParen *> toplevel <* token CloseParen))
    , node (ETableCell <$> (builtinWord "table_cell" *> token OpenParen *> toplevel)
                       <*> (token Comma *> toplevel <* token CloseParen))
    , node (EChart <$> (builtinWord "chart" *> token OpenParen *> toplevel <* token CloseParen))
    , node (EAffine <$> (builtinWord "affine" *> token OpenParen *> pairItem)
                    <*> (token Comma *> pairItem <* token CloseParen))
    , node (EChordal <$> (builtinWord "chordal" *> token OpenParen *> pairItem)
                     <*> (token Comma *> pairItem <* token CloseParen))
    , node ((\tbl ix -> ETableCellPoint tbl [ix])
             <$> (builtinWord "table_point" *> token OpenParen *> toplevel)
             <*> (token Comma *> toplevel <* token CloseParen))
    ]

  -- `diff(e, x)`, the derivative of e with respect to x, or `diff(e, x, n)`
  -- for the n-th derivative (the same as n nested diffs), and `partial`
  -- likewise
  diff <- ruleChoice
    [ derivative EDiff (void (token DiffKeyword))
    , derivative EPartial (builtinWord "partial")
    ]
  let derivative mk word = withSourceRange $
        (\e x n sr -> iterate (Syn sr . mk x) e !! fromMaybe 1 n)
          <$> (word *> token OpenParen *> pairItem)
          <*> (token Comma *> pairItem)
          <*> optional (token Comma *> derivativeOrder)
          <* token CloseParen
  derivativeOrder <- rule $ tokenMatch $ \case
    NumberI n | n >= 1 -> Just (fromIntegral n)
    _ -> Nothing

  -- `∂f/∂z` for `partial(f, z)` and `ⅆf/ⅆz` for `diff(f, z)`; with
  -- orders, `∂²f/∂z²`, and with several variables, `∂³f/∂x∂y²` (that is,
  -- `partial(partial(f, y, 2), x)`), the orders on the bottom adding up to
  -- the order on top. A fraction like this is never the base of a power
  -- (see `power`): `(∂f/∂z)^2` is its square, so that a superscript after
  -- a variable on the bottom is always that variable's order.
  fraction <- ruleChoice
    [ fractionOf EPartial (token PartialSign) "∂"
    , fractionOf EDiff (token DiffSign) "ⅆ"
    ]
  let fractionOf mk sign symbol = withSourceRange $
        (\n f bottom sr ->
           let m = sum (map snd bottom)
           in if n == m
                then foldr (\(v, k) e -> iterate (Syn sr . mk (Syn sr (EVar v))) e !! k) f bottom
                else Syn sr (EAdvice ("In this derivative, the order on top (" ++ show n
                                      ++ ") isn't the sum of the orders on the bottom (" ++ show m
                                      ++ "). To raise a derivative to a power, put it in parentheses: (" ++ symbol
                                      ++ "f/" ++ symbol ++ "z)^2.")))
          <$> (sign *> (fromMaybe 1 <$> optional (token Caret *> derivativeOrder)))
          <*> atomOrFunAp
          <*> (token Divide *> some ((,) <$> (sign *> varName)
                                         <*> (fromMaybe 1 <$> optional (token Caret *> derivativeOrder))))
  varName <- rule $ tokenMatch $ \case
    Identifier n | not (reserved n) -> Just n
    _ -> Nothing

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

  let reserved n = Set.member n funs || ((Set.member n builtinWords || isStdlibFunction n) && free n)
      builtinWords = Set.fromList
        [ "invert", "blend", "rgb", "hsv", "hsl", "mod", "diff", "point", "table_size", "filled"
        , "table_cell", "table_point", "in", "pixels", "partial"
        , "infinity", "chart", "affine", "chordal" ]
        `Set.union` Set.fromList (map autocolorModeName [minBound .. maxBound])
        `Set.union` Map.keysSet colors
        `Set.union` Map.keysSet commonFunctions
        `Set.union` Map.keysSet realFunctions
        `Set.union` Map.keysSet complexFunctions
        `Set.union` Map.keysSet roundingFunctions

  pure (toplevel, addOrSubNp, argument)

node :: Prod r (ExprF ParsedValue) -> Prod r ParsedValue
node = withSourceRange . fmap (flip mkExpr)

literal :: String -> TypeProxy t -> HaskellType t -> ExprF ParsedValue
literal d ty v = ELit d (SomeHaskellType ty v)
