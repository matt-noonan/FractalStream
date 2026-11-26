module Language.Parser
  ( optional
  , void
  , (<|>)
  , empty
  , many
  , some
  , (<&>)
  , ($>)
  , (<$)

  , ParseError(..)
  , ppFullError

  -- * Functions that behave as the like-named ones
  -- from Text.Earley, except also track source ranges
  , token
  , tokenMatch
  , satisfy
  , ruleChoice
  , rule
  , Prod(..)
  , withSourceRange
  , (<?>)
  , parse
  , ownBuiltinNames
  , isOwnableBuiltin
  , functionValueName
  , ident
  , tok

   -- * Re-exports from Text.Earley
  , P.Grammar

  ) where

import FractalStream.Prelude
import Data.Char (isDigit)

import qualified Data.Set as Set
import qualified Text.Earley as P

import Language.Parser.Tokenizer
import qualified Language.Value.Builtins as B
import qualified Data.Map as Map
import Language.Parser.SourceRange
import Language.Stdlib (isStdlibFunction)

------------------------------------------------------
-- Versions of types and combinators from Text.Earley,
-- modified so that they also track source ranges.
------------------------------------------------------

newtype Prod r a = Prod { runProd :: P.Prod r String SRToken (SourceRange, a) }
  deriving Functor

instance Applicative (Prod r) where
  pure = Prod . pure . (NoSourceRange,)
  Prod mf <*> Prod ma = Prod $
    (\(sr, f) (sr', x) -> (sr <> sr', f x)) <$> mf <*> ma

instance Alternative (Prod r) where
  empty = Prod empty
  Prod x <|> Prod y = Prod (x <|> y)
  many (Prod p) = Prod (sequence <$> many p)
  some (Prod p) = Prod (sequence <$> some p)

instance Semigroup a => Semigroup (Prod r a) where
  Prod px <> Prod py = Prod $
    (\(sr,x) (sr',y) -> (sr <> sr', x <> y)) <$> px <*> py

instance Monoid a => Monoid (Prod r a) where
  mempty = Prod (pure (NoSourceRange, mempty))

token :: Token -> Prod r Token
token bt = satisfy (== bt)

tokenMatch :: (Token -> Maybe a) -> Prod r a
tokenMatch f = satisfy (isJust . f) <&> \t -> case f t of
    Just x  -> x
    Nothing -> error "impossible"

satisfy :: (Token -> Bool) -> Prod r Token
satisfy f = Prod ((\(SRToken bt sr) -> (sr, bt)) <$> P.satisfy (f . baseToken))

ident :: Prod r String
ident = tokenMatch $ \case
  Identifier n -> Just n
  _ -> Nothing

-- | A word, as the tokenizer gives it: an identifier, or the keyword
-- token for a word the tokenizer reads as one (like @to@).
tok :: String -> Prod r ()
tok s = tokenMatch $ \case
  Identifier n | n == s -> Just ()
  t | Just t == Map.lookup s wordlikeTokens -> Just ()
  _ -> Nothing

ruleChoice :: [Prod r a] -> P.Grammar r (Prod r a)
ruleChoice = rule . asum

rule :: Prod r a -> P.Grammar r (Prod r a)
rule = fmap Prod . P.rule . runProd

-- | A named production (used for reporting expected things).
infixr 0 <?>
(<?>) :: Prod r a -> String -> Prod r a
Prod p <?> name = Prod (p P.<?> name)

parse :: (forall r. P.Grammar r (Prod r a)) -> [SRToken] -> Either ParseError a
parse p input
  | (sr, s) : _ <- [ (sr, s) | SRToken (ScientificNotation s) sr <- input ]
  = Left (ScientificNumber sr s)
  | (sr, n) : _ <- [ (sr, n) | SRToken (NumberI n) sr <- input, n > maxIntegerLiteral ]
  = Left (IntegerTooBig sr n)
  | b : _ <- builtinBindings input
  = Left b
  | otherwise = case P.fullParses (P.parser (fmap snd . runProd <$> p)) (functionValues input) of
    ([], r)  -> Left (unfinished r)
    ([x], _) -> pure x
    (_, r)   -> Left (AmbiguousParse `onReport` r)
  where
    onReport f P.Report{..} = f (toPos position) (Set.fromList expected)
    toPos i = case mconcat (take (i + 1) (map tokenSourceRange input)) of
      NoSourceRange   -> Nothing
      SourceRange _ e -> Just e
    -- What's still being written, said so, rather than that the parser
    -- got stuck: a block with nothing in it yet (a line ending with a
    -- colon is always a block's header, so its next token has to start
    -- the block), or a line that ends early.
    unfinished r@P.Report{..}
      | sr : _ <- [ sr | (SRToken Colon sr, SRToken Newline _, next) <- triples
                       , maybe True ((/= Indent) . baseToken) next ]
      = EmptyBlock sr
      | maybe True ((`elem` [Newline, Dedent]) . baseToken) (listToMaybe (drop position input))
      = UnfinishedLine (toPos position) (unclosedOn position) (Set.fromList expected)
      | otherwise = NoParse `onReport` r
    triples = zip3 input (drop 1 input) (map Just (drop 2 input) ++ [Nothing, Nothing])
    -- (the brackets left open on the line that ends at this token)
    unclosedOn i =
      let line = reverse (takeWhile ((`notElem` [Newline, Indent, Dedent]) . baseToken)
                                    (reverse (take i input)))
          depth = sum [ case baseToken t of
                          OpenParen -> 1; OpenBracket -> 1; OpenBrace -> 1
                          CloseParen -> -1; CloseBracket -> -1; CloseBrace -> -1
                          _ -> 0 :: Int
                      | t <- line ]
      in depth > 0

-- | The variable that holds a function's value inside its body (see
-- "Language.Code.FunctionBlocks").
functionValueName :: String -> String
functionValueName f = f ++ "'s value"

-- | Inside each @function f(...):@ block, @f@ is the variable holding the
-- function's value (a function can't call itself), not the function: so
-- that @f <- f + 1@ parses, with @f@ a variable, while @f(x)@ elsewhere
-- is a call.
functionValues :: [SRToken] -> [SRToken]
functionValues = go True
  where
    go atStart = \case
      fn@(SRToken (Identifier "function") _) : name@(SRToken (Identifier f) _) : rest | atStart ->
        let (header, afterHeader) = break ((== Indent) . baseToken) rest
        in case afterHeader of
             ind : body -> let (inside, after) = within f (0 :: Int) body
                           in fn : name : header ++ ind : inside ++ go True after
             [] -> fn : name : go False rest
      t : rest -> t : go (baseToken t `elem` [Newline, Indent, Dedent]) rest
      [] -> []
    -- the function's block, up to the Dedent that ends it
    within f depth = \case
      [] -> ([], [])
      t@(SRToken here sr) : rest -> case here of
        Dedent | depth == 0 -> ([], t : rest)
        Dedent -> first (t :) (within f (depth - 1) rest)
        Indent -> first (t :) (within f (depth + 1) rest)
        Identifier n | n == f -> first (SRToken (Identifier (functionValueName f)) sr :) (within f depth rest)
        _ -> first (t :) (within f depth rest)

-- | Keywords a script uses for its own variables, functions, and code
-- blocks (@i@, @e@, @iterations@, @length@, ...), which it can't: they're
-- the tokenizer's, and mean what they mean everywhere. Other built-in
-- names can be a script's own (see 'ownBuiltinNames').
builtinBindings :: [SRToken] -> [ParseError]
builtinBindings toks =
  [ BuiltinName sr name what role | (role, (t, sr)) <- scriptBindings toks
                                  , Just (name, what, False) <- [builtin t] ]

-- | The built-in names a script (the tokens of one of its stanzas, say)
-- takes for its own variables, functions, and code blocks. Those are the
-- script's wherever it parses with them (see
-- 'Language.Code.Parser.CommonDefs'), and the built-in of that name isn't
-- available there: so a built-in added in a later version of FractalStream
-- never changes what a script that already used its name means.
ownBuiltinNames :: [SRToken] -> Set String
ownBuiltinNames toks =
  Set.fromList [ name | (_, (t, _)) <- scriptBindings toks, Just (name, _, True) <- [builtin t] ]

-- | Is this a built-in name that a script can take for its own (see
-- 'ownBuiltinNames')?
isOwnableBuiltin :: String -> Bool
isOwnableBuiltin n = case builtin (Identifier n) of
  Just (_, _, True) -> True
  _ -> False

-- | The names a script defines, with what they name, where they're
-- defined: a function's (@f(x) ≔@) or @define@'s name and parameters,
-- @x ≔@, a statement's @x ⟵@ and @x : T@, @x ⟼@, @for each x@, @with
-- first x@.
scriptBindings :: [SRToken] -> [(String, (Token, SourceRange))]
scriptBindings = go True . map (\(SRToken t sr) -> (t, sr))
  where
    go _ [] = []
    go atStart toks@((t, _) : rest) = here atStart toks ++ go (t `elem` [Newline, Indent, Dedent]) rest

    here atStart = \case
      n : rest | atStart, Just _ <- functionDefinitionBody (map fst rest) -> named "function" n ++ params rest
      (Identifier "define", _) : n : rest -> named "code block" n ++ params rest
      (Identifier "function", _) : n : rest | atStart -> named "function" n ++ params rest
      (Identifier "each", _) : n : _ -> named "variable" n
      (Identifier "first", _) : n : _ -> named "variable" n
      n : (ColonEquals, _) : _ -> named "variable" n
      n : (RightArrow, _) : _ -> named "variable" n
      n : (LeftArrow, _) : _ | atStart -> named "variable" n
      n : (Colon, _) : (next, _) : _ | atStart, next `notElem` [Newline, Indent, Dedent] -> named "variable" n
      _ -> []

    named role n = [(role, n)]

    -- a function's or `define`'s parameters: each first in its slot
    params = \case
      (OpenParen, _) : rest -> slots rest
      _ -> []
    slots toks =
      let (slot, rest) = break ((`elem` [Comma, CloseParen]) . fst) toks
      in take 1 (concatMap (named "variable") (take 1 slot)) ++ case rest of
           (Comma, _) : more -> slots more
           _ -> []

-- | A built-in name, what it is, and whether a script can take it for its
-- own (all but the tokenizer's keywords), if this token is one.
builtin :: Token -> Maybe (String, String, Bool)
builtin = \case
  I -> Just ("i", "the imaginary unit", False)
  Euler -> Just ("e", "Euler's number", False)
  Pi -> Just ("pi", "π", False)
  Iterations -> Just ("iterations", "a built-in value (the last loop's iteration count)", False)
  Stuck -> Just ("stuck", "a built-in value (whether the last loop gave up)", False)
  ClickKeyword -> Just ("click", "a built-in value (the point a tool's `on click:` is given)", False)
  DragKeyword -> Just ("drag", "a built-in value (with `drag start` and `drag end`, the points a tool's \
                               \`on drag:` is given)", False)
  Identifier n
    | Just what <- lookup n described -> Just (n, "a built-in function (" ++ what ++ ")", True)
    | n `Map.member` B.realFunctions || n `Map.member` B.commonFunctions
      || n `Map.member` B.complexFunctions || n `Map.member` B.roundingFunctions
      || n `elem` others -> Just (n, "a built-in function", True)
    | isStdlibFunction n -> Just (n, "a standard library function", True)
    | n `Map.member` B.colors -> Just (n, "a color's name", True)
    | n == "infinity" -> Just (n, "a built-in value (the point at infinity)", True)
  t | Just n <- lookup t keywordNames -> Just (n, "a built-in function", False)
  _ -> Nothing
  where
    described =
      [ ("conj", "the complex conjugate"), ("re", "the real part")
      , ("im", "the imaginary part"), ("arg", "the argument") ]
    others = [ "mod", "rgb", "hsv", "hsl", "blend", "invert"
             , "autocolor", "nearest_autocolor", "get_autocolor", "point", "table_size", "filled"
             , "table_cell", "table_point", "in", "pixels", "partial", "diff_bar", "chart", "affine", "chordal"
             , "proj1", "proj2", "proj3", "proj4" ]
    keywordNames = [ (t, n) | (n, t) <- Map.toList wordlikeTokens
                            , t `elem` [ TextKeyword, AppendKeyword, PrependKeyword, JoinKeyword
                                       , RemoveKeyword, FindKeyword, TransformKeyword, RangeKeyword
                                       , LengthKeyword, DiffKeyword ] ]

withSourceRange :: Prod r (SourceRange -> a) -> Prod r a
withSourceRange (Prod p) = Prod $ (\(sr, f) -> (sr, f sr)) <$> p

data ParseError
  = NoParse (Maybe Pos) (Set String)
  | EmptyBlock SourceRange
    -- ^ A block's header (where its colon is), with nothing in the block
    -- yet. It's reported on the line after the header: where its first
    -- line is to go, and where the cursor is, once a header's typed.
  | UnfinishedLine (Maybe Pos) Bool (Set String)
    -- ^ A line that ends before what's on it does (where it ends; whether
    -- a bracket on it is still open), as one does while it's typed.
  | AmbiguousParse (Maybe Pos) (Set String)
  | ScientificNumber SourceRange String
    -- ^ A number written in scientific notation (see
    -- 'Language.Parser.Tokenizer.ScientificNotation'), as written.
  | BuiltinName SourceRange String String String
    -- ^ A built-in name (as written; what it is; what it was going to name)
    -- used for a variable, function, or code block of the script's own.
  | IntegerTooBig SourceRange Integer
    -- ^ An integer literal too big for an integer ('maxIntegerLiteral'),
    -- which would otherwise wrap around.
  deriving Show

-- | The largest integer a script can write: integers are 32 bits in the
-- compiled backends (LLVM's and the web's), so a bigger one would wrap
-- around there (as @2251799813685248@ did, silently).
maxIntegerLiteral :: Integer
maxIntegerLiteral = 2147483647

instance PrettyPrint ParseError where
  pp = (:[]) . \case
    NoParse _ prods -> "I got stuck and couldn't parse the input here" ++
      expectingClause prods
    EmptyBlock _ -> "This block has nothing in it yet: it needs at least one line, indented under it."
    UnfinishedLine _ unclosed prods ->
      "This line isn't finished" ++ (if unclosed then " (a bracket on it is still open)" else "")
      ++ expectingClause prods
    AmbiguousParse _ prods -> "I got stuck because the grammar is ambiguous here" ++
      expectingClause prods
    ScientificNumber _ s -> scientificNumberMessage s
    IntegerTooBig _ n ->
      "`" ++ show n ++ "` is too big for an integer, which cannot exceed " ++ show maxIntegerLiteral
      ++ ". For the real number, write `" ++ show n ++ ".0`."
    BuiltinName _ name what role ->
      "`" ++ name ++ "` is " ++ what ++ ", so it can't also name a " ++ role ++ ". Try another name"
      ++ (if name `elem` ["i", "e"] then " (k, j, n, ...)." else ".")

-- | Explain why @3.5e-2@ (say) isn't accepted, and how to write each
-- thing it could have meant: the number scientific notation would make
-- of it, or (when there's a sign) arithmetic with Euler's number.
scientificNumberMessage :: String -> String
scientificNumberMessage s =
  "`" ++ s ++ "` looks like scientific notation, which FractalStream doesn't use \
  \(e is reserved for Euler's number). Write " ++ decimal ++ " (or " ++ powerOfTen ++
  ") for the number" ++ eulerClause ++ "."
  where
    (mantissa, afterMantissa) = break (`elem` ("eE" :: String)) s
    (letter, signed) = case afterMantissa of
      c : rest -> (c, rest)
      [] -> ('e', "")
    (sign, digits) = case signed of
      c : rest | c `elem` ("+-" :: String) -> ([c], rest)
      _ -> ("", signed)
    exponent' = (if sign == "-" then negate else id) (read digits) :: Int
    powerOfTen
      | exponent' < 0 = mantissa ++ " * 10^(" ++ show exponent' ++ ")"
      | otherwise = mantissa ++ " * 10^" ++ show exponent'


    -- (a whole number too big to be an integer is written as a real one)
    decimal = case shiftDecimal mantissa exponent' of
      d | all isDigit d, read d > maxIntegerLiteral -> d ++ ".0"
        | otherwise -> d
    eulerClause
      | null sign = ""
      | otherwise = ", or add spaces, `" ++ mantissa ++ " " ++ [letter] ++ " " ++ sign ++ " "
                    ++ digits ++ "`, to multiply by " ++ [letter]

-- | Move a decimal number's point @n@ places right (left, for negative
-- @n@): @shiftDecimal "3.5" (-2) == "0.035"@.
shiftDecimal :: String -> Int -> String
shiftDecimal number n = tidy (whole, fraction)
  where
    (intPart, fracPart) = case break (== '.') number of
      (i, _ : f) -> (i, f)
      (i, []) -> (i, "")
    allDigits = intPart ++ fracPart
    point = length intPart + n
    (whole, fraction)
      | point <= 0 = ("0", replicate (negate point) '0' ++ allDigits)
      | point >= length allDigits = (allDigits ++ replicate (point - length allDigits) '0', "")
      | otherwise = splitAt point allDigits
    tidy (w, f) =
      let w' = case dropWhile (== '0') w of { "" -> "0"; w'' -> w'' }
          f' = reverse (dropWhile (== '0') (reverse f))
      in if null f' then w' else w' ++ "." ++ f'

expectingClause :: Set String -> String
expectingClause prods = case unsnoc (Set.toList prods) of
  Nothing  -> "."
  Just ([], x) -> ", where I was expecting " ++ x ++ "."
  Just ([x'], x) -> ", where I was expecting either " ++ x' ++ " or " ++ x ++ "."
  Just (xs, x)   -> ", where I was expecting one of " ++ intercalate ", " xs ++ ", or " ++ x ++ "."

instance HasErrorLocation ParseError where
  errorLocation = \case
    NoParse mp _ -> maybe NoSourceRange (\p -> SourceRange p p) mp
    EmptyBlock sr -> case sr of
      SourceRange (Pos r _) _ -> let p = Pos (r + 1) 0 in SourceRange p p
      NoSourceRange -> NoSourceRange
    UnfinishedLine mp _ _ -> maybe NoSourceRange (\p -> SourceRange p p) mp
    AmbiguousParse mp _ -> maybe NoSourceRange (\p -> SourceRange p p) mp
    ScientificNumber sr _ -> sr
    IntegerTooBig sr _ -> sr
    BuiltinName sr _ _ _ -> sr


ppFullError :: (PrettyPrint t, HasErrorLocation t) => t -> String -> String
ppFullError err input =
  let msg = pp err
      context = pp (Indicated (errorLocation err) input)
  in unlines (context ++ [""] ++ msg)
