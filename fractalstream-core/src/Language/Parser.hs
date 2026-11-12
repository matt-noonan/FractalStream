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

tok :: String -> Prod r ()
tok s = tokenMatch $ \case
  Identifier n | n == s -> Just ()
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
  | otherwise = case P.fullParses (P.parser (fmap snd . runProd <$> p)) input of
    ([], r)  -> Left (NoParse `onReport` r)
    ([x], _) -> pure x
    (_, r)   -> Left (AmbiguousParse `onReport` r)
  where
    onReport f P.Report{..} = f (toPos position) (Set.fromList expected)
    toPos i = case mconcat (take (i + 1) (map tokenSourceRange input)) of
      NoSourceRange   -> Nothing
      SourceRange _ e -> Just e

-- | Built-in names a script uses for its own variables, functions, and
-- code blocks: which the grammar would otherwise read as the built-in,
-- failing somewhere later, and confusingly. (Most of these names aren't
-- keywords to the tokenizer, so that the grammar can take them for what
-- they are where they're used.) A name is checked where it's defined:
-- a function's (@f(x) ≔@) or @define@'s name and parameters, @x ≔@, a
-- statement's @x ⟵@ and @x : T@, @x ⟼@, @for each x@, @with first x@, and the points
-- of a handler's @on ... at x:@.
builtinBindings :: [SRToken] -> [ParseError]
builtinBindings = go True . map (\(SRToken t sr) -> (t, sr))
  where
    go _ [] = []
    go atStart toks@((t, _) : rest) = here atStart toks ++ go (t `elem` [Newline, Indent, Dedent]) rest

    here atStart = \case
      n : rest | atStart, Just _ <- functionDefinitionBody (map fst rest) -> named "function" n ++ params rest
      (Identifier "define", _) : n : rest -> named "code block" n ++ params rest
      (Identifier "each", _) : n : _ -> named "variable" n
      (Identifier "first", _) : n : _ -> named "variable" n
      (Identifier "on", _) : rest | atStart -> handlerPoints rest
      n : (ColonEquals, _) : _ -> named "variable" n
      n : (RightArrow, _) : _ -> named "variable" n
      n : (LeftArrow, _) : _ | atStart -> named "variable" n
      n : (Colon, _) : (next, _) : _ | atStart, next `notElem` [Newline, Indent, Dedent] -> named "variable" n
      _ -> []

    named role (t, sr) = [ BuiltinName sr name what role | Just (name, what) <- [builtin t] ]

    -- a function's or `define`'s parameters: each first in its slot
    params = \case
      (OpenParen, _) : rest -> slots rest
      _ -> []
    slots toks =
      let (slot, rest) = break ((`elem` [Comma, CloseParen]) . fst) toks
      in take 1 (concatMap (named "variable") (take 1 slot)) ++ case rest of
           (Comma, _) : more -> slots more
           _ -> []

    -- the points of a handler's first line, up to its colon
    handlerPoints toks =
      let line = takeWhile ((/= Colon) . fst) toks
      in concat [ named "variable" n
                | ((before, _), n) <- zip line (drop 1 line)
                , before `elem` [Identifier "at", Identifier "from", Identifier "to", OpenParen, Comma] ]

-- | A built-in name, and what it is, if this token is one.
builtin :: Token -> Maybe (String, String)
builtin = \case
  I -> Just ("i", "the imaginary unit")
  Euler -> Just ("e", "Euler's number")
  Pi -> Just ("pi", "π")
  Iterations -> Just ("iterations", "a built-in value (the last loop's iteration count)")
  Stuck -> Just ("stuck", "a built-in value (whether the last loop gave up)")
  Identifier n
    | Just what <- lookup n described -> Just (n, "a built-in function (" ++ what ++ ")")
    | n `Map.member` B.realFunctions || n `Map.member` B.commonFunctions
      || n `Map.member` B.complexFunctions || n `Map.member` B.roundingFunctions
      || n `Map.member` B.wheels || n `elem` others -> Just (n, "a built-in function")
    | n `Map.member` B.colors -> Just (n, "a color's name")
  t | Just n <- lookup t keywordNames -> Just (n, "a built-in function")
  _ -> Nothing
  where
    described =
      [ ("bar", "the complex conjugate"), ("conj", "the complex conjugate")
      , ("Bar", "the complex conjugate"), ("Conj", "the complex conjugate")
      , ("re", "the real part"), ("Re", "the real part")
      , ("im", "the imaginary part"), ("Im", "the imaginary part")
      , ("arg", "the argument"), ("Arg", "the argument") ]
    others = [ "mod", "rgb", "hsv", "hsl", "blend", "cycle", "dark", "light", "invert"
             , "autocolor", "nearest_autocolor", "get_autocolor", "point", "table_size", "filled" ]
    keywordNames = [ (t, n) | (n, t) <- Map.toList wordlikeTokens
                            , t `elem` [ TextKeyword, AppendKeyword, PrependKeyword, JoinKeyword
                                       , RemoveKeyword, FindKeyword, TransformKeyword, RangeKeyword
                                       , LengthKeyword, DiffKeyword ] ]

withSourceRange :: Prod r (SourceRange -> a) -> Prod r a
withSourceRange (Prod p) = Prod $ (\(sr, f) -> (sr, f sr)) <$> p

data ParseError
  = NoParse (Maybe Pos) (Set String)
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
    AmbiguousParse _ prods -> "I got stuck because the grammar is ambiguous here" ++
      expectingClause prods
    ScientificNumber _ s -> scientificNumberMessage s
    IntegerTooBig _ n ->
      "`" ++ show n ++ "` is too big for an integer (they go up to " ++ show maxIntegerLiteral
      ++ "). For the real number, write `" ++ show n ++ ".0`."
    BuiltinName _ name what role ->
      "`" ++ name ++ "` is " ++ what ++ ", so it can't also name a " ++ role ++ ". Try another name"
      ++ (if name `elem` ["i", "e"] then " (k, j, n, ...)." else ".")

-- | Explain why @3.5e-2@ (say) isn't accepted, and how to write each
-- thing it could have meant: the number scientific notation would make
-- of it, or (when there's a sign) arithmetic with Euler's number.
scientificNumberMessage :: String -> String
scientificNumberMessage s =
  "`" ++ s ++ "` looks like scientific notation, which FractalStream doesn't use \
  \(e is Euler's number). Write " ++ decimal ++ " (or " ++ powerOfTen ++
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
    AmbiguousParse mp _ -> maybe NoSourceRange (\p -> SourceRange p p) mp
    ScientificNumber sr _ -> sr
    IntegerTooBig sr _ -> sr
    BuiltinName sr _ _ _ -> sr


ppFullError :: (PrettyPrint t, HasErrorLocation t) => t -> String -> String
ppFullError err input =
  let msg = pp err
      context = pp (Indicated (errorLocation err) input)
  in unlines (context ++ [""] ++ msg)
