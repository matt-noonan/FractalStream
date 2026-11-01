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

import qualified Data.Set as Set
import qualified Text.Earley as P

import Language.Parser.Tokenizer
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
  | otherwise = case P.fullParses (P.parser (fmap snd . runProd <$> p)) input of
    ([], r)  -> Left (NoParse `onReport` r)
    ([x], _) -> pure x
    (_, r)   -> Left (AmbiguousParse `onReport` r)
  where
    onReport f P.Report{..} = f (toPos position) (Set.fromList expected)
    toPos i = case mconcat (take (i + 1) (map tokenSourceRange input)) of
      NoSourceRange   -> Nothing
      SourceRange _ e -> Just e

withSourceRange :: Prod r (SourceRange -> a) -> Prod r a
withSourceRange (Prod p) = Prod $ (\(sr, f) -> (sr, f sr)) <$> p

data ParseError
  = NoParse (Maybe Pos) (Set String)
  | AmbiguousParse (Maybe Pos) (Set String)
  | ScientificNumber SourceRange String
    -- ^ A number written in scientific notation (see
    -- 'Language.Parser.Tokenizer.ScientificNotation'), as written.
  deriving Show

instance PrettyPrint ParseError where
  pp = (:[]) . \case
    NoParse _ prods -> "I got stuck and couldn't parse the input here" ++
      expectingClause prods
    AmbiguousParse _ prods -> "I got stuck because the grammar is ambiguous here" ++
      expectingClause prods
    ScientificNumber _ s -> scientificNumberMessage s

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


    decimal = shiftDecimal mantissa exponent'
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


ppFullError :: (PrettyPrint t, HasErrorLocation t) => t -> String -> String
ppFullError err input =
  let msg = pp err
      context = pp (Indicated (errorLocation err) input)
  in unlines (context ++ [""] ++ msg)
