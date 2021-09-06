{-# language OverloadedStrings #-}

module Language.Parser
  ( type Parser
  , parse
  , BadParse(..)
  , bad
  , tok_
  , tokenize
  , tokenizeWithIndentation
  , Token(..)
  , nest
  , anyToken
  -- Re-exports from megaparsec
  , (<|>)
  , (<?>)
  , (<$)
  , (<*)
  , (*>)
  , (<&>)
  , sepBy1
  , satisfy
  , try
  , manyTill
  , manyTill_
  , lookAhead
  , some
  , eof
  ) where

import Text.Megaparsec hiding (Token, parse)
import qualified Data.List.NonEmpty as NE
import Data.String
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Control.Monad

data BadParse
  = AmbiguousParse
  | UnboundVariable String
  | MismatchedType String
  | Unexpected
  | Other String
  | Internal
  deriving (Eq, Ord, Show)

-- | Fail with a custom parse error
bad :: forall t. BadParse -> Parser t
bad = fancyFailure . Set.singleton . ErrorCustom

type Parser t = Parsec BadParse [Token] t

parse :: Parser t -> [Token] -> Either (Int, BadParse) t
parse p toks = runParser (p <* eof) "" toks & \case
  Left errs -> case NE.head (bundleErrors errs) of
    FancyError n (Set.toList -> (e:_)) -> Left . (n,) $ case e of
      ErrorFail s -> Other s
      ErrorIndentation{} -> Other "indentation error"
      ErrorCustom e' -> e'
    FancyError n _ -> Left (n, Internal)
    TrivialError n _ _ -> Left (n, Unexpected)
  Right v   -> Right v

nest :: Either (Int, BadParse) t -> Parser t
nest =  \case
  Right t     -> pure t
  Left (_, e) -> bad e

anyToken :: Parser Token
anyToken = satisfy (const True)

-- | Parse a single token and discard it
tok_ :: Token -> Parser ()
tok_ = void . single

ident :: Char -> Bool
ident c = isAlphaNum c || c == '_'


singleTokens :: Map Char Token
singleTokens = Map.fromList
  [ ('+', Plus), ('-', Minus), ('*', Times), ('/', Divide)
  , ('^', Caret), ('|', Bar), ('(', OpenParen), (')', CloseParen)
  , ('[', OpenBracket), (']', CloseBracket)
  , ('{', OpenBrace), ('}', CloseBrace)
  , (',', Comma), (':', Colon), (';', Semicolon)
  ]

wordlikeTokens :: Map String Token
wordlikeTokens = Map.fromList
  [ ("if", If), ("then", Then), ("else", Else)
  , ("e", Euler), ("pi", Pi)
  , ("true", True_), ("false", False_)
  , ("or", Or_), ("and", And_), ("not", Not_)
  ]
data TokenGroup
  = Group [Token] [TokenGroup]
  | Single [Token]
  deriving (Eq, Ord, Show)

toTok :: TokenGroup -> [Token]
toTok (Single s) = s ++ [Newline]
toTok (Group s xs) = [Indent] ++ s ++ [Newline] ++ concatMap toTok xs ++ [Dedent]


tokenize :: String -> [Token]
tokenize = \case
  -- Done!
  [] -> []

  -- Skip whitespace
  (c:cs) | isSpace c -> tokenize cs

  -- Tokenize negative numbers
  ('-':cs@(d:_))
    | isDigit d ->
      let ds = '-' : takeWhile isDigit cs
      in case dropWhile isDigit cs of
            ('.' : cs'@(d' : _))
              | isDigit d' ->
                  let ds' = ds <> "." <> takeWhile isDigit cs'
                  in NumberF (read ds') : tokenize (dropWhile isDigit cs')
            cs' -> NumberI (read ds) : tokenize cs'

  -- Tokenize positive numbers
  cs@(d:_)
    | isDigit d ->
        let ds = takeWhile isDigit cs
        in case dropWhile isDigit cs of
          ('.' : cs'@(d' : _))
            | isDigit d' ->
                let ds' = ds <> "." <> takeWhile isDigit cs'
                    in NumberF (read ds') : tokenize (dropWhile isDigit cs')
          cs' -> NumberI (read ds) : tokenize cs'

  -- Tokenize the single-character tokens
  (c:cs) | Just tok <- Map.lookup c singleTokens
           -> tok : tokenize cs

  -- Tokenize identifiers
  cs@(c:_) | isAlpha c ->
               let ds = takeWhile ident cs
                   cs' = dropWhile ident cs
                   tok = case Map.lookup ds wordlikeTokens of
                     Just t  -> t
                     Nothing -> Identifier ds
               in tok : tokenize cs'

  -- Otherwise, grab a junk character
  (c:cs) -> Junk c : tokenize cs

tokenizeWithIndentation :: String -> [Token]
tokenizeWithIndentation
         = ([Indent] ++) . (++ [Dedent])
         . concatMap toTok
         . block
         . map observeSpaces
         . filter (not . all isSpace)
         . lines
  where
    observeSpaces :: String -> (Int, String)
    observeSpaces s = (length (takeWhile isSpace s), dropWhile isSpace s)

    -- Dedent all lines by n spaces
    extract :: Int -> [(Int, String)] -> ([TokenGroup], [(Int, String)])
    extract n ss =
      let ss'  = takeWhile ((>= n) . fst) ss
          ss'' = dropWhile ((>= n) . fst) ss
      in (block [(m - n, x) | (m,x) <- ss'], ss'')

    block :: [(Int, String)] -> [TokenGroup]
    block = \case
      [] -> []
      ((n,s) : ss)
        | n == 0 -> Single (tokenize s) : block ss
        | n > 0 ->
            let (is, ss') = extract n ss
            in Group (tokenize s) is : block ss'
      _ -> error "bad indent"

data Token
  = NumberI Integer
  | NumberF Double
  | Plus
  | Minus
  | Times
  | Divide
  | Caret
  | Bar
  | Identifier String
  | Junk Char
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma
  | Colon
  | Semicolon
  | Equal
  | NotEqual
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual
  | Indent
  | Dedent
  | Newline
  | If
  | Then
  | Else
  | Euler
  | Pi
  | True_
  | False_
  | Or_
  | And_
  | Not_
  deriving (Eq, Ord, Show)

instance IsString Token where fromString = Identifier
