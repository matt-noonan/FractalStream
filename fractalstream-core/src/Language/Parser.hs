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
  , eol
  -- * Precedence-based parsers
  , ParserStep(..)
  , PriorityParser(..)
  , parsePrio
  , leveledParser
  , infixR
  , infixL
  , prefixOp
  -- * Re-exports from megaparsec
  , dbg
  , (<|>)
  , (<?>)
  , (<$)
  , ($>)
  , (<*)
  , (*>)
  , (<&>)
  , sepBy1
  , satisfy
  , try
  , manyTill
  , manyTill_
  , lookAhead
  , many
  , some
  , eof
  , mzero
  , choice
  , notFollowedBy
  , void
  , parseError
  , ErrorFancy(..)
  , ErrorItem(..)
  , ParseError(..)
  ) where

import Text.Megaparsec hiding (Token, parse)
import qualified Data.List.NonEmpty as NE
import Data.String
import Data.Functor ((<&>), ($>))
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Control.Monad
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord

-- | Swap this out for @Text.Megaparsec.Debug@'s
-- 'dbg'' to debug the parsers.
--import Text.Megaparsec.Debug (dbg')
dbg :: String -> Parser t -> Parser t
--dbg = dbg'
dbg _ = id

data BadParse
  = AmbiguousParse
  | UnboundVariable String
  | MismatchedType String
  | Unexpected (Maybe [Token]) [[Token]]
  | Other String
  | Internal
  deriving (Eq, Ord, Show)

instance ShowErrorComponent BadParse where showErrorComponent = show
instance VisualStream [Token] where showTokens _ (t NE.:| ts) = show (t : ts)

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
    TrivialError n ue es -> Left (n, Unexpected (getTokens <$> ue) (map getTokens (Set.toList es)))
  Right v   -> Right v

getTokens :: ErrorItem Token -> [Token]
getTokens = \case
  Tokens (t NE.:| ts) -> t : ts
  Label  (c NE.:| cs) -> [Identifier (c:cs)]
  EndOfInput          -> [Newline]

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

opTokens :: [(String, Token)]
opTokens = sortOn (\x -> (Down (length (fst x)), x)) $
  [ ("+", Plus), ("-", Minus), ("*", Times), ("/", Divide)
  , ("^", Caret), ("|", Bar), ("(", OpenParen), (")", CloseParen)
  , ("[", OpenBracket), ("]", CloseBracket)
  , ("{", OpenBrace), ("}", CloseBrace)
  , (",", Comma), (":", Colon), (";", Semicolon)
  , (">", GreaterThan), ("<", LessThan), ("=", Equal)
  , (">=", GreaterThanOrEqual), ("<=", LessThanOrEqual)
  , ("≥", GreaterThanOrEqual), ("≤", LessThanOrEqual)
  , ("!=", NotEqual), ("=/=", NotEqual), ("≠", NotEqual)
  , ("->", RightArrow), ("<-", LeftArrow)
  , ("⭢", RightArrow), ("⭠", LeftArrow)
  , ("→", RightArrow), ("←", LeftArrow)
  ]

longestMatchingOperator :: String -> Maybe (Token, String)
longestMatchingOperator cs =
  listToMaybe [ (t, drop (length s) cs)
              | (s, t) <- opTokens, s `isPrefixOf` cs ]

wordlikeTokens :: Map String Token
wordlikeTokens = Map.fromList
  [ ("if", If), ("then", Then), ("else", Else)
  , ("e", Euler), ("pi", Pi), ("i", I)
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

-- | End of line (including end of input)
eol :: Parser ()
eol = eof <|> tok_ Newline

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

  -- Tokenize special operators
  cs | Just (tok, cs') <- longestMatchingOperator cs
       -> tok : tokenize cs'

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
  | I
  | True_
  | False_
  | Or_
  | And_
  | Not_
  | LeftArrow
  | RightArrow
  deriving (Eq, Ord, Show)

instance IsString Token where fromString = Identifier

---------------------------------------------------------------------------------
-- Precedence-based parsers
---------------------------------------------------------------------------------

data ParserStep t = ParserStep
  { pFull :: Parser t
  , pSame :: Parser t
  , pNext :: Parser t
  }

newtype PriorityParser t = PriorityParser
  { parseAtPriority ::
      Map (Down Integer) (ParserStep t -> Parser t)
  }

instance Semigroup (PriorityParser t) where
  PriorityParser m1 <> PriorityParser m2 =
    PriorityParser (Map.unionWith (\f g s -> f s <|> g s) m1 m2)

instance Monoid (PriorityParser t) where
  mempty = PriorityParser Map.empty

parsePrio :: forall a. PriorityParser a -> Parser a
parsePrio (PriorityParser m) = head parserList
  where
    pstep0 = ParserStep
      { pFull = head parserList
      , pSame = head parserList
      , pNext = head (tail parserList) -- (*), see below
      }
    parserList :: [Parser a]
    parserList = go pstep0 (Map.elems m)
    go :: ParserStep a
       -> [(ParserStep a -> Parser a)]
       -> [Parser a]
    go _ [] = [mzero, mzero] -- ensure at least 2 entries for (*)
    go pstep (p:ps) = p pstep
                    : let nexts = go (pstep { pSame = pNext pstep
                                            , pNext = head (tail nexts) }) ps
                      in nexts

leveledParser :: forall a
               . Integer
              -> (ParserStep a -> Parser a)
              -> PriorityParser a
leveledParser level =
  PriorityParser . Map.singleton (Down level)

infixR :: forall a
          . Token
         -> Integer
         -> String
         -> (a -> a -> a)
         -> PriorityParser a
infixR tk level name ctor =
  leveledParser level $ \ParserStep{..} ->
       dbg name $ do
         lhs <- pNext
         ((ctor lhs <$> (tok_ tk *> pSame))
          <|> pure lhs
          <?> name)

infixL :: forall a
          . Token
         -> Integer
         -> String
         -> (a -> a -> a)
         -> PriorityParser a
infixL tk level name ctor =
  leveledParser level $ \ParserStep{..} ->
       dbg name $ (foldl1 ctor <$>
                   (pNext `sepBy1` tok_ tk) <?> name)

prefixOp :: forall a
          . Token
         -> Integer
         -> String
         -> (a -> a)
         -> PriorityParser a
prefixOp tk level name ctor =
  leveledParser level $ \ParserStep{..} ->
    dbg name (ctor <$> (tok_ tk *> pSame)
               <|> pNext
               <?> name)
