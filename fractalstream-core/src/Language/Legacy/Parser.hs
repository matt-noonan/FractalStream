-- | A parser for original-FractalStream scripts ("Language.Legacy.Syntax").
--
-- The language ignores case, ends every command with a period, and
-- writes comments in parentheses where a command could start
-- (@(like this)@); elsewhere, parentheses group, as usual.
module Language.Legacy.Parser
  ( parseLegacyScript
  , ParseNote(..)
  ) where

import FractalStream.Prelude hiding (many, some)

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Data.List (dropWhileEnd)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

import Language.Legacy.Syntax

type Parser = Parsec Void String

-- | Something worth knowing about how a script was read.
data ParseNote
  = ThenStartsBlocks
    -- ^ The script only parsed when every @then@ (without a colon) was
    -- read as @then:@, starting a block that runs to an @end.@
  deriving (Eq, Show)

-- | How to read a colon-less @then@.
data ThenMode
  = ThenCommand  -- ^ followed by one command, as the guides write it
  | ThenBlock    -- ^ like @then:@, starting a block
  deriving Eq

-- | How a script is being read, and where in it the parser is.
data Mode = Mode
  { thenMode  :: ThenMode
  , strays    :: Bool
    -- ^ whether an @end.@ that can't end a block, or an @until c.@ that
    -- can't end a @do@ loop, is kept as a 'Stray' instead of failing
  , blockOpen :: Bool  -- ^ inside a block, which an @end.@ ends
  , inDo      :: Bool  -- ^ directly inside a @do@ loop, which an @until@ ends
  }

-- | Parse a script, or describe where and why it couldn't be.
--
-- A script that doesn't parse as usual is tried again more leniently,
-- in this order, keeping the first reading that works:
--
-- * Some scripts write @if c then@ with no colon, followed by several
--   commands and an @end.@, so every colon-less @then@ is read as
--   @then:@.
-- * Some have an @end.@ that ends no block, or an @until c.@ with no
--   @do@, which are kept as 'Stray's: first with @then@ read as
--   @then:@, and then as usual.
--
-- (Reading @then@ as @then:@ first matters: with strays allowed, a
-- script of colon-less @then@ blocks also parses as single-command
-- @if@s followed by strays, which nests the commands wrongly.) The error
-- reported, if no reading works, is the usual reading's.
parseLegacyScript :: String -> Either String (Script, [ParseNote])
parseLegacyScript src = case parse (script (mode ThenCommand False)) "script" src of
  Right s -> Right (s, [])
  Left err -> case mapMaybe attempt readings of
    s : _ -> Right s
    [] -> Left (errorBundlePretty err)
  where
    mode t st = Mode { thenMode = t, strays = st, blockOpen = False, inDo = False }
    readings =
      [ (mode ThenBlock False, [ThenStartsBlocks])
      , (mode ThenBlock True, [ThenStartsBlocks])
      , (mode ThenCommand True, []) ]
    attempt (m, notes) = either (const Nothing) (\s -> Just (s, notes)) (parse (script m) "script" src)

script :: Mode -> Parser Script
script mode = do
  space
  isReal <- option False (True <$ (symbol "{" *> keyword "real" *> symbol "}"))
  Script isReal <$> many (command mode) <* eof

------------------------------------------------------------------------
-- Lexing

lexeme :: Parser a -> Parser a
lexeme p = p <* space

symbol :: String -> Parser String
symbol = lexeme . string

period :: Parser ()
period = void (symbol ".")

-- | A keyword, in any case, not followed by more of a word.
keyword :: String -> Parser ()
keyword w = lexeme (try (string' w *> notFollowedBy (satisfy isWordChar)))

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'

keywords :: [String]
keywords =
  [ "set", "to", "default", "if", "then", "else", "block", "end", "do"
  , "until", "iterate", "on", "repeat", "times", "reduce", "mod", "report"
  , "succeed", "fail", "par", "dyn", "probe", "load", "using", "each", "in"
  , "at", "and", "or", "xor", "escapes", "vanishes", "stops" ]

identifier :: Parser String
identifier = lexeme . try $ do
  name <- (:) <$> satisfy isAlpha <*> many (satisfy isWordChar)
  when (map toLower name `elem` keywords) $
    fail ("`" ++ name ++ "` is a keyword, not a variable")
  pure name

quoted :: Parser String
quoted = lexeme (char '"' *> manyTill anySingle (char '"'))

-- | Text up to (not including) one of the given characters, trimmed.
textUntil :: [Char] -> Parser String
textUntil stops = trim <$> many (satisfy (`notElem` stops))

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

------------------------------------------------------------------------
-- Commands

command :: Mode -> Parser Cmd
command mode = choice
  [ comment
  , setCmd
  , Default <$> (keyword "default" *> identifier) <*> (keyword "to" *> expr <* period)
  , ifCmd mode
  , Block <$> (blockStart *> blockBody mode)
  , DoUntil <$> (keyword "do" *> many (command mode { inDo = True })) <*> (keyword "until" *> cond <* period)
  , Iterate <$> (keyword "iterate" *> expr)
            <*> optional (keyword "on" *> identifier)
            <*> (keyword "until" *> cond <* period)
  , Repeat <$> (keyword "repeat" *> expr <* keyword "times") <*> command mode
  , Reduce <$> (keyword "reduce" *> identifier) <*> (keyword "mod" *> expr <* period)
  , Report <$> (keyword "report" *> expr <* period)
  , Succeed <$ keyword "succeed" <* period
  , Fail <$ keyword "fail" <* period
  , flag
  , Par <$> (keyword "par" *> command mode)
  , Dyn <$> (keyword "dyn" *> command mode)
  , Probe <$> (keyword "probe" *> option ProbeComplex probeFormat) <*> quoted <*> command mode
  , unsupported "load" (keyword "load" *> textUntil "." *> period)
  , unsupported "using each" (keyword "using" *> keyword "each" *> identifier *> keyword "in"
                                *> textUntil "\n" *> pure ())
  , stray "end." (guard (strays mode && not (blockOpen mode)) *> keyword "end" *> period)
  , stray "until" (guard (strays mode && not (inDo mode)) *> keyword "until" *> cond *> period)
  ] <?> "a command"
  where
    blockStart = keyword "block" <|> void (symbol ":")

    probeFormat = choice
      [ ProbeComplex <$ keyword "complex", ProbeReal <$ keyword "real"
      , ProbeInteger <$ keyword "integer", ProbeRational <$ keyword "rational" ]

    -- keep the source text of a command there's nothing to translate to
    unsupported what p = lexeme $ do
      (src, ()) <- match p
      pure (Unsupported what (trim src))

    stray what p = try $ do
      (src, ()) <- match p
      pure (Stray what (trim src))

-- | The commands of a block, and the @end.@ ending it.
blockBody :: Mode -> Parser [Cmd]
blockBody mode = many (command mode { blockOpen = True, inDo = False }) <* keyword "end" <* period

-- | @(a comment)@, with nested parentheses allowed inside.
comment :: Parser Cmd
comment = lexeme (Comment . trim <$> (char '(' *> body <* char ')'))
  where
    body = concat <$> many (nested <|> ((:[]) <$> satisfy (`notElem` "()")))
    nested = (\b -> "(" ++ b ++ ")") <$> (char '(' *> body <* char ')')

setCmd :: Parser Cmd
setCmd = do
  keyword "set"
  vars <- ((:[]) <$> identifier)
      <|> between (symbol "(") (symbol ")") (identifier `sepBy1` symbol ",")
  keyword "to"
  Set vars <$> expr <* period

ifCmd :: Mode -> Parser Cmd
ifCmd mode = do
  keyword "if"
  c <- cond
  keyword "then"
  yes <- thenCommand
  If c yes <$> optional (keyword "else" *> command mode)
  where
    -- `then:` and `else:` start a block, as `:` does anywhere; in
    -- `ThenBlock` mode, so does a `then` without one
    thenCommand = case thenMode mode of
      ThenCommand -> command mode
      ThenBlock -> Block <$> (optional (symbol ":") *> blockBody mode)

-- | @[name]@ or @[name | color]@, usually followed by a period, which
-- some scripts leave out.
flag :: Parser Cmd
flag = lexeme $ do
  _ <- symbol "["
  name <- textUntil "|]"
  hint <- optional (symbol "|" *> textUntil "]")
  _ <- symbol "]"
  optional period
  pure (Flag name hint)

------------------------------------------------------------------------
-- Expressions

expr :: Parser Expr
expr = makeExprParser term
  [ [ InfixR (Bin Pow <$ symbol "^") ]
  , [ Prefix (Neg <$ symbol "-") ]
  , [ InfixL (Bin Mul <$ symbol "*"), InfixL (Bin Div <$ symbol "/") ]
  , [ InfixL (Bin Add <$ symbol "+"), InfixL (Bin Sub <$ symbol "-") ]
  ] <?> "an expression"

term :: Parser Expr
term = choice
  [ number
  , Pi <$ (symbol "π" <|> try (string' "pi" <* notFollowedBy (satisfy isWordChar) <* space))
  , imaginaryUnit
  , parenthesized
  , try (Call <$> identifier <*> between (symbol "(") (symbol ")") expr)
  , Var <$> identifier
  ]
  where
    imaginaryUnit = lexeme (try (Imag "1" <$ char' 'i' <* notFollowedBy (satisfy isWordChar)))
    parenthesized = between (symbol "(") (symbol ")") $ do
      a <- expr
      option (Paren a) (Pair a <$> (symbol "," *> expr))

-- | A number, as written: @17@, @1.25@, @.5@, @2e-3@; with an @i@
-- right after it, an imaginary one (@1.2i@).
number :: Parser Expr
number = lexeme . try $ do
  digits <- some (satisfy isDigit) <|> lookAhead (char '.' *> satisfy isDigit) *> pure ""
  frac <- option "" (try ((:) <$> char '.' <*> some (satisfy isDigit)))
  ex <- option "" . try $ do
    e <- char' 'e'
    sign <- option "" ((:[]) <$> (char '+' <|> char '-'))
    ds <- some (satisfy isDigit)
    pure (e : sign ++ ds)
  let lit = digits ++ frac ++ ex
  imaginary <- option False (True <$ try (char' 'i' <* notFollowedBy (satisfy isWordChar)))
  notFollowedBy (satisfy isWordChar)
  pure (if imaginary then Imag lit else Num lit)

------------------------------------------------------------------------
-- Conditions

cond :: Parser Cond
cond = makeExprParser condTerm
  [ [ InfixL (BoolOp And <$ keyword "and") ]
  , [ InfixL (BoolOp Xor <$ keyword "xor"), InfixL (BoolOp Or <$ keyword "or") ]
  ] <?> "a condition"

condTerm :: Parser Cond
condTerm = try (ParenCond <$> between (symbol "(") (symbol ")") cond) <|> simple
  where
    simple = do
      a <- expr
      choice
        [ Escapes a <$ keyword "escapes"
        , Vanishes a <$ keyword "vanishes"
        , Stops a <$ keyword "stops"
        , Rel <$> relOp <*> pure a <*> expr
        ]
    relOp = choice
      [ LessEq <$ symbol "<=", GreaterEq <$ symbol ">="
      , Less <$ symbol "<", Greater <$ symbol ">"
      , Equal <$ (symbol "==" <|> symbol "=") ]
