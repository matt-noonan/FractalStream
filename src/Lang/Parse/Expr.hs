{- |
Module      : Lang.Expr
Description : Parsers for expression ASTs.
-}
module Lang.Parse.Expr
  ( parseExpr
  ) where

import           Lang.Expr

import           Data.Functor.Identity (Identity)
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.String
import           Text.Parsec.Token

-- | Definition of "token" for the expression language
token_def :: GenLanguageDef String () Identity
token_def = emptyDef
  { commentStart = "/*"
  , commentEnd = "*/"
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> char '_'
  , opStart  = oneOf ":!#$%&*+./<=>?@\\^-~"
  , opLetter = oneOf ":!#$%&*+./<=>?@\\^-~"
  , reservedOpNames = words "+ - * / ^ = < > <= >="
  , reservedNames =
    [ "if", "then", "else" ]
}

-- | Basic token parsers
p_parens     :: Parser a -> Parser a
p_identifier :: Parser String
p_reservedOp :: String -> Parser ()
p_whiteSpace :: Parser ()
p_symbol     :: String -> Parser String
p_num        :: Parser (Either Integer Double)

TokenParser
  { parens     = p_parens
  , identifier = p_identifier
  , reservedOp = p_reservedOp
  , reserved   = _ -- p_reserved
  , whiteSpace = p_whiteSpace
  , symbol     = p_symbol
  , naturalOrFloat = p_num
  } = makeTokenParser token_def

-- | Table of operators, in order of precedence.
expr_table :: Floating a => OperatorTable String () Identity a
expr_table  =
  [ [ Infix  (p_reservedOp "^" >> return (**)) AssocLeft ]
  , [ Prefix (p_reservedOp "-" >> return negate) ]
  , [ Infix  (p_whiteSpace     >> return (*)) AssocLeft ]
  , [ Infix  (p_reservedOp "/" >> return (/)) AssocLeft ]
  , [ Infix  (p_reservedOp "*" >> return (*)) AssocLeft ]
  , [ Infix  (p_reservedOp "+" >> return (+)) AssocLeft
    , Infix  (p_reservedOp "-" >> return (-)) AssocLeft
    ]
  ]

-- | Parser definition and state
data TermContext = TermContext
  { inNorm  :: Bool -- ^ Is the parser currently handling a |...| construction?
  }

expr_term :: TermContext -> Parser Expr
expr_term ctx = if inNorm ctx then p_normfree_terms else (p_normfree_terms <|> p_normed_term)
  where p_normfree_terms = p_paren_expr
                       <|> p_call_or_var
                       <|> (fromDouble . either fromIntegral id) <$> p_num
        p_normed_term = abs <$> between (p_symbol "|") (p_symbol "|")
                                        (p_expr ctx { inNorm = True })
        p_paren_expr = p_parens (p_expr ctx { inNorm = False })
        p_call_or_var = do
          name <- p_identifier
          apply (var name) <$> p_paren_expr <|> return (var name)

-- | Generic expression parser.
p_expr :: TermContext -> Parser Expr
p_expr ctx = buildExpressionParser expr_table (expr_term ctx) <?> "expression"

-- | Parser for expressions over $\mathbb{R}$.
p_expression :: Parser Expr
p_expression = p_expr (TermContext { inNorm = False })

-- | Helper to parse a complete expression.
allOf :: Parser a -> Parser a
allOf p = do
  p_whiteSpace
  r <- p
  eof
  return r

-- | Parse a string representing an expression.
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (allOf p_expression) "expression"
