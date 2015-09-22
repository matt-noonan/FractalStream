{- |
Module      : Lang.Expr
Description : Parsers for expression ASTs.
-}
module Lang.Parse.Expr
  ( parseRExpr
  , parseCExpr
  ) where

import Lang.Expr
import Lang.Numbers

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity (Identity)

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
p_reserved   :: String -> Parser ()
p_whiteSpace :: Parser ()
p_symbol     :: String -> Parser String
p_num        :: Parser (Either Integer Double)

TokenParser
  { parens     = p_parens
  , identifier = p_identifier
  , reservedOp = p_reservedOp
  , reserved   = p_reserved
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
data TermContext a = TermContext
  { mkVar   :: String -> a  -- ^ Handler when parsing an identifier
  , mkConst :: Double -> a  -- ^ Handler when parsing a number
  , inNorm  :: Bool -- ^ Is the parser currently handling a |...| construction?
  }

expr_term :: Floating a => TermContext a -> Parser a
expr_term ctx = if inNorm ctx then p_normfree_terms else (p_normfree_terms <|> p_normed_term)
  where p_normfree_terms = p_parens (p_expr ctx { inNorm = False })
                       <|> mkVar ctx <$> p_identifier
                       <|> (mkConst ctx . either fromIntegral id) <$> p_num
        p_normed_term = abs <$> between (p_symbol "|") (p_symbol "|")
                                        (p_expr ctx { inNorm = True })

-- | Generic expression parser.
p_expr :: Floating a => TermContext a -> Parser a
p_expr ctx = buildExpressionParser expr_table (expr_term ctx) <?> "expression"

-- | Parser for expressions over $\mathbb{C}$.
p_cexpr :: Parser (Expr C)
p_cexpr = p_expr (TermContext { mkVar = CVar, mkConst = CConst . toC, inNorm = False })
  where toC x = complex x 0

-- | Parser for expressions over $\mathbb{R}$.
p_rexpr :: Parser (Expr R)
p_rexpr = p_expr (TermContext { mkVar = Var, mkConst = Const . R, inNorm = False })

-- | Helper to parse a complete expression.
allOf :: Parser a -> Parser a
allOf p = do
  p_whiteSpace
  r <- p
  eof
  return r

-- | Parse a string representing an expression over $\mathbb{C}$.
parseCExpr :: String -> Either ParseError (Expr C)
parseCExpr = parse (allOf p_cexpr) "C expression"

-- | Parse a string representing an expression over $\mathbb{R}$.
parseRExpr :: String -> Either ParseError (Expr R)
parseRExpr = parse (allOf p_rexpr) "R expression"
