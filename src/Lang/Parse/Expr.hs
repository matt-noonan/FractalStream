{- |
Module      : Lang.Expr
Description : Parsers for expression ASTs.
-}
module Lang.Parse.Expr
  ( parseCExpr
  ) where

import Lang.Expr
import Lang.Numbers

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

token_def = emptyDef
  { commentStart = "/*"
  , commentEnd = "*/"
  , identStart = letter <|> char '_'
  , identLetter = alphaNum <|> char '_'
  , opStart  = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames = words "+ - * / ^ = < > <= >="
  , reservedNames =
    [ "if", "then", "else" ]
}

TokenParser
  { parens = p_parens
  , identifier = p_identifier
  , reservedOp = p_reservedOp
  , reserved = p_reserved
  , semiSep1 = p_semiSep1
  , whiteSpace = p_whiteSpace
  , naturalOrFloat = p_num
  } = makeTokenParser token_def

-- | Table of operators, in order of precedence.
cexpr_table =
  [ [ Prefix (p_reservedOp "-" >> return CNeg)
    ]
  , [ Infix  (p_reservedOp "^" >> return CPow) AssocLeft
    ]
  , [ Infix  (p_reservedOp "*" >> return CMul) AssocLeft
    , Infix  (p_whiteSpace     >> return CMul) AssocLeft
    , Infix  (p_reservedOp "/" >> return CDiv) AssocLeft
    ]
  , [ Infix  (p_reservedOp "+" >> return CAdd) AssocLeft
    , Infix  (p_reservedOp "-" >> return CSub) AssocLeft
    ]
  ]

cexpr_term = p_parens p_cexpr
  <|> CVar <$> p_identifier
  <|> (CConst . (\x -> complex x 0) . either fromIntegral id) <$> p_num

p_cexpr :: Parser (Expr C)
p_cexpr = buildExpressionParser cexpr_table cexpr_term <?> "expression"

allOf :: Parser a -> Parser a
allOf p = do
  p_whiteSpace
  r <- p
  eof
  return r

parseCExpr :: String -> Either ParseError (Expr C)
parseCExpr = parse (allOf p_cexpr) ""
