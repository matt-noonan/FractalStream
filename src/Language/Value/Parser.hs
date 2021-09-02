{-# language RecursiveDo, ViewPatterns #-}
module Language.Value.Parser
  ( valueExpr
  , parseValue
  ) where

import Text.Earley
import Language.Type
import Language.Value
import Data.Indexed.Functor

import Control.Applicative ((<|>))
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
--import GHC.TypeLits

data Token
  = NumberI Integer
  | NumberF Double
  | Plus
  | Minus
  | Times
  | Divide
  | Caret
  | Function String
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

functionNames :: Set String
functionNames = Set.fromList
  [ "exp", "log", "abs"
  , "cos", "sin", "tan"
  , "arccos", "arcsin", "arctan", "arctan2"
  , "acos", "asin", "atan", "atan2"
  ]

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
                     Nothing -> if Set.member ds functionNames
                                then Function ds else Identifier ds
               in tok : tokenize cs'

  -- Otherwise, grab a junk character
  (c:cs) -> Junk c : tokenize cs

ident :: Char -> Bool
ident c = isAlphaNum c || c == '_'

{-
inEnv :: EnvironmentProxy env
      -> ScalarProxy t
      -> String
      -> (forall name. (Required name env ~ t, KnownSymbol name) => Proxy name -> a)
      -> Maybe a
inEnv env t nameStr k = case someSymbolVal nameStr of
  SomeSymbol name -> case _ of
    Just _  -> k name
    Nothing -> error (nameStr <> " is not bound in the environment")
-}


valueExpr :: forall env t r
           . EnvironmentProxy env
          -> ScalarProxy t
          -> Grammar r (Prod r String Token (Value env t))
valueExpr env = \case

  -- Parse boolean expressions
  BooleanProxy -> mdo
    startRule <- rule rOr
    rOr <- rule $ Fix <$> (Or <$> rOr <* token Or_ <*> rAnd)
            <|> rAnd
            <?> "boolean disjunction"
    rAnd <- rule $ Fix <$> (And <$> rAnd <* token And_ <*> rNot)
            <|> rNot
            <?> "boolean conjunction"
    rNot <- rule $ Fix <$> (Not <$> (token Not_ *> rNot))
            <|> rITE
            <?> "boolean negation"

    rITE <- rule $ Fix <$> (ITE BooleanProxy
                            <$> (token If   *> startRule)
                            <*> (token Then *> startRule)
                            <*> (token Else *> startRule))
             <|> rParen
             <?> "boolean-valued if/then/else expression"

    rParen <- rule $ token OpenParen *> startRule <* token CloseParen
              <|> rTrue
              <|> rFalse
              <?> "subexpression"
    rTrue <- rule $ (Fix (Const (Scalar BooleanProxy True)) <$ token True_)
              <?> "true"
    rFalse <- rule $ (Fix (Const (Scalar BooleanProxy False)) <$ token False_)
              <?> "false"
    pure startRule

  -- Parse integer expressions
  IntegerProxy -> mdo
    startRule <- rule rNegate
    rNegate <- rule $ Fix <$> (NegI <$> (token Minus *> rSub))
               <|> rSub
               <?> "negation"
    rSub <- rule $ Fix <$> (SubI <$> rSub <* token Minus <*> rAdd)
               <|> rAdd
               <?> "subtract"
    rAdd <- rule $ Fix <$> (AddI <$> rAdd <* token Plus <*> rMul)
               <|> rMul
               <?> "sum"
    rMul <- rule $ Fix <$> (MulI <$> rMul <* token Times <*> rDiv)
               <|> rDiv
               <?> "product"
    rDiv <- rule $ Fix <$> (DivI <$> rDiv <* token Divide <*> rPower)
               <|> rPower
               <?> "quotient"
    rPower <- rule $ Fix <$> (PowI <$> rITE <* token Caret <*> rPower)
               <|> rITE
               <?> "exponential"
    boolRule <- valueExpr env BooleanProxy
    rITE <- rule $ Fix <$> (ITE IntegerProxy
                            <$> (token If   *> boolRule)
                            <*> (token Then *> startRule)
                            <*> (token Else *> startRule))
             <|> rConst
             <?> "integer-valued if/then/else expression"
    rConst <- rule $ (\(NumberI n) -> Fix (Const (Scalar IntegerProxy (fromIntegral n))))
                  <$> satisfy numberI
               <|> rVar
               <?> "integer constant"

    rVar <- rule rParen
{-
    rVar <- rule $ (\(Identifier x) -> inEnv env IntegerProxy x (\p -> Fix (Var p IntegerProxy))) <$> satisfy ident
               <|> rParen
               <?> "integer variable"
-}
    rParen <- rule $ token OpenParen *> startRule <* token CloseParen
               <|> rAbs
               <?> "subexpression"
    rAbs <- rule $ Fix <$> (AbsI <$> (token Bar *> startRule <* token Bar))
               <?> "absolute value"
    pure startRule

  -- Parse real expressions
  RealProxy -> mdo
    startRule <- rule rNegate
    rNegate <- rule $ Fix <$> (NegF <$> (token Minus *> rSub))
               <|> rSub
               <?> "negation"
    rSub <- rule $ Fix <$> (SubF <$> rSub <* token Minus <*> rAdd)
               <|> rAdd
               <?> "subtract"
    rAdd <- rule $ Fix <$> (AddF <$> rAdd <* token Plus <*> rMul)
               <|> rMul
               <?> "sum"
    rMul <- rule $ Fix <$> (MulF <$> rMul <* token Times <*> rDiv)
               <|> rDiv
               <?> "product"
    rDiv <- rule $ Fix <$> (DivF <$> rDiv <* token Divide <*> rPower)
               <|> rPower
               <?> "quotient"
    rPower <- rule rITE
    --rPower <- rule $ Fix <$> (PowF <$> rITE <* token Caret <*> rPower)
    --          <|> rITE
    --           <?> "exponential"
    boolRule <- valueExpr env BooleanProxy
    rITE <- rule $ Fix <$> (ITE RealProxy
                            <$> (token If   *> boolRule)
                            <*> (token Then *> startRule)
                            <*> (token Else *> startRule))
             <|> rFunc
             <?> "real-valued if/then/else expression"

    rFunc <- rule $ ((\(toFun -> Just f) -> Fix . f) <$> satisfy isFunction <*> rFunc)
               <|> rE
               <?> "real function of one variable"
    rE <- rule $ (Fix (Const (Scalar RealProxy (exp(1)))) <$ token Euler)
               <|> rPi
               <?> "e"
    rPi <- rule $ (Fix (Const (Scalar RealProxy pi)) <$ token Pi)
               <|> rConstI
               <?> "pi"
    rConstI <- rule $ (\(NumberI n) -> Fix (Const (Scalar RealProxy (fromIntegral n))))
                  <$> satisfy numberI
               <|> rConst
               <?> "real constant"
    rConst <- rule $ (\(NumberF n) -> Fix (Const (Scalar RealProxy n)))
                  <$> satisfy numberF
               <|> rParen
               <?> "real constant"
    rParen <- rule $ token OpenParen *> startRule <* token CloseParen
               <|> rAbs
               <?> "subexpression"
    rAbs <- rule $ Fix <$> (AbsF <$> (token Bar *> startRule <* token Bar))
               <?> "absolute value"
    pure startRule

  -- Parse tuples
  PairProxy t1 t2 -> do
    r1 <- valueExpr env t1
    r2 <- valueExpr env t2
    rule (Fix @_ @(ValueF env) @t
           <$> (PairV (PairProxy t1 t2)
                <$> (token OpenParen *> r1 <* token Comma)
                <*> (r2 <* token CloseParen)))

  _ -> error "todo"


  where
    numberI = \case { NumberI _ -> True; _ -> False }
    numberF = \case { NumberF _ -> True; _ -> False }
    -- ident = \case { Identifier _ -> True; _ -> False }
    isFunction = \case { Function _ -> True; _ -> False }
    toFun = \case
      Function "exp" -> Just ExpF
      Function "log" -> Just LogF
      Function "sin" -> Just SinF
      Function "cos" -> Just CosF
      Function "tan" -> Just TanF
      Function "asin" -> Just ArcsinF
      Function "acos" -> Just ArccosF
      Function "atan" -> Just ArctanF
      Function "arcsin" -> Just ArcsinF
      Function "arccos" -> Just ArccosF
      Function "arctan" -> Just ArctanF
      Function "abs" -> Just AbsF
      _ -> Nothing

parseValue :: EnvironmentProxy env
           -> ScalarProxy t
           -> String
           -> [Value env t]
parseValue env t = fst . fullParses (parser (valueExpr env t)) . tokenize
