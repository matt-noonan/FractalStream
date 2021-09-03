{-# language RecursiveDo, ViewPatterns #-}
module Language.Value.Parser
  ( valueExpr
  , parseValue
  , ParseResult(..)
  ) where

import Text.Earley
import Language.Type
import Language.Value
import Data.Indexed.Functor
import Fcf (Pure1)

import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits

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

newtype Status a = Status (Either String a)
  deriving (Functor, Applicative, Monad)

type StatusValue env = Status :.: Pure1 (Value env)

good :: ValueF env (Pure1 (Value env)) t -> Status (Value env t)
good = Status . Right . Fix

bad :: String -> Status (Value env t)
bad = Status . Left

ok :: forall t env f. Functor f => f (ValueF env (StatusValue env) t) -> f (Status (Value env t))
ok = fmap (good <=< isequence @_ @_ @t @Status @(Pure1 (Value env)))

valueExpr :: forall env t r
           . EnvironmentProxy env
          -> ScalarProxy t
          -> Grammar r (Prod r String Token (Status (Value env t)))
valueExpr env = \case

  -- Parse boolean expressions
  BooleanProxy -> mdo
    startRule <- rule rOr
    rOr <- rule $ ok (Or <$> rOr <* token Or_ <*> rAnd)
            <|> rAnd
            <?> "boolean disjunction"
    rAnd <- rule $ ok (And <$> rAnd <* token And_ <*> rNot)
            <|> rNot
            <?> "boolean conjunction"
    rNot <- rule $ ok (Not <$> (token Not_ *> rNot))
            <|> rITE
            <?> "boolean negation"

    rITE <- rule $ ok (ITE BooleanProxy
                            <$> (token If   *> startRule)
                            <*> (token Then *> startRule)
                            <*> (token Else *> startRule))
             <|> rVar
             <?> "boolean-valued if/then/else expression"

    rVar <- rule $ (satisfy isIdentifier <&> \(Identifier n) ->
                       case someSymbolVal n of
                         SomeSymbol name -> case lookupEnv name BooleanProxy env of
                           Found pf  -> good (Var name BooleanProxy pf)
                           Absent _  -> bad ("Unbound variable " <> n)
                           WrongType -> bad ("Mismatched type for variable " <> n)
                   )
                 <|> rParen
                 <?> "boolean variable"
    rParen <- rule $ token OpenParen *> startRule <* token CloseParen
              <|> rTrue
              <|> rFalse
              <?> "subexpression"
    rTrue <- rule $ (good (Const $ Scalar BooleanProxy True) <$ token True_)
              <?> "true"
    rFalse <- rule $ (good (Const $ Scalar BooleanProxy False) <$ token False_)
              <?> "false"
    pure startRule

  -- Parse integer expressions
  IntegerProxy -> mdo
    startRule <- rule rNegate
    rNegate <- rule $ ok (NegI <$> (token Minus *> rSub))
               <|> rSub
               <?> "negation"
    rSub <- rule $ ok (SubI <$> rSub <* token Minus <*> rAdd)
               <|> rAdd
               <?> "subtract"
    rAdd <- rule $ ok (AddI <$> rAdd <* token Plus <*> rMul)
               <|> rMul
               <?> "sum"
    rMul <- rule $ ok (MulI <$> rMul <* token Times <*> rDiv)
               <|> ok (MulI <$> rMul <*> rDiv)
               <|> rDiv
               <?> "product"
    rDiv <- rule $ ok (DivI <$> rDiv <* token Divide <*> rPower)
               <|> rPower
               <?> "quotient"
    rPower <- rule $ ok (PowI <$> rITE <* token Caret <*> rPower)
               <|> rITE
               <?> "exponential"
    boolRule <- valueExpr env BooleanProxy
    rITE <- rule $ ok (ITE IntegerProxy
                            <$> (token If   *> boolRule)
                            <*> (token Then *> startRule)
                            <*> (token Else *> startRule))
             <|> rConst
             <?> "integer-valued if/then/else expression"
    rConst <- rule $ (\(NumberI n) -> good (Const $ Scalar IntegerProxy (fromIntegral n)))
                  <$> satisfy numberI
               <|> rVar
               <?> "integer constant"

    rVar <- rule $ (satisfy isIdentifier <&> \(Identifier n) ->
                       case someSymbolVal n of
                         SomeSymbol name -> case lookupEnv name IntegerProxy env of
                           Found pf  -> good (Var name IntegerProxy pf)
                           Absent _  -> bad ("Unbound variable " <> n)
                           WrongType -> bad ("Mismatched type for variable " <> n)
                   )
                 <|> rParen
                 <?> "integer variable"

    rParen <- rule $ token OpenParen *> startRule <* token CloseParen
               <|> rAbs
               <?> "subexpression"
    rAbs <- rule $ ok (AbsI <$> (token Bar *> startRule <* token Bar))
               <?> "absolute value"
    pure startRule

  -- Parse real expressions
  RealProxy -> mdo
    startRule <- rule rNegate
    rNegate <- rule $ ok (NegF <$> (token Minus *> rSub))
               <|> rSub
               <?> "negation"
    rSub <- rule $ ok (SubF <$> rSub <* token Minus <*> rAdd)
               <|> rAdd
               <?> "subtract"
    rAdd <- rule $ ok (AddF <$> rAdd <* token Plus <*> rMul)
               <|> rMul
               <?> "sum"
    rMul <- rule $ ok (MulF <$> rMul <* token Times <*> rDiv)
               <|> ok (MulF <$> rMul <*> rDiv)
               <|> rDiv
               <?> "product"
    rDiv <- rule $ ok (DivF <$> rDiv <* token Divide <*> rPower)
               <|> rPower
               <?> "quotient"
    rPower <- rule rITE
    --rPower <- rule $ ok (PowF <$> rITE <* token Caret <*> rPower)
    --          <|> rITE
    --           <?> "exponential"
    boolRule <- valueExpr env BooleanProxy
    rITE <- rule $ ok (ITE RealProxy
                            <$> (token If   *> boolRule)
                            <*> (token Then *> startRule)
                            <*> (token Else *> startRule))
             <|> rFunc
             <?> "real-valued if/then/else expression"

    rFunc <- rule $ ((\(toFun -> Just f) arg -> (Fix . f) <$> arg) <$> satisfy isFunction <*> rFunc')
               <|> rAtom
               <?> "real function of one variable"

    rFunc' <- rule $ ((\(toFun -> Just f) arg -> (Fix . f) <$> arg) <$> satisfy isFunction <*> rFunc')
                  <|> ok (MulF <$> (rFunc' <* token Times) <*> rAtom)
                  <|> ok (MulF <$> rFunc' <*> rAtom)
                  <|> rAtom
                  <?> "multiplication of function arguments"


    rAtom <- rule $ (rE <|> rPi <|> rConstI <|> rConst <|> rVar <|> rParen <|> rAbs)
                 <?> "constant, variable, or delimited expression"

    rE <- rule $ (good (Const $ Scalar RealProxy (exp 1)) <$ token Euler)
               <?> "e"
    rPi <- rule $ (good (Const (Scalar RealProxy pi)) <$ token Pi)
               <?> "pi"
    rConstI <- rule $ ((\(NumberI n) -> good $ Const (Scalar RealProxy (fromIntegral n)))
                  <$> satisfy numberI)
               <?> "real constant"
    rConst <- rule $ ((\(NumberF n) -> good $ Const (Scalar RealProxy n))
                  <$> satisfy numberF)
               <?> "real constant"
    rVar <- rule $ (satisfy isIdentifier <&> \(Identifier n) ->
                       case someSymbolVal n of
                         SomeSymbol name -> case lookupEnv name RealProxy env of
                           Found pf  -> good (Var name RealProxy pf)
                           Absent _  -> bad ("Unbound variable " <> n)
                           WrongType -> bad ("Mismatched type for variable " <> n)
                   )
                 <?> "real variable"
    rParen <- rule $ token OpenParen *> startRule <* token CloseParen
               <?> "subexpression"
    rAbs <- rule $ ok (AbsF <$> (token Bar *> startRule <* token Bar))
              <?> "absolute value"

    pure startRule

  -- Parse tuples
  PairProxy t1 t2 -> do
    r1 <- valueExpr env t1
    r2 <- valueExpr env t2
    rule (ok
             (PairV (PairProxy t1 t2)
                <$> (token OpenParen *> r1 <* token Comma)
                <*> (r2 <* token CloseParen)))

  _ -> error "todo"


  where
    numberI = \case { NumberI _ -> True; _ -> False }
    numberF = \case { NumberF _ -> True; _ -> False }
    isIdentifier = \case { Identifier _ -> True; _ -> False }
    isFunction = \case { Function _ -> True; _ -> False }
    toFun :: Token -> Maybe (Value env 'RealT -> ValueF env (Pure1 (Value env)) 'RealT)
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

data ParseResult a
  = AmbiguousParse
  | NoParse
  | ParseError String
  | Ok a
  deriving (Functor, Show, Eq, Ord)

parseValue :: EnvironmentProxy env
           -> ScalarProxy t
           -> String
           -> ParseResult (Value env t)
parseValue env t input =
  case fst (fullParses (parser (valueExpr env t)) (tokenize input)) of
    []  -> NoParse
    [Status p] -> either ParseError Ok p
    _   -> AmbiguousParse
