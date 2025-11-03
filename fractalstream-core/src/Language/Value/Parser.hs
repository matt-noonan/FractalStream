{-# language RecursiveDo, ImpredicativeTypes #-}
module Language.Value.Parser
  ( parseValue
  , parseParsedValue
  , parseInputValue
  , parseType
  , parseEnvironment
  , valueGrammar
  , valueGrammarWithNoSplices
  , atType
  , typeGrammar
  , ParsedValue(..)
  , CheckedValue
  , Splices
  -- * Re-exports
  , TCError
  , ParseError
  , ppFullError
  ) where

import Prelude hiding (LT)
import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Value
import Language.Value.Typecheck
import Language.Parser
import Language.Typecheck
import Language.Parser.Tokenizer

------------------------------------------------------
-- Main functions for parsing Values
------------------------------------------------------

parseType :: String -> Either ParseError SomeType
parseType = fmap (`withType` SomeType) . parse typeGrammar . tokenize

parseEnvironment :: String -> Either ParseError (Map String SomeType)
parseEnvironment = fmap (Map.fromList . map (fmap (`withType` SomeType)))
                 . parse envGrammar . tokenize

parseValue :: forall env ty. (KnownEnvironment env, KnownType ty)
           => Splices
           -> String
           -> Either (Either ParseError TCError) (Value '(env, ty))
parseValue splices input = do
  tv <- first Left (parseParsedValue splices input)
  case atType tv typeProxy of TC x -> first Right x

parseParsedValue :: Map String ParsedValue
                 -> String
                 -> Either ParseError ParsedValue
parseParsedValue splices = parse (valueGrammar splices) . tokenize

parseInputValue :: forall env ty. (KnownEnvironment env, KnownType ty)
                => Splices
                -> String
                -> Either (Either ParseError TCError) (Value '(env, ty))
parseInputValue splices input = case typeProxy @ty of
  TextType     -> parseValue splices (show input)
  ListType ity -> case filter (not . (flip (elem @[]) " \t\n\r")) input of
    "" -> pure (List ity [])
    _  -> parseValue splices ("[" ++ input ++ "]")
  _ -> parseValue splices input

------------------------------------------------------
-- Type and Value grammars
------------------------------------------------------

envGrammar :: forall r. Grammar r (Prod r [(String, FSType)])
envGrammar = mdo

  toplevel <- ruleChoice
    [ pure []
    , (:) <$> oneVar <*> many (token Comma *> oneVar)
    ]

  oneVar <- rule ((,) <$> (ident <* token Colon) <*> typ)

  typ <- typeGrammar

  pure toplevel

typeGrammar :: forall r. Grammar r (Prod r FSType)
typeGrammar = mdo

  toplevel <- ruleChoice
    [ productType
    , listType
    ]

  listType <- rule
    (ListT <$> (token (Identifier "List") *> token (Identifier "of") *> listType'))

  listType' <- ruleChoice [ listType, baseType ]

  productType <- ruleChoice
    [ Pair <$> (baseType <* token (Identifier "x")) <*> productType
    , baseType
    ]

  baseType <- ruleChoice
    [ scalarType
    , (token OpenParen *> toplevel <* token CloseParen)
    ]

  scalarType <- rule $ tokenMatch $ \case
    Identifier t -> Map.lookup t types
    _ -> Nothing

  pure toplevel

valueGrammarWithNoSplices :: forall r. Grammar r (Prod r ParsedValue)
valueGrammarWithNoSplices = valueGrammar Map.empty

valueGrammar :: forall r
              . Splices
             -> Grammar r (Prod r ParsedValue)
valueGrammar splices = mdo

  let toplevel = cast

  cast <- ruleChoice
    [ check (tcCast <$> ite <*> (token Colon *> typ))
    , ite
    ]

  ite <- ruleChoice
    [ check (tcITE <$> (token If *> scalar)
                             <*> (token Then *> scalar)
                             <*> (token Else *> ite))
    , scalar
    ]

  scalar <- ruleChoice [ list, pair ]

  list <- rule (check (tcList <$> (token OpenBracket *> list' <* token CloseBracket)))

  list' <- ruleChoice
    [ (:) <$> pOr <*> many (token Comma *> pOr)
    , pure []
    ]

  pair <- ruleChoice
    [ check (tcPair <$> (ite <* token Comma) <*> pair)
    , pOr
    ]

  pOr <- ruleChoice
    [ check (tcOr <$> (pAnd <* token Or_) <*> pOr <?> "disjunction")
    , pAnd
    ]

  pAnd <- ruleChoice
    [ check (tcAnd <$> (pNot <* token And_) <*> pAnd <?> "conjunction")
    , pNot
    ]

  pNot <- ruleChoice
    [ check (tcNot <$> (token Not_ *> arith))
    , comparison
    ]

  comparison <- ruleChoice
    [ check (tcEql <$> (arith <* token ExactlyEqual) <*> arith)
    , check (tcAppxEql splices <$> (arith <* token Equal) <*> arith)
    , check (tcAppxNEq splices <$> (arith <* token NotEqual) <*> arith)
    , check (tcLT  <$> (arith <* token LessThan) <*> arith)
    , check (tcGT  <$> (arith <* token GreaterThan) <*> arith)
    , check (tcGTE <$> (arith <* token GreaterThanOrEqual) <*> arith)
    , check (tcLTE <$> (arith <* token LessThanOrEqual) <*> arith)
    , check (tcEscapes splices <$> (arith <* token Escapes))
    , check (tcVanishes splices <$> (arith <* token Vanishes))
    , arith
    ]

  let arith = addOrSub

  addOrSub <- ruleChoice
    [ check (tcAdd <$> (addOrSub <* token Plus)  <*> mulOrDiv)
    , check (tcSub <$> (addOrSub <* token Minus) <*> mulOrDiv)
    , mulOrDiv
    ]

  mulOrDiv <- ruleChoice
    [ check (tcMul <$> (mulOrDiv <* token Times) <*> negated)
    , check (tcMul <$> concatenatedFunAps <*> funAp)
    , check (tcMul <$> concatenatedAtoms <*> noFunPower)
    , check (tcNeg <$> (token Minus *> concatenatedAtoms'))
    , check (tcDiv <$> (mulOrDiv <* token Divide) <*> negated)
    , check (tcIDiv <$> (mulOrDiv <* token IntegerDivide) <*> negated)
    , negated
    ]

  concatenatedAtoms' <- rule $ check
    (tcMul <$> concatenatedAtoms <*> noFunPower)

  concatenatedAtoms <- ruleChoice
    [ check (tcMul <$> concatenatedAtoms <*> noFunPower)
    , noFunPower
    , check (funAp $> \sr _ ->
                advise sr "To avoid ambiguity when using implicit multiplication, functions must go to the left of other values")
    ]

  concatenatedFunAps <- ruleChoice
    [ check (tcMul <$> concatenatedFunAps <*> funAp)
    , check (tcMul <$> concatenatedAtoms <*> atom)
    , funAp
    , atom
    ]

  funAp <- ruleChoice
    [ check (tcCommonFun
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n commonFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , check (tcRealFun
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n realFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)

    , check (tcComplexFun
        <$> (tokenMatch (\case { Identifier n -> (n,) <$> Map.lookup n complexFunctions;
                                 _ -> Nothing }))
        <*> atomOrFunAp)
    , check (tcMod
        <$> (token (Identifier "mod") *> token OpenParen *> arith)
        <*> (token Comma *> arith <* token CloseParen))
    ]

  negated <- ruleChoice
    [ check (tcNeg <$> (token Minus *> negated))
    , power
    ]

  power <- ruleChoice
    [ check (tcPow <$> (atomOrFunAp <* token Caret) <*> power)
    , atomOrFunAp
    ]

  noFunPower <- ruleChoice
    [ check (tcPow <$> (atom <* token Caret) <*> power)
    , atom
    ]

  atomOrFunAp <- ruleChoice [funAp, atom]

  atom <- ruleChoice
    [ simpleAtom
    , token OpenParen   *> toplevel <* token CloseParen
    , token OpenBrace   *> toplevel <* token CloseBrace
    ]

  simpleAtom <- ruleChoice
    [ check (tcScalar "`true`"  BooleanType True  <$ token True_)
    , check (tcScalar "`false`" BooleanType False <$ token False_)
    , check (tcScalar "𝑖" ComplexType (0.0 :+ 1.0) <$ token I)
    , check (tcScalar "𝑒" RealType (exp 1.0) <$ token Euler)
    , check (tcScalar "π" RealType pi <$ token Pi)
    , check (tcScalar "text" TextType <$> quoted)
    , check (tcIterations splices <$ token Iterations)
    , check (tcStuck splices <$ token Stuck)
    , check (
        (\n -> tcScalar (show n) IntegerType (fromIntegral n))
        <$> tokenMatch (\case { NumberI n -> Just n; _ -> Nothing }))
    , check (
        (\n -> tcScalar (show n) RealType n)
        <$> tokenMatch (\case { NumberF n -> Just n; _ -> Nothing }))
    , check (tcAbs <$> (token Bar *> arith <* token Bar))

    , splicedValue

    , textConcat
    , var
    , color
    , listOp
    ]

  quoted <- rule $ tokenMatch (\case { Quoted txt -> Just txt; _ -> Nothing })

  splicedValue <- rule $
    token OpenSplice *>
    (tokenMatch $ \case { Identifier n -> Map.lookup n splices; _ -> Nothing })
    <* token CloseSplice

  var <- rule $ check (
    fmap tcVar . tokenMatch $ \case
        Identifier n | not (reserved n) -> Just n;
        _ -> Nothing)

  color <- ruleChoice
    [ check (
        fmap (\(n,c) -> tcScalar n ColorType c) . tokenMatch $ \case
            Identifier n -> ("`" ++ n ++ "`",) <$> Map.lookup n colors
            _ -> Nothing)

    , check (tcDark <$> (token (Identifier "dark") *> atom))
    , check (tcLight <$> (token (Identifier "light") *> atom))
    , check (tcInvert <$> (token (Identifier "invert") *> atom))
    , check (
        tcBlend <$> (token (Identifier "blend") *> (token OpenParen *> arith))
                <*> (token Comma *> atom)
                <*> (token Comma *> atom <* token CloseParen))
    , check (
        tcCycle <$> (token (Identifier "cycle") *> (token OpenParen *> arith))
                <*> (token Comma *> atom)
                <*> (token Comma *> atom <* token CloseParen))
    , check (
        tcRainbow <$> (token (Identifier "rainbow") *>
                       (token OpenParen *> arith <* token CloseParen)))
    , check (
        tcArenberg <$> (token (Identifier "arenberg") *>
                       (token OpenParen *> arith <* token CloseParen)))
    , check (
        tcRGB <$> (token (Identifier "rgb") *> (token OpenParen *> arith))
              <*> (token Comma *> arith)
              <*> (token Comma *> arith <* token CloseParen))
    ]

  textConcat <- rule $ check $
    tcText <$> ((:) <$> (token TextKeyword *> token OpenParen *> toplevel)
                    <*> (some (token Comma *> toplevel) <* token CloseParen))

  listOp <- ruleChoice
    [ check (tcJoin <$>
             ((:) <$> (token JoinKeyword *> token OpenParen *> toplevel)
                  <*> (some (token Comma *> toplevel) <* token CloseParen)))
    , check (tcAppend <$> (token AppendKeyword *> token OpenParen *> toplevel)
                      <*> (token Comma *> toplevel <* token CloseParen))
    , check (tcPrepend <$> (token PrependKeyword *> token OpenParen *> toplevel)
                       <*> (token Comma *> toplevel <* token CloseParen))
    , check (tcRemove <$> (token RemoveKeyword *> token OpenParen *> toplevel)
                      <*> (token Comma *> ident <* token RightArrow)
                      <*> (toplevel <* token CloseParen))
    , check (tcFind <$> (token FindKeyword *> token OpenParen *> toplevel)
                    <*> (token Comma *> ident <* token RightArrow)
                    <*> toplevel
                    <*> (token Comma *> toplevel <* token CloseParen))
    , check (tcTransform <$> (token TransformKeyword *> token OpenParen *> toplevel)
                         <*> (token Comma *> ident <* token RightArrow)
                         <*> (toplevel <* token CloseParen))
    , check (tcRange <$> (token RangeKeyword *> token OpenParen *> toplevel)
                     <*> (token Comma *> toplevel <* token CloseParen))
    , check (tcLength <$>
             (token LengthKeyword *> token OpenParen *> toplevel <* token CloseParen))
    , check (tcIndex False <$> (toplevel <* token At) <*> toplevel)
    , check (tcIndex True  <$> (toplevel <* token AtAt) <*> toplevel)
    ]

  typ <- typeGrammar

  let reserved = (`Set.member` reservedWords)
      reservedWords = Set.fromList
        [ "dark", "light", "invert", "blend", "cycle", "rainbow", "arenberg", "rgb", "mod"]
        `Set.union` Map.keysSet colors
        `Set.union` Map.keysSet commonFunctions
        `Set.union` Map.keysSet realFunctions
        `Set.union` Map.keysSet complexFunctions
        `Set.union` Map.keysSet splices

  pure toplevel

check :: Prod r CheckedValue -> Prod r ParsedValue
check = withSourceRange
      . fmap @_ @CheckedValue (\v sr -> ParsedValue sr (\ty -> withKnownType ty $ v sr ty))
