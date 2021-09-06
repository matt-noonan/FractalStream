module Language.Value.Parser
  ( parseValue
  , parseValueFromTokens
  , BadParse(..)
  , value_
  , tokenize
  , Token(..)
  ) where

import Language.Type
import Language.Value
import Language.Parser
import Data.Indexed.Functor
import Fcf (Pure1)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.TypeLits

funR2R :: Map String (Value env 'RealT -> Value env 'RealT)
funR2R = Map.fromList
  [ ("exp", Fix . ExpF), ("log", Fix . LogF), ("abs", Fix . AbsF)
  , ("cos", Fix . CosF), ("sin", Fix . SinF), ("tan", Fix . TanF)
  , ("arccos", Fix . ArccosF), ("arcsin", Fix . ArcsinF), ("arctan", Fix . ArctanF)
  , ("acos", Fix . ArccosF), ("asin", Fix . ArcsinF), ("atan", Fix . ArctanF)
  , ("cosh", Fix . CoshF), ("sinh", Fix . SinhF), ("tanh", Fix . TanhF)
  , ("arccosh", Fix . ArccoshF), ("arcsinh", Fix . ArcsinhF), ("arctanh", Fix . ArctanhF)
  , ("acosh", Fix . ArccoshF), ("asinh", Fix . ArcsinhF), ("atanh", Fix . ArctanhF)
  ]

funC2C :: Map String (Value env 'ComplexT -> Value env 'ComplexT)
funC2C = Map.fromList
  [ ("exp", Fix . ExpC), ("log", Fix . LogC)
  , ("cos", Fix . CosC), ("sin", Fix . SinC), ("tan", Fix . TanC)
  , ("cosh", Fix . CoshC), ("sinh", Fix . SinhC), ("tanh", Fix . TanhC)
  , ("bar", Fix . ConjC), ("conj", Fix . ConjC)
  ]

funC2R :: Map String (Value env 'ComplexT -> Value env 'RealT)
funC2R = Map.fromList
  [ ("re", Fix . ReC), ("im", Fix . ImC), ("arg", Fix . ArgC) ]

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


good :: ValueF env (Pure1 (Value env)) t -> Parser (Value env t)
good = pure . Fix

ok :: Parser (ValueF env (Pure1 (Value env)) t) -> Parser (Value env t)
ok = (>>= good)

---------------------------------------------------------------------------------
-- Parse typed values
---------------------------------------------------------------------------------

value_ :: forall t env
            . (KnownEnvironment env, KnownType t)
           => Parser (Value env t)
value_ = case typeProxy @t of
  VoidProxy     -> fail "no values of void type"
  BooleanProxy  -> boolValue_
  IntegerProxy  -> intValue_
  RealProxy     -> realValue_
  ComplexProxy  -> cplxValue_
  PairProxy x y -> pairValue_ x y
  p -> error ("todo: value @" <> showType p)

valueAtType_ :: forall t env
                . KnownEnvironment env
               => ScalarProxy t
               -> Parser (Value env t)
valueAtType_ t = withKnownType t (value_ @t)

atom_ :: forall t env
       . ( KnownEnvironment env, KnownType t )
      => Parser (Value env t)
atom_ = case typeProxy @t of
  VoidProxy     -> fail "no values of void type"
  BooleanProxy  -> boolAtom_
  IntegerProxy  -> intAtom_
  RealProxy     -> realAtom_
  ComplexProxy  -> cplxAtom_
  PairProxy x y -> pairValue_ x y
  p -> error ("todo: atom @" <> showType p)

---------------------------------------------------------------------------------
-- Parse boolean values
---------------------------------------------------------------------------------

boolValue_ :: forall env. KnownEnvironment env => Parser (Value env 'BooleanT)
boolValue_ = boolOr_
  where
    boolOr_ = do
      lhs <- boolAnd_
      ok (Or lhs <$> (tok_ Or_ *> boolOr_))
        <|> pure lhs
        <?> "boolean disjunction"
    boolAnd_ = do
      lhs <- boolNot_
      ok (And lhs <$> (tok_ And_ *> boolAnd_))
        <|> pure lhs
        <?> "boolean conjunction"
    boolNot_ = ok (Not <$> (tok_ Not_ *> boolNot_))
               <|> boolITE_
               <?> "boolean negation"
    boolITE_ = ok (ITE BooleanProxy
                   <$> (tok_ If   *> boolValue_)
                   <*> (tok_ Then *> boolValue_)
                   <*> (tok_ Else *> boolValue_))
               <|> boolAtom_
               <?> "boolean-value if/then/else expression"

boolAtom_ :: forall env. KnownEnvironment env => Parser (Value env 'BooleanT)
boolAtom_ = boolParen_ <|> boolTrue_ <|> boolFalse_ <|> boolVar_
  where
    boolParen_ = (tok_ OpenParen *> boolValue_ <* tok_ CloseParen)
                 <?> "parenthesized boolean expression"
    boolTrue_  = ok (Const (Scalar BooleanProxy True) <$ tok_ True_)
                  <?> "true"
    boolFalse_ = ok (Const (Scalar BooleanProxy False) <$ tok_ False_)
                  <?> "false"
    boolVar_ =
      (satisfy (\case { Identifier _ -> True; _ -> False }) >>=
        \(Identifier n) ->
          case someSymbolVal n of
            SomeSymbol name ->
              case lookupEnv name BooleanProxy (envProxy (Proxy @env)) of
                Found pf  -> good (Var name BooleanProxy pf)
                Absent _  -> bad (UnboundVariable n)
                WrongType -> bad (MismatchedType n)
      ) <?> "boolean variable"

---------------------------------------------------------------------------------
-- Parse pair-typed values
---------------------------------------------------------------------------------

pairValue_ :: forall t1 t2 env
            . ( KnownEnvironment env )
           => ScalarProxy t1
           -> ScalarProxy t2
           -> Parser (Value env ('Pair t1 t2))
pairValue_ t1 t2 = (withKnownType t1 $ withKnownType t2 $
  ok
    (PairV (PairProxy t1 t2)
      <$> (tok_ OpenParen *> value_ <* tok_ Comma)
      <*> (value_ <* tok_ CloseParen)))
  <?> "pair"

---------------------------------------------------------------------------------
-- Parse basic arithmetic expressions (shared between integers, reals,
-- and complex numbers)
---------------------------------------------------------------------------------

data ArithOps env t = ArithOps
    { opAdd :: Value env t -> Value env t -> ValueF env (Pure1 (Value env)) t
    , opSub :: Value env t -> Value env t -> ValueF env (Pure1 (Value env)) t
    , opMul :: Value env t -> Value env t -> ValueF env (Pure1 (Value env)) t
    , opDiv :: Value env t -> Value env t -> ValueF env (Pure1 (Value env)) t
    , opPow :: Value env t -> Value env t -> ValueF env (Pure1 (Value env)) t
    , opAbs :: Value env t -> ValueF env (Pure1 (Value env)) (OpAbsResultType t)
    , opNeg :: Value env t -> ValueF env (Pure1 (Value env)) t
    , opType :: ScalarProxy t
    }

type family OpAbsResultType (t :: Type) :: Type where
  OpAbsResultType 'IntegerT = 'IntegerT
  OpAbsResultType 'RealT    = 'RealT
  OpAbsResultType 'ComplexT = 'RealT

arithValue_ :: forall env t
             . ( KnownEnvironment env, KnownType t )
            => ArithOps env t
            -> Parser (Value env t)
arithValue_ ArithOps{..} = sub_
  where
    sub_ = do
      -- To get the associativity correct, gather
      -- up a bunch of "add" expressions with -
      -- between them, then use a left fold to get
      -- (((x1 - x2) - x3) - x4) - ...
      args <- add_ `sepBy1` tok_ Minus
      pure (foldl1 (\l r -> Fix (opSub l r)) args)
        <?> "subtraction"
    add_ = do
      lhs <- div_
      ok (opAdd lhs <$> (tok_ Plus *> add_))
        <|> pure lhs
        <?> "addition"
    div_ = do
      lhs <- mul_
      ok (opDiv lhs <$> (tok_ Divide *> mul_))
        <|> pure lhs
        <?> "division"
    mul_ = do
      lhs <- neg_
      ok (opMul lhs <$> (tok_ Times *> mul_))
        <|> ok (opMul lhs <$> try mulNoNeg_) -- multiplication by concatenation
        <|> pure lhs
        <?> "multiplication"
    mulNoNeg_ = do
      lhs <- pow_
      ok (opMul lhs <$> (tok_ Times *> mul_))
        <|> ok (opMul lhs <$> try mulNoNeg_) -- multiplication by concatenation
        <|> pure lhs
        <?> "multiplication"
    neg_ = ok (opNeg <$> (tok_ Minus *> atom_))
           <|> pow_
           <?> "negation"
    pow_ = do
      lhs <- ite_
      ok (opPow lhs <$> (tok_ Caret *> pow_))
        <|> pure lhs
        <?> "power"
    ite_ = ok (ITE opType
                <$> (tok_ If   *> boolValue_)
                <*> (tok_ Then *> valueAtType_ opType)
                <*> (tok_ Else *> valueAtType_ opType))
           <|> atom_ @t

---------------------------------------------------------------------------------
-- Parse integer expressions
---------------------------------------------------------------------------------

intOps :: ArithOps env 'IntegerT
intOps = ArithOps
  { opType = IntegerProxy
  , opAdd = AddI, opSub = SubI, opMul = MulI, opDiv = DivI
  , opPow = PowI, opAbs = AbsI, opNeg = NegI }

intValue_ :: forall env. KnownEnvironment env => Parser (Value env 'IntegerT)
intValue_ = arithValue_ intOps <|> atom_

intAtom_ :: forall env. KnownEnvironment env => Parser (Value env 'IntegerT)
intAtom_ = intConst_ <|> intParen_ {- <|> intAbs_ -} <|> intVar_
  where
    intConst_ = (\(NumberI n) -> Fix (Const $ Scalar IntegerProxy (fromIntegral n)))
                <$> satisfy (\case { NumberI _ -> True; _ -> False })
                <?> "integer constant"

    intVar_ =
      (satisfy (\case { Identifier _ -> True; _ -> False }) >>=
        \(Identifier n) ->
          case someSymbolVal n of
            SomeSymbol name -> case lookupEnv name IntegerProxy (envProxy (Proxy @env)) of
                                 Found pf  -> good (Var name IntegerProxy pf)
                                 Absent _  -> bad (UnboundVariable n)
                                 WrongType -> bad (MismatchedType n))
      <?> "integer variable"

    intParen_ = tok_ OpenParen *> intValue_ @env <* tok_ CloseParen
                <?> "subexpression"
--    intAbs_ = ok (opAbs intOps <$> (tok_ Bar *> valueAtType_ opAbsResultType <* tok_ Bar))
--              <?> "absolute value"

---------------------------------------------------------------------------------
-- Parse real expressions
---------------------------------------------------------------------------------

realOps :: ArithOps env 'RealT
realOps = ArithOps
  { opType = RealProxy
  , opAdd = AddF, opSub = SubF, opMul = MulF, opDiv = DivF
  , opPow = PowF, opAbs = AbsF, opNeg = NegF }

realValue_ :: forall env. KnownEnvironment env => Parser (Value env 'RealT)
realValue_ = arithValue_ realOps <|> atom_

realAtom_ :: forall env. KnownEnvironment env => Parser (Value env 'RealT)
realAtom_
    =   realConst_ <|> realConstI_ <|> e_ <|> pi_
    <|> realParen_
    <|> someAbs_
    <|> try realFun_
    <|> realVar_
  where
    e_  = (Fix $ Const $ Scalar RealProxy (exp 1)) <$ tok_ Euler
    pi_ = (Fix $ Const $ Scalar RealProxy pi)      <$ tok_ Pi

    realConstI_ = (\(NumberI n) -> Fix (Const $ Scalar RealProxy (fromIntegral n)))
                  <$> satisfy (\case { NumberI _ -> True; _ -> False })
                  <?> "real constant"

    realConst_ = (\(NumberF n) -> Fix (Const $ Scalar RealProxy n))
                <$> satisfy (\case { NumberF _ -> True; _ -> False })
                <?> "real constant"

    realVar_ =
      (satisfy (\case { Identifier _ -> True; _ -> False }) >>=
        \(Identifier n) ->
          case someSymbolVal n of
            SomeSymbol name -> case lookupEnv name RealProxy (envProxy (Proxy @env)) of
                                 Found pf  -> good (Var name RealProxy pf)
                                 Absent _  -> bad (UnboundVariable n)
                                 WrongType -> bad (MismatchedType n))
      <?> "real variable"

    realParen_ = tok_ OpenParen *> realValue_ @env <* tok_ CloseParen
                <?> "subexpression"

    someAbs_ = try realAbs_ <|> cplxAbs_

    realAbs_ = case realOps of
      ArithOps{..} ->
        ok (opAbs <$> (tok_ Bar *> realValue_ @env <* tok_ Bar))
               <?> "absolute value"

    cplxAbs_ = case cplxOps of
      ArithOps{..} ->
        ok (opAbs <$> (tok_ Bar *> cplxValue_ @env <* tok_ Bar))
               <?> "complex magnitude"

    funNames = Map.keysSet funR2R `Set.union` Map.keysSet funC2R

    realFun_ =
      (satisfy (\case { Identifier f -> Set.member f funNames; _ -> False }) >>=
        \(Identifier f) -> case Map.lookup f funR2R of
          Just fun -> fun <$> realAtom_
          Nothing -> case Map.lookup f funC2R of
            Just fun -> fun <$> cplxAtom_
            Nothing -> error "internal error, should be unreachable")
      <?> "function"

---------------------------------------------------------------------------------
-- Parse complex expressions
---------------------------------------------------------------------------------

cplxOps :: ArithOps env 'ComplexT
cplxOps = ArithOps
  { opType = ComplexProxy
  , opAdd = AddC, opSub = SubC, opMul = MulC, opDiv = DivC
  , opPow = PowC, opAbs = AbsC, opNeg = NegC }

cplxValue_ :: forall env. KnownEnvironment env => Parser (Value env 'ComplexT)
cplxValue_ = arithValue_ cplxOps <|> atom_

cplxAtom_ :: forall env. KnownEnvironment env => Parser (Value env 'ComplexT)
cplxAtom_
    =   cplxConst_
    <|> cplxParen_
    <|> try cplxFun_
    <|> cplxVar_
  where
    cplxConst_ :: Parser (Value env 'ComplexT)
    cplxConst_ = (satisfy (\case { NumberI _ -> True; _ -> False }) <&>
                   \(NumberI n) -> Fix (Const $ Scalar ComplexProxy (fromIntegral n)))
                <?> "complex constant"

    cplxVar_ =
      (satisfy (\case { Identifier _ -> True; _ -> False }) >>=
        \(Identifier n) ->
          case someSymbolVal n of
            SomeSymbol name -> case lookupEnv name ComplexProxy (envProxy (Proxy @env)) of
                                 Found pf  -> good (Var name ComplexProxy pf)
                                 Absent _  -> bad (UnboundVariable n)
                                 WrongType -> bad (MismatchedType n))
      <?> "complex variable"

    cplxParen_ = tok_ OpenParen *> cplxValue_ @env <* tok_ CloseParen
                <?> "subexpression"

    funNames = Map.keysSet funC2C

    cplxFun_ =
      (satisfy (\case { Identifier f -> Set.member f funNames; _ -> False }) >>=
        \(Identifier f) -> case Map.lookup f funC2C of
          Just fun -> fun <$> cplxAtom_
          Nothing -> error "internal error, should be unreachable")
      <?> "function"

parseValue :: EnvironmentProxy env
           -> ScalarProxy t
           -> String
           -> Either (Int, BadParse) (Value env t)

parseValue env t input = parseValueFromTokens env t (tokenize input)

parseValueFromTokens
  :: EnvironmentProxy env
  -> ScalarProxy t
  -> [Token]
  -> Either (Int, BadParse) (Value env t)
parseValueFromTokens env t toks =
   withEnvironment env (withKnownType t (parse value_ toks))
