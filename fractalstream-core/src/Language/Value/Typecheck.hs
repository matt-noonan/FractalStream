module Language.Value.Typecheck where

import Prelude hiding (LT)
import FractalStream.Prelude

import qualified Data.Map as Map

import Language.Value
import Language.Typecheck
import Language.Parser.SourceRange

import Data.Color

------------------------------------------------------
-- Parsed values
------------------------------------------------------

-- | Parsed values which have not yet been checked for
-- type / scope correctness.
data ParsedValue = ParsedValue SourceRange
  (forall env ty. (KnownEnvironment env, KnownType ty)
    => TypeProxy ty
    -> TC (Value '(env, ty)))

type Splices = Map String ParsedValue

-- | Values which have been checked for type and scope
-- correctness.
type CheckedValue = SourceRange -> (forall env ty. (KnownEnvironment env, KnownType ty) => TypeProxy ty -> TC (Value '(env,ty)))

-- | Try to interpret the value at the given type, inserting casts if needed.
atType :: (KnownEnvironment env, KnownType ty)
       => ParsedValue
       -> TypeProxy ty
       -> TC (Value '(env, ty))
atType v@(ParsedValue sr f) ty = case ty of
  TextType    -> tryEachType (Advice sr "I don't know how to turn this type of value into text.")
    [ f TextType
    , ToText IntegerType <$> atType v IntegerType
    , ToText RealType    <$> atType v RealType
    , ToText ComplexType <$> atType v ComplexType
    ]
  RealType    -> catchError (I2R <$> atType v IntegerType) (\_ -> f RealType)
  ComplexType -> catchError (R2C <$> atType v RealType) (\_ -> f ComplexType)
  _           -> f ty

------------------------------------------------------
-- Value typechecking
------------------------------------------------------

tcCast :: ParsedValue -> FSType -> CheckedValue
tcCast v tgt sr ty = withType tgt $ \tgtTy ->
  case sameHaskellType ty tgtTy of
    Nothing -> throwError (BadConversion sr (SomeType tgtTy) (Expected $ SomeType ty))
    Just Refl -> case ty of
      RealType -> tryEachType (Advice sr "Could not convert to a real number")
        [ atType v RealType
        , I2R <$> atType v IntegerType
        ]
      ComplexType -> tryEachType (Advice sr "Could not convert to a complex number")
        [ atType v ComplexType
        , R2C <$> atType v RealType
        , R2C . I2R <$> atType v IntegerType
        ]
      _ -> atType v ty

tcPair :: ParsedValue -> ParsedValue -> CheckedValue
tcPair lhs rhs sr ty = case ty of
  PairType t1 t2 -> PairV ty <$> atType lhs t1 <*> atType rhs t2
  _ -> throwError (Surprise sr "an ordered pair" "of some product type" (Expected $ an ty))

tcITE :: ParsedValue -> ParsedValue -> ParsedValue -> CheckedValue
tcITE cond yes no _sr ty =
  ITE ty <$> atType cond BooleanType <*> atType yes ty <*> atType no ty

tcOr, tcAnd :: ParsedValue -> ParsedValue -> CheckedValue
tcOr lhs rhs sr = \case
  BooleanType -> Or <$> atType lhs BooleanType <*> atType rhs BooleanType
  ty -> throwError (Surprise sr "the result of a disjunction" "a truth value" (Expected $ an ty))

tcAnd lhs rhs sr = \case
  BooleanType -> And <$> atType lhs BooleanType <*> atType rhs BooleanType
  ty -> throwError (Surprise sr "the result of a conjunction" "a truth value" (Expected $ an ty))

tcNot :: ParsedValue -> CheckedValue
tcNot arg sr = \case
  BooleanType -> Not <$> atType arg BooleanType
  ty -> throwError (Surprise sr "the result of a logical negation" "a truth value" (Expected $ an ty))

tcEql, tcLT, tcLTE, tcGT, tcGTE :: ParsedValue -> ParsedValue -> CheckedValue

tcEql lhs rhs sr = \case
  BooleanType -> tryEachType (Advice sr "Arguments to ≡ must be real or complex numbers.")
    [ Eql RealType    <$> atType lhs RealType    <*> atType rhs RealType
    , Eql ComplexType <$> atType lhs ComplexType <*> atType rhs ComplexType
    ]
  ty -> throwError (Surprise sr "the result of an equality check" "a truth value" (Expected $ an ty))

tcLT lhs rhs sr = \case
  BooleanType -> tryEachType (Advice sr "Arguments to a comparison must be integers or real numbers.")
    [ LTF <$> atType lhs RealType    <*> atType rhs RealType
    , LTI <$> atType lhs IntegerType <*> atType rhs IntegerType
    ]
  ty -> throwError (Surprise sr "the result of a comparison" "a truth value" (Expected $ an ty))

tcGT lhs rhs sr = \case
  BooleanType -> tryEachType (Advice sr "Arguments to a comparison must be integers or real numbers.")
    [ LTF <$> atType rhs RealType    <*> atType lhs RealType
    , LTI <$> atType rhs IntegerType <*> atType lhs IntegerType
    ]
  ty -> throwError (Surprise sr "the result of a comparison" "a truth value" (Expected $ an ty))

tcGTE lhs rhs sr = \case
  BooleanType -> tryEachType (Advice sr "Arguments to a comparison must be integers or real numbers.")
    [ Not <$> (LTF <$> atType lhs RealType    <*> atType rhs RealType)
    , Not <$> (LTI <$> atType lhs IntegerType <*> atType rhs IntegerType)
    ]
  ty -> throwError (Surprise sr "the result of a comparison" "a truth value" (Expected $ an ty))

tcLTE lhs rhs sr = \case
  BooleanType -> tryEachType (Advice sr "Arguments to a comparison must be integers or real numbers.")
    [ Not <$> (LTF <$> atType rhs RealType    <*> atType lhs RealType)
    , Not <$> (LTI <$> atType rhs IntegerType <*> atType lhs IntegerType)
    ]
  ty -> throwError (Surprise sr "the result of a comparison" "a truth value" (Expected $ an ty))

tcAppxEql, tcAppxNEq :: Splices -> ParsedValue -> ParsedValue -> CheckedValue

tcAppxEql splices lhs rhs sr = \case
  BooleanType -> tryEachType (Advice sr "Arguments to ≠ must be of some comparable type.")
    [ Eql IntegerType <$> atType lhs IntegerType <*> atType rhs IntegerType
    , Eql BooleanType <$> atType lhs BooleanType <*> atType rhs BooleanType
    , let delta = ParsedValue sr (tcSub lhs rhs sr)
          norm  = ParsedValue sr (tcAbs delta sr)
      in tcVanishes splices norm sr BooleanType
    ]
  ty -> throwError (Surprise sr "the result of an equality check" "a truth value" (Expected $ an ty))

tcAppxNEq splices lhs rhs sr = \case
  BooleanType -> tryEachType (Advice sr "Arguments to = must be of some comparable type.")
    [ NEq IntegerType <$> atType lhs IntegerType <*> atType rhs IntegerType
    , NEq BooleanType <$> atType lhs BooleanType <*> atType rhs BooleanType
    , let delta = ParsedValue sr (tcSub lhs rhs sr)
          norm  = ParsedValue sr (tcAbs delta sr)
      in Not <$> tcVanishes splices norm sr BooleanType
    ]
  ty -> throwError (Surprise sr "the result of an inequality check" "a truth value" (Expected $ an ty))

-- | Do an arithmetic operation, but try to keep conversions at the outermost
-- layer. For example, if we want to parse "1 + 2" at ComplexT, we want the
-- addition to happen over integers with the conversion to complex happening last.
-- To do this, we'll try to interpret "1 + 2" as a integer expression followed by
-- a conversion. If that fails, we'll try it as a real expression followed by a
-- conversion. And finally if that fails, we'll try it as a complex expression.
type ArithFun t = forall env value. KnownEnvironment env => Eval (value '(env, t)) -> Eval (value '(env, t)) -> ValueF value '(env, t)

tcArith :: ArithFun 'IntegerT
        -> ArithFun 'RealT
        -> ArithFun 'ComplexT
        -> ParsedValue
        -> ParsedValue
        -> CheckedValue
tcArith makeI makeF makeC lhs rhs sr = \case
  IntegerType -> makeI <$> atType lhs IntegerType <*> atType rhs IntegerType
  RealType    -> makeF <$> atType lhs RealType    <*> atType rhs RealType
  ComplexType -> makeC <$> atType lhs ComplexType <*> atType rhs ComplexType
  ty -> throwError (Surprise sr "the result of an arithmetic operation" "of some numeric type" (Expected $ an ty))

tcAdd, tcSub, tcMul, tcDiv, tcIDiv, tcPow :: ParsedValue -> ParsedValue -> CheckedValue

tcAdd = tcArith AddI AddF AddC
tcSub = tcArith SubI SubF SubC
tcMul = tcArith MulI MulF MulC
tcPow = tcArith PowI PowF PowC

tcDiv lhs rhs sr = \case
  IntegerType -> advise sr "For integer division, use the // operator."
  RealType    -> DivF <$> atType lhs RealType <*> atType rhs RealType
  ComplexType -> DivC <$> atType lhs ComplexType <*> atType rhs ComplexType
  ty -> throwError (Surprise sr "the result of /" "of some numeric type" (Expected $ an ty))

tcIDiv lhs rhs sr = \case
  IntegerType -> DivI <$> atType lhs IntegerType <*> atType rhs IntegerType
  ty -> throwError (Surprise sr "the result of //" "an integer" (Expected $ an ty))

tcNeg :: ParsedValue -> CheckedValue
tcNeg x sr = \case
  IntegerType -> NegI <$> atType x IntegerType
  RealType    -> NegF <$> atType x RealType
  ComplexType -> NegC <$> atType x ComplexType
  ty -> throwError (Surprise sr "the result of negation" "of some numeric type" (Expected $ an ty))

tcScalar :: String -> TypeProxy ty -> HaskellType ty -> CheckedValue
tcScalar name ty' v sr ty = case sameHaskellType ty ty' of
  Just Refl -> pure (Const $ Scalar ty v)
  Nothing   -> throwError (Surprise sr name (an $ SomeType ty') (Expected $ an ty))

tcIterations :: Splices -> CheckedValue
tcIterations splices sr ty = case Map.lookup internalIterations splices of
  Nothing -> throwError $ internal
    (Advice sr "The script's internal iteration counter was not defined.")
  Just r  -> atType r ty

tcStuck :: Splices -> CheckedValue
tcStuck splices sr ty = case Map.lookup internalStuck splices of
  Just (ParsedValue _ stuck) -> ParsedValue sr stuck `atType` ty
  Nothing -> throwError (internal $ Advice sr "The script's internal `stuck` status variable was not defined.")

tcAbs :: ParsedValue -> CheckedValue
tcAbs x sr = \case
  IntegerType -> AbsI <$> atType x IntegerType
  RealType -> tryEachType (Advice sr "The argument to |·| is not a real or complex number")
     [ AbsF <$> atType x RealType
     , AbsC <$> atType x ComplexType ]
  ty -> throwError (Surprise sr "the result of |·|" "a real number or integer" (Expected $ an ty))

tcVar :: String -> CheckedValue
tcVar n sr ty = case someSymbolVal n of
  SomeSymbol name -> case lookupEnv name ty (envProxy Proxy) of
    Found pf -> pure (Var name ty pf)
    WrongType (SomeType ty') ->
      throwError (Surprise sr n (an $ SomeType ty') (Expected $ an ty))
    Absent _ -> throwError (MissingName sr n)

type Fun a b = forall env value. KnownEnvironment env => Eval (value '(env, a)) -> ValueF value '(env, b)

data CommonFun = CommonFun (Fun 'RealT 'RealT) (Fun 'ComplexT 'ComplexT)

tcCommonFun :: (String, CommonFun) -> ParsedValue -> CheckedValue
tcCommonFun (name, CommonFun makeF makeC) x sr = \case
  RealType    -> makeF <$> atType x RealType
  ComplexType -> makeC <$> atType x ComplexType
  ty -> throwError (Surprise sr ("the result of " ++ name) "a real or complex number" (Expected $ an ty))

tcRealFun :: (String, RealFun) -> ParsedValue -> CheckedValue
tcRealFun (name, RealFun makeF) x sr = \case
  RealType -> makeF <$> atType x RealType
  ty -> throwError (Surprise sr ("the result of " ++ name) "a real number" (Expected $ an ty))

tcComplexFun :: (String, ComplexFun) -> ParsedValue -> CheckedValue
tcComplexFun (name, ComplexFun resultTy makeC) x sr ty = case resultTy of
  RealType -> case ty of
    RealType -> makeC <$> atType x ComplexType
    ComplexType -> R2C . makeC <$> atType x ComplexType
    _ -> throwError (Surprise sr ("the result of " ++ name) "a real number" (Expected $ an ty))
  ComplexType -> case ty of
    ComplexType -> makeC <$> atType x ComplexType
    _ -> throwError (Surprise sr ("the result of " ++ name) "a complex number" (Expected $ an ty))
  _ -> throwError (Surprise sr ("the result of " ++ name) (an $ SomeType resultTy) (Expected $ an ty))

tcInvert, tcDark, tcLight :: ParsedValue -> CheckedValue
tcDark c sr = \case
  ColorType -> Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType black)
               <$> atType c ColorType
  ty -> throwError (Surprise sr "the result of a darkening operation" "a color" (Expected $ an ty))

tcLight c sr = \case
  ColorType -> Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType white)
               <$> atType c ColorType
  ty -> throwError (Surprise sr "the result of a lightening operation" "a color" (Expected $ an ty))

tcInvert c sr = \case
  ColorType -> InvertRGB <$> atType c ColorType
  ty -> throwError (Surprise sr "the result of a color inversion" "a color" (Expected $ an ty))

tcBlend, tcCycle, tcRGB :: ParsedValue -> ParsedValue -> ParsedValue -> CheckedValue
tcBlend t c1 c2 sr = \case
  ColorType -> Blend <$> atType t RealType
                     <*> atType c1 ColorType
                     <*> atType c2 ColorType
  ty -> throwError (Surprise sr "the result of a blend operation" "a color" (Expected $ an ty))

tcCycle t c1 c2 sr = \case
  ColorType -> do
    let argName = Proxy @"#cycle-arg"

        go :: forall env. KnownEnvironment env
           => NameIsAbsent "#cycle-arg" env
           -> TC (Value '(env, 'ColorT))
        go pf = recallIsAbsent pf $ do
          argVal <- ModF <$> atType t RealType <*> pure 1
          let argVar = Var argName RealType bindingEvidence
              expr = ITE RealType (argVar `LTF` 0.5) (2 * argVar) (2 - 2 * argVar)
          blend <- Blend <$> pure expr
                         <*> atType c1 ColorType
                         <*> atType c2 ColorType
          pure (LocalLet argName RealType pf argVal ColorType blend)

    case lookupEnv' argName (envProxy Proxy) of
      Found' {} -> throwError (internal $ AlreadyDefined sr (symbolVal argName))
      Absent' pf -> go pf

  ty -> throwError (Surprise sr "the result of a cyclic blend operation" "a color" (Expected $ an ty))

-- Cycle through 8 colors over the [0,1] interval:
--   red, orange, yellow, green, cyan, blue, purple, magenta
tcRainbow :: ParsedValue -> CheckedValue
tcRainbow = tcWheel "rainbow" red orange yellow green cyan blue purple violet
  where
    cyan = rgbToColor (0, 128, 128)

-- | Sampled from 8 points in the Arenberg phase wheel
-- (https://eprovst.github.io/DomainColoring.jl/stable/arenberg/#The-Arenberg-Phase-Wheel)
-- and converted to sRGB using D50 at 2 degrees
tcArenberg :: ParsedValue -> CheckedValue
tcArenberg = tcWheel "arenberg"
  (rgbToColor (255, 80, 81))
  (rgbToColor (255, 174, 0))
  (rgbToColor (152, 190, 0))
  (rgbToColor (0, 183, 74))
  (rgbToColor (0, 237, 232))
  (rgbToColor (0, 172, 255))
  (rgbToColor (184, 161, 255))
  (rgbToColor (255, 134, 226))

tcWheel :: String
        -> Color -> Color -> Color -> Color
        -> Color -> Color -> Color -> Color
        -> ParsedValue -> CheckedValue
tcWheel name c1 c2 c3 c4 c5 c6 c7 c8 t sr = \case
  ColorType -> do
    let argName = Proxy @"#wheel-arg"

        go :: forall env. KnownEnvironment env
           => NameIsAbsent "#wheel-arg" env
           -> TC (Value '(env, 'ColorT))
        go pf = recallIsAbsent pf $ do
          argVal <- ModF <$> atType t RealType <*> pure 1
          let argVar = Var argName RealType bindingEvidence
              ifBelow = ITE tripleType . LTF argVar
              expr = ifBelow 0.5
                       (ifBelow 0.25
                          (ifBelow 0.125 (triple 0) (triple 1))
                          (ifBelow 0.375 (triple 2) (triple 3)))
                       (ifBelow 0.75
                          (ifBelow 0.625 (triple 4) (triple 5))
                          (ifBelow 0.875 (triple 6) (triple 7)))
              wheel = map (Const . Scalar ColorType)
                          [c1, c2, c3, c4, c5, c6, c7, c8, c1]

              triple i =
                let lo = fromIntegral i * 0.125
                in PairV tripleType (8 * (argVar - Const (Scalar RealType lo)))
                         (PairV pairType (wheel !! (i + 1)) (wheel !! i))

              tripleType = PairType RealType pairType
              pairType = PairType ColorType ColorType

              blend = Blend (ProjV1 tripleType expr)
                            (ProjV1 pairType (ProjV2 tripleType expr))
                            (ProjV2 pairType (ProjV2 tripleType expr))

          pure (LocalLet argName RealType pf argVal ColorType blend)

    case lookupEnv' argName (envProxy Proxy) of
      Found' {} -> throwError (internal $ AlreadyDefined sr (symbolVal argName))
      Absent' pf -> go pf

  ty -> throwError (Surprise sr ("the result of a " ++ name ++ " blend operation") "a color"
                                 (Expected $ an ty))

tcRGB r g b sr = \case
  ColorType -> RGB <$> atType r RealType
                   <*> atType g RealType
                   <*> atType b RealType
  ty -> throwError (Surprise sr "an RGB value" "a color" (Expected $ an ty))

tcList :: [ParsedValue] -> CheckedValue
tcList xs sr = \case
  ListType ity -> List ity <$> traverse (\x -> atType x ity) xs
  ty -> throwError (Surprise sr "a list literal" "some kind of list" (Expected $ an ty))

tcMod :: ParsedValue -> ParsedValue -> CheckedValue
tcMod x y sr = \case
  IntegerType -> ModI <$> atType x IntegerType <*> atType y IntegerType
  RealType    -> ModF <$> atType x RealType <*> atType y RealType
  ty -> throwError (Surprise sr
                       "the result of a modulo operation"
                       "an integer or real number" (Expected $ an (SomeType ty)))

tcEscapes :: Splices -> ParsedValue -> CheckedValue
tcEscapes splices pv sr = \case
  BooleanType -> do
    p <- tryEachType (Advice sr "The argument to `escapes` should be a real or complex number.")
     [ AbsC <$> atType pv ComplexType
     , AbsF <$> atType pv RealType ]
    case Map.lookup internalEscapeRadius splices of
      Nothing -> throwError $ internal (Advice sr "The script's escape radius was not defined.")
      Just r  -> LTF <$> atType r RealType <*> pure p
  ty -> throwError (Surprise sr "the result of `escapes`" "a truth value" (Expected $ an (SomeType ty)))

tcVanishes :: Splices -> ParsedValue -> CheckedValue
tcVanishes splices pv sr = \case
  BooleanType -> do
    p <- tryEachType (Advice sr "The argument to `vanishes` should be a real or complex number.")
     [ AbsC <$> atType pv ComplexType
     , AbsF <$> atType pv RealType ]
    case Map.lookup internalVanishingRadius splices of
      Nothing -> throwError $ internal (Advice sr "The script's vanishing radius was not defined.")
      Just r  -> LTF p <$> atType r RealType
  ty -> throwError (Surprise sr "the result of `vanishes`" "a truth value" (Expected $ an (SomeType ty)))

internalEscapeRadius, internalVanishingRadius, internalIterations :: String
internalStuck, internalIterationLimit :: String
internalEscapeRadius      = "[internal] escape radius"
type InternalEscapeRadius = "[internal] escape radius"
internalVanishingRadius      = "[internal] vanishing radius"
type InternalVanishingRadius = "[internal] vanishing radius"
internalIterations      = "[internal] iteration count"
type InternalIterations = "[internal] iteration count"
internalIterationLimit      = "[internal] iteration limit"
type InternalIterationLimit = "[internal] iteration limit"
type InternalStuck = "[internal] stuck"
internalStuck      = "[internal] stuck"

tcText :: [ParsedValue] -> CheckedValue
tcText args sr = \case
  TextType -> do
    let pv (ParsedValue sr' f) =
          tryEachType (Advice sr' "I don't know how to make this value into text.")
          [ f TextType
          , ToText IntegerType <$> f IntegerType
          , ToText RealType <$> f RealType
          , ToText ComplexType <$> f ComplexType
          ]
    ConcatText <$> mapM pv args
  ty -> throwError (Surprise sr "the result of text concatentation" "some text" (Expected $ an ty))

tcJoin :: [ParsedValue] -> CheckedValue
tcJoin args sr = \case
  ListType ty -> Join ty <$> mapM (`atType` ListType ty) args
  ty -> throwError (Surprise sr "the result of a join operation" "some list type" (Expected $ an ty))

tcAppend :: ParsedValue -> ParsedValue -> CheckedValue
tcAppend plst px sr = \case
  ListType ty -> do
    lst <- plst `atType` ListType ty
    x <- px `atType` ty
    pure (Join ty [lst, List ty [x]])
  ty -> throwError (Surprise sr "the result of an append operation" "some list type" (Expected $ an ty))

tcPrepend :: ParsedValue -> ParsedValue -> CheckedValue
tcPrepend plst px sr = \case
  ListType ty -> do
    lst <- plst `atType` ListType ty
    x <- px `atType` ty
    pure (Join ty [List ty [x], lst])
  ty -> throwError (Surprise sr "the result of an prepend operation" "some list type" (Expected $ an ty))

tcRemove :: ParsedValue -> String -> ParsedValue -> CheckedValue
tcRemove lst n p sr = go
  where
    go :: forall env ty. (KnownEnvironment env, KnownType ty)
       => TypeProxy ty -> TC (Value '(env, ty))
    go = \case
      ListType ty -> case someSymbolVal n of
        SomeSymbol name -> do
          DeclaredVar pf _ <- declareVar @_ @_ @env sr name ty (envProxy Proxy)
          Remove name ty pf <$> atType lst (ListType ty) <*> atType p BooleanType
      ty -> throwError (Surprise sr "the result of a remove operation" "some list type" (Expected $ an ty))

tcFind :: ParsedValue -> String -> ParsedValue -> ParsedValue -> CheckedValue
tcFind lst n p d sr = go
  where
    go :: forall env ty. (KnownEnvironment env, KnownType ty)
       => TypeProxy ty -> TC (Value '(env, ty))
    go ty = case someSymbolVal n of
      SomeSymbol name -> do
        DeclaredVar pf _ <- declareVar @_ @_ @env sr name ty (envProxy Proxy)
        Find name ty pf <$> atType lst (ListType ty)
                        <*> atType p BooleanType
                        <*> atType d ty

tcTransform :: ParsedValue -> String -> ParsedValue -> CheckedValue
tcTransform lst n fun sr = go
  where
    go :: forall env ty. (KnownEnvironment env, KnownType ty)
       => TypeProxy ty -> TC (Value '(env, ty))
    go = \case
      ListType ty -> case someSymbolVal n of
        -- FIXME: right now we only have functions from A -> A here, because
        -- there isn't a clean way to ask a ParsedValue "what type do you *want* to be?"
        SomeSymbol name -> do
          DeclaredVar pf _ <- declareVar @_ @_ @env sr name ty (envProxy Proxy)
          Transform name ty ty pf <$> atType lst (ListType ty) <*> atType fun ty
      ty -> throwError (Surprise sr "the result of a transform operation" "some list type" (Expected $ an ty))

tcRange :: ParsedValue -> ParsedValue -> CheckedValue
tcRange x y sr = \case
  ListType IntegerType -> Range <$> atType x IntegerType <*> atType y IntegerType
  ty -> throwError (Surprise sr "the result of a range operation" "a list of integers" (Expected $ an ty))

tcLength :: ParsedValue -> CheckedValue
tcLength _lst sr = \case
  IntegerType -> throwError (internal $ Advice sr "`length` has not been implemented yet")
  ty -> throwError (Surprise sr "the result of a length operation" "an integer" (Expected $ an ty))

tcIndex :: Bool -> ParsedValue -> ParsedValue -> CheckedValue
tcIndex cyc lst ix _sr ty =
  Index ty cyc <$> atType lst (ListType ty) <*> atType ix IntegerType

------------------------------------------------------
-- Various tables
------------------------------------------------------

colors :: Map String Color
colors = Map.fromList
  [ ("red", red), ("green", green), ("blue", blue)
  , ("black", black), ("white", white), ("grey", grey), ("gray", grey)
  , ("orange", orange), ("yellow", yellow), ("purple", purple), ("violet", violet)
  , ("pink", pink)]

realFunctions :: Map String RealFun
realFunctions = Map.fromList
  [ ("arccos",  RealFun ArccosF)
  , ("arcsin",  RealFun ArcsinF)
  , ("arctan",  RealFun ArctanF)
  , ("acos",    RealFun ArccosF)
  , ("asin",    RealFun ArcsinF)
  , ("atan",    RealFun ArctanF)
  , ("arccosh", RealFun ArccoshF)
  , ("arcsinh", RealFun ArcsinhF)
  , ("arctanh", RealFun ArctanhF)
  , ("acosh",   RealFun ArccoshF)
  , ("asinh",   RealFun ArcsinhF)
  , ("atanh",   RealFun ArctanhF)
  ]

commonFunctions :: Map String CommonFun
commonFunctions = Map.fromList
  [ ("exp",  CommonFun ExpF   ExpC)
  , ("log",  CommonFun LogF   LogC)
--  , ("abs", CommonFun AbsF AbsC)
  , ("sqrt", CommonFun SqrtF  SqrtC)
  , ("cos",  CommonFun CosF   CosC)
  , ("sin",  CommonFun SinF   SinC)
  , ("tan",  CommonFun TanF   TanC)
  , ("cosh", CommonFun CoshF  CoshC)
  , ("sinh", CommonFun SinhF  SinhC)
  , ("tanh", CommonFun TanhF  TanhC)
  ]

data RealFun = RealFun (Fun 'RealT 'RealT)
data ComplexFun where
  ComplexFun :: forall ty. TypeProxy ty -> Fun 'ComplexT ty -> ComplexFun

complexFunctions :: Map String ComplexFun
complexFunctions = Map.fromList
  [ ("re",  ComplexFun RealType ReC)
  , ("Re",  ComplexFun RealType ReC)
  , ("im",  ComplexFun RealType ImC)
  , ("Im",  ComplexFun RealType ImC)
  , ("arg", ComplexFun RealType ArgC)
  , ("Arg", ComplexFun RealType ArgC)
  , ("conj", ComplexFun ComplexType ConjC)
  , ("Conj", ComplexFun ComplexType ConjC)
  , ("bar", ComplexFun ComplexType ConjC)
  , ("Bar", ComplexFun ComplexType ConjC)
  ]

types :: Map String FSType
types = Map.fromList
  [ ("R", RealT), ("ℝ", RealT), ("Real", RealT)
  , ("Z", IntegerT), ("ℤ", IntegerT), ("Int", IntegerT), ("Integer", IntegerT)
  , ("C", ComplexT), ("ℂ", ComplexT), ("Complex", ComplexT)
  , ("Bool", BooleanT), ("Boolean", BooleanT)
  , ("Color", ColorT), ("Text", TextT)
  ]
