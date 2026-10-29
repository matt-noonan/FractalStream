-- | Elaboration: turn a fully-typed surface expression (see
-- "Language.Value.Typing") into a typed 'Value'. Every node already knows
-- its own type, so there are no choices left to make here: each node is
-- built at its own type and then converted to the type its parent wants.
-- Errors raised here (other than from 'derivative') indicate a typing-pass
-- bug, and are reported as internal errors.
module Language.Value.Elaborate
  ( elaborate
  ) where

import Prelude hiding (LT)
import FractalStream.Prelude

import qualified Data.Map as Map

import Language.Value
import Language.Value.Syntax
import Language.Value.Builtins
import Language.Value.Derivative
import Language.Typecheck
import Language.Parser.SourceRange
import Data.Color

-- | Elaborate a typed expression at the given type, inserting an implicit
-- conversion from the expression's own type if needed.
elaborate :: forall env ty
           . KnownEnvironment env
          => TypeProxy ty
          -> TExpr
          -> TC (Value '(env, ty))
elaborate ty e = withType (exprType e) $ \own ->
  elabOwn own e >>= convert (synRange e) own ty

convert :: forall env a b
         . KnownEnvironment env
        => SourceRange -> TypeProxy a -> TypeProxy b -> Value '(env, a) -> TC (Value '(env, b))
convert sr from to v = case sameHaskellType from to of
  Just Refl -> pure v
  Nothing -> case (from, to) of
    (IntegerType, RealType)    -> pure (I2R v)
    (IntegerType, ComplexType) -> pure (R2C (I2R v))
    (RealType,    ComplexType) -> pure (R2C v)
    (IntegerType, TextType)    -> pure (ToText from v)
    (RealType,    TextType)    -> pure (ToText from v)
    (ComplexType, TextType)    -> pure (ToText from v)
    (ComplexType, PairType RealType RealType) -> pure (C2R2 v)
    _ -> inconsistent sr

inconsistent :: SourceRange -> TC a
inconsistent sr = throwError (Internal (Advice sr "inconsistent types during elaboration"))

same :: SourceRange -> TypeProxy a -> TypeProxy b -> (a ~ b => TC r) -> TC r
same sr a b k = case sameHaskellType a b of
  Just Refl -> k
  Nothing   -> inconsistent sr

type ArithFun t = forall env value. KnownEnvironment env
               => Eval (value '(env, t)) -> Eval (value '(env, t)) -> ValueF value '(env, t)

-- | Build a node at its own type @t@.
elabOwn :: forall env t
         . (KnownEnvironment env, KnownType t)
        => TypeProxy t
        -> TExpr
        -> TC (Value '(env, t))
elabOwn t (Syn (sr, _) node) = case node of

  ELit _ (SomeHaskellType lt v) -> same sr lt t $ pure (Const (Scalar t v))

  EVar n -> variable sr n t

  ECast x _ -> el t x

  EITE c yes no -> ITE t <$> el BooleanType c <*> el t yes <*> el t no

  EPair x y -> case t of
    PairType t1 t2 -> PairV t <$> el t1 x <*> el t2 y
    _ -> inconsistent sr

  EList xs -> case t of
    ListType it -> List it <$> traverse (el it) xs
    _ -> inconsistent sr

  EBin op x y -> case op of
    OpAdd -> arith AddI AddF AddC x y
    OpSub -> arith SubI SubF SubC x y
    OpMul -> arith MulI MulF MulC x y
    OpPow -> arith PowI PowF PowC x y
    OpDiv -> case t of
      RealType    -> DivF <$> el t x <*> el t y
      ComplexType -> DivC <$> el t x <*> el t y
      _ -> inconsistent sr
    OpIDiv -> same sr t IntegerType $ DivI <$> el t x <*> el t y
    OpMod -> case t of
      IntegerType -> ModI <$> el t x <*> el t y
      RealType    -> ModF <$> el t x <*> el t y
      _ -> inconsistent sr
    OpOr  -> same sr t BooleanType $ Or  <$> el t x <*> el t y
    OpAnd -> same sr t BooleanType $ And <$> el t x <*> el t y
    OpExactEq -> same sr t BooleanType $ withType (joinType' (exprType x) (exprType y)) $ \et ->
      Eql et <$> el et x <*> el et y
    -- Only exact (integer / truth value) comparisons are left at this
    -- point; the typing pass rewrote the rest in terms of `vanishes`.
    OpApproxEq  -> same sr t BooleanType $ withType (exprType x) $ \et ->
      Eql et <$> el et x <*> el et y
    OpApproxNEq -> same sr t BooleanType $ withType (exprType x) $ \et ->
      NEq et <$> el et x <*> el et y
    OpLt -> same sr t BooleanType $ lessThan x y
    OpGt -> same sr t BooleanType $ lessThan y x
    OpGe -> same sr t BooleanType $ Not <$> lessThan x y
    OpLe -> same sr t BooleanType $ Not <$> lessThan y x

  EUn op x -> case op of
    OpNeg -> case t of
      IntegerType -> NegI <$> el t x
      RealType    -> NegF <$> el t x
      ComplexType -> NegC <$> el t x
      _ -> inconsistent sr
    OpNot -> same sr t BooleanType $ Not <$> el t x
    OpAbs -> case exprType x of
      IntegerT -> same sr t IntegerType $ AbsI <$> el IntegerType x
      RealT    -> same sr t RealType    $ AbsF <$> el RealType x
      ComplexT -> same sr t RealType    $ AbsC <$> el ComplexType x
      _ -> inconsistent sr
    OpEscapes -> same sr t BooleanType $
      LTF <$> variable sr internalEscapeRadius RealType <*> absValue x
    OpVanishes -> same sr t BooleanType $
      LTF <$> absValue x <*> variable sr internalVanishingRadius RealType
    OpDark -> same sr t ColorType $
      Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType black) <$> el t x
    OpLight -> same sr t ColorType $
      Blend (Const $ Scalar RealType 0.333) (Const $ Scalar ColorType white) <$> el t x
    OpInvert -> same sr t ColorType $ InvertRGB <$> el t x

  EApp f x
    | Just (CommonFun makeF makeC) <- Map.lookup f commonFunctions -> case t of
        RealType    -> makeF <$> el RealType x
        ComplexType -> makeC <$> el ComplexType x
        _ -> inconsistent sr
    | Just (RealFun makeF) <- Map.lookup f realFunctions ->
        same sr t RealType $ makeF <$> el RealType x
    | Just (RoundingFun makeF) <- Map.lookup f roundingFunctions ->
        same sr t IntegerType $ makeF <$> el RealType x
    | Just (ComplexFun resultTy makeC) <- Map.lookup f complexFunctions ->
        same sr t resultTy $ makeC <$> el ComplexType x
    | Just cs <- Map.lookup f wheels ->
        same sr t ColorType $ wheel sr cs =<< el RealType x
    | otherwise -> inconsistent sr

  EColor3 op x y z -> same sr t ColorType $ case op of
    OpBlend -> Blend <$> el RealType x <*> el t y <*> el t z
    OpRGB   -> RGB <$> el RealType x <*> el RealType y <*> el RealType z
    OpCycle -> el RealType x >>= \x' -> cycleColors sr x' y z

  EDiff x f -> do
    xv <- el t x
    fv <- el t f
    derivative (synRange x) xv (synRange f) fv

  EText xs -> same sr t TextType $ ConcatText <$> traverse (el t) xs

  EJoin xs -> case t of
    ListType it -> Join it <$> traverse (el t) xs
    _ -> inconsistent sr

  EAppend lst x -> case t of
    ListType it -> do
      lst' <- el t lst
      x' <- el it x
      pure (Join it [lst', List it [x']])
    _ -> inconsistent sr

  EPrepend lst x -> case t of
    ListType it -> do
      lst' <- el t lst
      x' <- el it x
      pure (Join it [List it [x'], lst'])
    _ -> inconsistent sr

  ERemove lst n p -> case t of
    ListType it -> withBinder @env sr n it $ \name pf ->
      Remove name it pf <$> el t lst <*> elaborate BooleanType p
    _ -> inconsistent sr

  EFind lst n p d -> withBinder @env sr n t $ \name pf ->
    Find name t pf <$> el (ListType t) lst <*> elaborate BooleanType p <*> el t d

  ETransform lst n f -> case (t, exprType lst) of
    (ListType t2, ListT it) -> withType it $ \t1 -> withBinder @env sr n t1 $ \name pf ->
      Transform name t1 t2 pf <$> el (ListType t1) lst <*> elaborate t2 f
    _ -> inconsistent sr

  ERange x y -> same sr t (ListType IntegerType) $
    Range <$> el IntegerType x <*> el IntegerType y

  ELength lst -> case exprType lst of
    ListT it -> same sr t IntegerType $ withType it $ \ity ->
      Length ity <$> el (ListType ity) lst
    _ -> inconsistent sr

  EIndex cyc lst ix -> Index t cyc <$> el (ListType t) lst <*> el IntegerType ix

  ELet n v body -> withType (exprType v) $ \vty -> withBinder @env sr n vty $ \name pf -> do
    v' <- el vty v
    LocalLet name vty pf v' t <$> elaborate t body

  ECall{} -> inconsistent sr

  EAdvice{} -> inconsistent sr

  where
    el :: forall u. TypeProxy u -> TExpr -> TC (Value '(env, u))
    el = elaborate

    -- integers are compared as integers, anything else as real numbers
    lessThan :: TExpr -> TExpr -> TC (Value '(env, 'BooleanT))
    lessThan a b
      | exprType a == IntegerT && exprType b == IntegerT =
          LTI <$> el IntegerType a <*> el IntegerType b
      | otherwise = LTF <$> el RealType a <*> el RealType b

    absValue :: TExpr -> TC (Value '(env, 'RealT))
    absValue a
      | exprType a == ComplexT = AbsC <$> el ComplexType a
      | otherwise = AbsF <$> el RealType a

    arith :: ArithFun 'IntegerT -> ArithFun 'RealT -> ArithFun 'ComplexT
          -> TExpr -> TExpr -> TC (Value '(env, t))
    arith makeI makeF makeC x y = case t of
      IntegerType -> makeI <$> el t x <*> el t y
      RealType    -> makeF <$> el t x <*> el t y
      ComplexType -> makeC <$> el t x <*> el t y
      _ -> inconsistent sr

-- | The join of two numeric types (the typing pass already checked it exists).
joinType' :: FSType -> FSType -> FSType
joinType' a b
  | ComplexT `elem` [a, b] = ComplexT
  | RealT `elem` [a, b] = RealT
  | otherwise = a

variable :: forall env t. KnownEnvironment env
         => SourceRange -> String -> TypeProxy t -> TC (Value '(env, t))
variable sr n t = case someSymbolVal n of
  SomeSymbol name -> case lookupEnv name t (envProxy (Proxy @env)) of
    Found pf -> pure (Var name t pf)
    WrongType (SomeType t') -> throwError (Surprise sr n (an $ SomeType t') (Expected $ an (SomeType t)))
    Absent _ -> throwError (MissingName sr n)

-- | Run an action that binds a new local variable.
withBinder :: forall env ty r
            . (KnownEnvironment env, KnownType ty)
           => SourceRange
           -> String
           -> TypeProxy ty
           -> (forall name. (KnownSymbol name, NotPresent name env)
                 => Proxy name -> NameIsAbsent name env -> TC r)
           -> TC r
withBinder sr n _ k = case someSymbolVal n of
  SomeSymbol name -> case lookupEnv' name (envProxy (Proxy @env)) of
    Found'{} -> throwError (AlreadyDefined sr n)
    Absent' pf -> recallIsAbsent pf (k name pf)

------------------------------------------------------
-- Color cycles and wheels
------------------------------------------------------

cycleColors :: forall env
             . KnownEnvironment env
            => SourceRange
            -> Value '(env, 'RealT)
            -> TExpr
            -> TExpr
            -> TC (Value '(env, 'ColorT))
cycleColors sr t c1 c2 = do
  let argName = Proxy @"#cycle-arg"
  case lookupEnv' argName (envProxy (Proxy @env)) of
    Found' {} -> throwError (internal $ AlreadyDefined sr (symbolVal argName))
    Absent' pf -> recallIsAbsent pf $ do
      let argVal = ModF t 1
          argVar = Var argName RealType bindingEvidence
          expr = ITE RealType (argVar `LTF` 0.5) (2 * argVar) (2 - 2 * argVar)
      blend <- Blend expr <$> elaborate ColorType c1 <*> elaborate ColorType c2
      pure (LocalLet argName RealType pf argVal ColorType blend)

wheel :: forall env
       . KnownEnvironment env
      => SourceRange
      -> [Color]
      -> Value '(env, 'RealT)
      -> TC (Value '(env, 'ColorT))
wheel sr cs t = do
  let argName = Proxy @"#wheel-arg"
  case lookupEnv' argName (envProxy (Proxy @env)) of
    Found' {} -> throwError (internal $ AlreadyDefined sr (symbolVal argName))
    Absent' pf -> recallIsAbsent pf $ do
      let argVal = ModF t 1
          argVar = Var argName RealType bindingEvidence
          ifBelow = ITE tripleType . LTF argVar
          expr = ifBelow 0.5
                   (ifBelow 0.25
                      (ifBelow 0.125 (triple 0) (triple 1))
                      (ifBelow 0.375 (triple 2) (triple 3)))
                   (ifBelow 0.75
                      (ifBelow 0.625 (triple 4) (triple 5))
                      (ifBelow 0.875 (triple 6) (triple 7)))
          colorWheel = map (Const . Scalar ColorType) (cs ++ take 1 cs)

          triple :: Int -> Value '( '("#wheel-arg", 'RealT) ': env, 'Pair 'RealT ('Pair 'ColorT 'ColorT))
          triple i =
            let lo = fromIntegral i * 0.125
            in PairV tripleType (8 * (argVar - Const (Scalar RealType lo)))
                     (PairV pairType (colorWheel !! (i + 1)) (colorWheel !! i))

          tripleType = PairType RealType pairType
          pairType = PairType ColorType ColorType

          blend = Blend (ProjV1 tripleType expr)
                        (ProjV1 pairType (ProjV2 tripleType expr))
                        (ProjV2 pairType (ProjV2 tripleType expr))

      pure (LocalLet argName RealType pf argVal ColorType blend)
