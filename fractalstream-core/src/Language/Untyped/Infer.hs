module Language.Untyped.Infer
  ( infer
  , TypeVar_
  ) where

import qualified Language.Untyped.Value as U
import Language.Untyped.Value (ValueWith)
import Language.Untyped.Shape
import Language.Value hiding (get, Ty)
import Data.Indexed.Functor
import Data.Recursive hiding (Fix)

import GHC.TypeLits --hiding (LTI, GTI)
import Control.Monad.State.Strict hiding (foldM)
import Control.Monad.Except hiding (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph
import Data.Foldable hiding (fold)
import qualified Data.Array as Array

import Fcf (Exp, Eval)
import Data.Type.Equality ((:~:)(..))
import Data.Kind

import Language.Untyped.Constraints

addEnvironmentConstraints :: forall m env
                           . (MonadState TCState m, MonadError TCError m)
                          => EnvironmentProxy env
                          -> Map String TypeVar
                          -> m ()
addEnvironmentConstraints env ctx = void $ fromEnvironmentM env $ \name ty -> do
  case Map.lookup (symbolVal name) ctx of
    Nothing -> throwError (UnboundVariable (symbolVal name))
    Just tv -> go ty tv
 where
   v `is` t = addConstraints [ v `IsUsedLike` t, v `IsDefinedLike` t ]
   go :: TypeProxy t -> TypeVar -> m ()
   go ty tv = case (ty, tv) of
     (BooleanType, BoolTV) -> pure ()
     (ColorType, ColorTV)  -> pure ()
     (_, ErrorTV e) -> throwError (ShapeErr $ show e)
     (PairType t1 t2, PairTV tv1 tv2) -> go t1 tv1 >> go t2 tv2
     (IntegerType, NumTV v) -> v `is` Z_T
     (RealType,    NumTV v) -> v `is` R_T
     (ComplexType, NumTV v) -> v `is` C_T
     _ -> throwError InconsistentType

solveConstraints :: forall m
                  . ( MonadState TCState m, MonadError TCError m )
                 => m (TV -> m NumTy)
solveConstraints = do
  cs <- Set.toList <$> gets tvcEdges
  let csEdges = flip map cs $ \case
        src `Generalizes` tgt  -> (TyVar src, TyVar tgt)
--        src `IsUsedLike` tc    -> (TyVar src, TyConstLo tc)
--        src `IsDefinedLike` tc -> (TyConstHi tc, TyVar src)
        src `IsDefinedLike` tc    -> (TyVar src, TyConstLo tc)
        src `IsUsedLike` tc -> (TyConstHi tc, TyVar src)
  lastTV <- gets tvcFresh

  let nodeMap = flip execState Map.empty $ do
        -- Allocate nodes for the intermediate type variables
        forM_ [0 .. lastTV - 1] (allocateTVC . TyVar)
        -- Allocate nodes for each derived variable appearing in the constraints
        forM_ csEdges $ \(src, tgt) -> allocateTVC src >> allocateTVC tgt

  let toTVID :: TVC -> Int
      toTVID v = case Map.lookup v nodeMap of
        Just n  -> n
        Nothing -> error ("couldn't find " ++ show v ++ " in nodeMap")

  -- Build a directed graph out of the constraints, where
  -- X -> Y whenever X `IsAbove` Y.
  let g = buildG (0, Map.size nodeMap - 1)
                 (uniques [ (toTVID src, toTVID tgt)
                          | (src, tgt) <- csEdges])

  -- forM csEdges (\(src, tgt) -> traceM (show src ++ " --> " ++ show tgt))

  -- Find the DAG of SCCs in the constraint graph. Each type variable in an
  -- SCC will be given the same type. The sinks in the DAG correspond to
  -- type constants acting as lower bounds. Starting from the sinks, work
  -- back through the DAG by assigning each SCC to the join of the types
  -- of its successors. If you get to a source s -> X, assert that the type
  -- of s is >= the type of X.
  let sccs  = Map.fromList (zip [0..] (map (Set.fromList . toList) (scc g)))
      sccsT = Map.fromList [(v,i) | (i,vs) <- Map.toList sccs
                                  , v <- Set.toList vs ]
      toSCC v = case Map.lookup v sccsT of
        Just s -> s
        Nothing -> error ("couldn't find " ++ show v ++ " in sccsT")

  -- Build the DAG of SCCs, reverse edges, and topologically sort. This
  -- ensures that if there is an edge Scc1 -> Scc2, then Scc2 comes
  -- before Scc1 in the resulting vertex list.
  let dag = buildG (0, Map.size sccs - 1)
              (uniques [ (toSCC (toTVID src), toSCC (toTVID tgt))
                       | (src, tgt) <- csEdges ])
      dagT = transposeG dag
      todo0 = topSort dagT
      todo1 = topSort dag
      lowerTypes0 = Map.fromList
        [ (toSCC (toTVID (TyConstLo ty)), ty)
        | (_, TyConstLo ty) <- csEdges ]
      upperTypes0 = Map.fromList
        [ (toSCC (toTVID (TyConstHi ty)), ty)
        | (TyConstHi ty, _) <- csEdges ]
      lowerTypes = flip execState lowerTypes0 $ forM_ todo0 $ \s -> do
        -- Take the join of all types of our successors, assign that type to s
        typesSoFar <- get
        case Map.lookup s typesSoFar of
          Just _  -> pure () -- lower bound type constants
          Nothing -> do
            let succTypes = [ case Map.lookup nbr typesSoFar of {
                                Just v -> v; Nothing -> Z_T }
                            | nbr <- dag Array.! s ]
                ty = foldl' joinNumTy Num_T succTypes
            modify' (Map.insert s ty)
      upperTypes = flip execState upperTypes0 $ forM_ todo1 $ \s -> do
        -- Take the meet of all types of our predecessors, assign that type to s
        typesSoFar <- get
        case Map.lookup s typesSoFar of
          Just _  -> pure () -- upper bound type constants
          Nothing -> do
            let succTypes = [ case Map.lookup nbr typesSoFar of
                                { Just v -> v; Nothing -> C_T }
                            | nbr <- dagT Array.! s ]
                ty = foldl' meetNumTy Top_T succTypes
            modify' (Map.insert s ty)

  -- Assign each type variable to the lower type of its SCC. But raise a
  -- type error if the upper type of the SCC is not above the lower type!
  forM_ (Map.toList lowerTypes) $ \(s, lo) -> do
    let hi = case Map.lookup s upperTypes of
          Just h  -> h
          Nothing -> C_T -- default case: upper bound is most general numeric type
    --traceM ("SCC " ++ show s ++ " : " ++ " lo=" ++ show lo ++ ", hi=" ++ show hi)
    when (joinNumTy lo hi /= hi) (throwError (TypeMismatch (numTyToTy lo) (numTyToTy hi)))

  pure $ \tv -> case Map.lookup (toSCC . toTVID . TyVar $ tv) lowerTypes of
                  Just tt -> case tt of
                    Num_T -> {-traceM ("typeOf " ++ show tv ++ " (scc " ++ show (toSCC . toTVID . TyVar $ tv) ++ ") = Z_T (default)") >> -} pure Z_T
                    _     -> {-traceM ("typeOf " ++ show tv ++ " (scc " ++ show (toSCC . toTVID . TyVar $ tv) ++ ") = " ++ show tt) >> -} pure tt
                  Nothing -> pure Z_T -- default case: no constraints? integer.

data TypeVar_ :: (Environment, FSType) -> Exp Type
type instance Eval (TypeVar_ et) = TypeVar

infer :: forall env rt m
       . ( MonadState TCState m, MonadError TCError m )
      => EnvironmentProxy env
      -> Map String TypeVar
      -> TypeProxy rt
      -> ValueWith TypeVar
      -> m (Value '(env, rt))
infer env ctx rt ast = withEnvironment env $ do
  --traceM ("value: " ++ show ast)
  -- Add constraints coming from the environment
  addEnvironmentConstraints env ctx
  -- Generate constraints from the value
  result <- foldM genConstraintsAlg ast
  case result of
    NumTV resultTV -> case rt of
      IntegerType -> addConstraint (resultTV `IsUsedLike` Z_T)
      RealType    -> addConstraint (resultTV `IsUsedLike` R_T)
      ComplexType -> addConstraint (resultTV `IsUsedLike` C_T)
      _            -> throwError InconsistentType
    _ -> pure ()

  -- Solve the constraints
  getNumType <- solveConstraints

  let getType = \case
        BoolTV -> pure BooleanTy
        ColorTV -> pure ColorTy
        ErrorTV e -> throwError (ShapeErr (show e))
        PairTV x y -> PairTy <$> getType x <*> getType y
        NumTV tv -> numTyToTy <$> getNumType tv

  -- Apply the solution to get a typed value
  toTypedValue getType (envTypeProxy env rt) ast
  -- traceM ("result: " ++ show v)
  --pure v

toTypedValue :: forall env0 t0 m
              . (MonadState TCState m, MonadError TCError m)
             => KnownEnvironment env0
             => (TypeVar -> m Ty)
             -> EnvTypeProxy '(env0, t0)
             -> ValueWith TypeVar
             -> m (Value '(env0, t0))
toTypedValue getType =
    indexedUnfoldM @ValueWithTypeVar_ @Value @ValueF (\(EnvType t) -> go t)
  where
    lookupName :: forall env t a
                . KnownEnvironment env
               => String
               -> TypeProxy t
               -> (forall name. KnownSymbol name
                     => Proxy name
                     -> NameIsPresent name t env
                     -> m a)
               -> m a
    lookupName s t k = case someSymbolVal s of
      SomeSymbol name -> case lookupEnv name t (envProxy (Proxy @env)) of
        Found pf     -> k name pf
        WrongType t' -> do
          --traceM ("in environment " ++ show (envProxy (Proxy @env)))
          --traceM ("while looking for " ++ show (symbolVal name))
          throwError (TypeMismatch' (SomeType t) t')
        Absent _     -> throwError (UnboundVariable s)

    --bad :: m a
    --bad = throwError InconsistentType

    typeOf :: ValueWith TypeVar -> m Ty
    typeOf = getType . annotation

    withJoinedType ::
      forall a. ValueWith TypeVar -> ValueWith TypeVar -> (forall t. TypeProxy t -> m a) -> m a
    withJoinedType v1 v2 k = do
      t1 <- typeOf v1
      t2 <- typeOf v2
      withType (t1 `joinTy` t2) k

    go :: forall env t
        . KnownEnvironment env
       => TypeProxy t
       -> ValueWith TypeVar
       -> m (ValueF ValueWithTypeVar_ '(env,t))
    go wanted tvv = do
      ity <- typeOf tvv
      withType ity $ \inferred ->
        case sameHaskellType wanted inferred of
          Just _  -> go' wanted tvv
          Nothing -> case (wanted, inferred) of
            (RealType,    IntegerType) -> pure (I2R tvv)
            (ComplexType, RealType)    -> pure (R2C tvv)
            (ComplexType, IntegerType) -> pure (R2C tvv)
              -- this ^ will go back through the (RealType, IntegerType)
              -- case during the next unfolding step.
            _ -> error ("wanted=" ++ showType wanted ++ ", but inferred=" ++ showType inferred)

    go' :: forall env t
         . KnownEnvironment env
        => TypeProxy t
        -> ValueWith TypeVar
        -> m (ValueF ValueWithTypeVar_ '(env,t))
    go' ty (Ann _ (U.Var s)) =
      lookupName s ty (\name pf -> pure (Var name ty pf))
    go' ty (Ann _ (U.ProjV1 p)) = do
      pairTy <- typeOf p
      withType pairTy $ \case
        PairType t1 t2 -> case sameHaskellType ty t1 of
          Just Refl -> pure (ProjV1 (PairType t1 t2) p)
          Nothing   -> error "bad"
        _               -> error "bad"
    go' ty (Ann _ (U.ProjV2 p)) = do
      pairTy <- typeOf p
      withType pairTy $ \case
        PairType t1 t2 -> case sameHaskellType t2 ty of
          Just Refl -> pure (ProjV2 (PairType t1 t2) p)
          Nothing   -> error "bad"
        _               -> error "bad"
    go' ty (Ann _ (U.ITE c y n)) = pure (ITE ty c y n)
    go' ty tvv@(Ann _ uv) = case ty of
      PairType _ _ -> case uv of
        U.PairV lhs rhs -> pure (PairV ty lhs rhs)
        _               -> error "bad"
      BooleanType -> case uv of
        U.ConstB b    -> pure (Const (Scalar BooleanType b))
        U.Or  lhs rhs -> pure (Or lhs rhs)
        U.And lhs rhs -> pure (And lhs rhs)
        U.Not x       -> pure (Not x)
        U.Eql lhs rhs -> withJoinedType lhs rhs (\ct -> pure (Eql ct lhs rhs))
        U.NEq lhs rhs -> withJoinedType lhs rhs (\ct -> pure (NEq ct lhs rhs))
        U.Cmp op lhs rhs -> withJoinedType lhs rhs $ \case
          IntegerType -> case op of
            U.LE -> pure (LEI lhs rhs)
            U.LT -> pure (LTI lhs rhs)
            U.GE -> pure (GEI lhs rhs)
            U.GT -> pure (GTI lhs rhs)
          RealType -> case op of
            U.LE -> pure (LEF lhs rhs)
            U.LT -> pure (LTF lhs rhs)
            U.GE -> pure (GEF lhs rhs)
            U.GT -> pure (GTF lhs rhs)
          _         -> error "bad"
        _ -> error "bad"
      ColorType -> case uv of
        U.ConstColor c  -> pure (Const (Scalar ColorType c))
        U.RGB r g b     -> pure (RGB r g b)
        U.Blend s c1 c2 -> pure (Blend s c1 c2)
        U.InvertRGB c   -> pure (InvertRGB c)
        _               -> error "bad"
      IntegerType -> case uv of
        U.ConstI n    -> pure (Const (Scalar IntegerType (fromIntegral n)))
        U.Arith op lhs rhs -> case op of
          U.Add       -> pure (AddI lhs rhs)
          U.Sub       -> pure (SubI lhs rhs)
          U.Mul       -> pure (MulI lhs rhs)
          U.Div       -> pure (DivI lhs rhs)
          U.Mod       -> pure (ModI lhs rhs)
          U.Pow       -> pure (PowI lhs rhs)
          _           -> error "bad"
        U.Ap1 U.Abs x -> pure (AbsI x)
        U.Ap1 U.Neg x -> pure (NegI x)
        _             -> error "bad"
      RealType -> case uv of
        U.ConstI n    -> pure (Const (Scalar RealType (fromIntegral n)))
        U.ConstF x    -> pure (Const (Scalar RealType x))
        U.Arith op lhs rhs -> case op of
          U.Add       -> pure (AddF lhs rhs)
          U.Sub       -> pure (SubF lhs rhs)
          U.Mul       -> pure (MulF lhs rhs)
          U.Div       -> pure (DivF lhs rhs)
          U.Mod       -> pure (ModF lhs rhs)
          U.Pow       -> pure (PowF lhs rhs)
          U.Arctan2   -> pure (Arctan2F lhs rhs)
        U.Ap1 f x -> case f of
          U.Neg       -> pure (NegF x)
          U.Abs -> typeOf x >>= \case
            IntegerTy -> pure (I2R (Ann (annotation x) (U.Ap1 U.Abs x)))
            RealTy    -> pure (AbsF x)
            ComplexTy -> pure (AbsC x)
            _         -> error "bad"
          U.Re        -> pure (ReC x)
          U.Im        -> pure (ImC x)
          U.Arg       -> pure (ArgC x)
          U.Sqrt      -> pure (SqrtF x)
          U.Exp       -> pure (ExpF x)
          U.Log       -> pure (LogF x)
          U.Sin       -> pure (SinF x)
          U.Cos       -> pure (CosF x)
          U.Tan       -> pure (TanF x)
          U.Sinh      -> pure (SinhF x)
          U.Cosh      -> pure (CoshF x)
          U.Tanh      -> pure (TanhF x)
          U.Arcsin    -> pure (ArcsinF x)
          U.Arccos    -> pure (ArccosF x)
          U.Arctan    -> pure (ArctanF x)
          U.Arcsinh   -> pure (ArcsinhF x)
          U.Arccosh   -> pure (ArccoshF x)
          U.Arctanh   -> pure (ArctanhF x)
          _           -> error "bad"
        _             -> error "bad"
      ComplexType -> case uv of
        U.ConstI n    -> pure (Const (Scalar ComplexType (fromIntegral n :+ 0)))
        U.ConstF x    -> pure (Const (Scalar ComplexType (x :+ 0)))
        U.ConstC x y  -> pure (Const (Scalar ComplexType (x :+ y)))
        U.Arith op lhs rhs -> case op of
          U.Add       -> pure (AddC lhs rhs)
          U.Sub       -> pure (SubC lhs rhs)
          U.Mul       -> pure (MulC lhs rhs)
          U.Div       -> pure (DivC lhs rhs)
          U.Pow       -> pure (PowC lhs rhs)
          _           -> error "bad"
        U.Ap1 f x -> case f of
          U.Neg       -> pure (NegC x)
          U.Conj      -> pure (ConjC x)
          U.Sqrt      -> pure (SqrtC x)
          U.Exp       -> pure (ExpC x)
          U.Log       -> pure (LogC x)
          U.Sin       -> pure (SinC x)
          U.Cos       -> pure (CosC x)
          U.Tan       -> pure (TanC x)
          U.Sinh      -> pure (SinhC x)
          U.Cosh      -> pure (CoshC x)
          U.Tanh      -> pure (TanhC x)
          _           -> error "bad"
        _             -> error "bad"
      _ -> error ("FIXME: " ++ show (unfold @U.Value (\(Ann _ v) -> v) tvv))

data ValueWithTypeVar_ :: (Environment, FSType) -> Exp Type
type instance Eval (ValueWithTypeVar_ et) = ValueWith TypeVar

uniques :: Ord a => [a] -> [a]
uniques = Set.toList . Set.fromList

-- | Fold over a type-variable-annotated value, producing type
-- constraints as a side effect.
genConstraintsAlg :: forall m
                   . (MonadState TCState m, MonadError TCError m)
                  => AnnF U.ValueF TypeVar TypeVar
                  -> m TypeVar
genConstraintsAlg (AnnF tv0 val) = do
  cs <- constraints val
  addConstraints cs
  pure tv0

 where

  assertBoolean = \case
    BoolTV -> pure ()
    _ -> throwError @_ @m InconsistentType

  isNum nt = \case
    NumTV x -> pure [ x `IsUsedLike` nt
                    , x `IsDefinedLike` nt ]
    _ -> throwError @_ @m InconsistentType

  isReal = isNum R_T
  isComplex = isNum C_T

  concat' = fmap concat . sequence

  assertColor = \case
    ColorTV -> pure ()
    _ -> throwError @_ @m InconsistentType

  binOp lhs0 rhs0 = case (tv0, lhs0, rhs0) of
    (NumTV tv, NumTV lhs, NumTV rhs) ->
      pure [ tv `Generalizes` lhs
           , tv `Generalizes` rhs ]
    _ -> throwError @_ @m InconsistentType

  -- like binOp, but don't fail for non-numeric values
  -- TODO: better typechecking here?
  binOp' lhs0 rhs0 = binOp'' tv0 lhs0 rhs0

  binOp'' xx yy zz = case (xx, yy, zz) of
    (BoolTV, BoolTV, BoolTV) -> pure []
    (ColorTV, ColorTV, ColorTV) -> pure []
    (PairTV a1 a2, PairTV b1 b2, PairTV c1 c2) -> do
      concat' [binOp'' a1 b1 c1, binOp'' a2 b2 c2]
    (NumTV tv, NumTV lhs, NumTV rhs) ->
      pure [ tv `Generalizes` lhs
           , tv `Generalizes` rhs ]
    _ -> throwError @_ @m InconsistentType

  fun x0 = case (tv0, x0) of
    (NumTV tv, NumTV x) -> pure [ tv `Generalizes` x
                                , x `IsDefinedLike` R_T
                                ]
    _ -> throwError @_ @m InconsistentType

  realFun x0 = case (tv0, x0) of
    (NumTV tv, NumTV x) ->
      concat' [ isReal tv0, pure [ tv `Generalizes` x ]]
    _ -> throwError @_ @m InconsistentType

  definedLike t = case tv0 of
    NumTV tv -> pure [ tv `IsDefinedLike` t ]
    _ -> throwError @_ @m InconsistentType

  generalizes lhs0 rhs0 = case (lhs0, rhs0) of
    (BoolTV, BoolTV) -> pure []
    (ColorTV, ColorTV) -> pure []
    (ErrorTV _, _) -> pure []
    (_, ErrorTV _) -> pure []
    (NumTV x, NumTV y) -> pure [x `Generalizes` y]
    (PairTV x1 x2, PairTV y1 y2) ->
      (++) <$> (x1 `generalizes` y1) <*> (x2 `generalizes` y2)
    _ -> throwError @_ @m InconsistentType

  constraints = \case
    U.ConstB _ -> assertBoolean tv0 >> pure []
    U.ConstI _ -> definedLike Z_T
    U.ConstF _ -> definedLike R_T
    U.ConstC{} -> definedLike C_T
    U.ConstColor _ -> assertColor tv0 >> pure []
    U.Var _    -> pure []

    U.PairV lhs rhs -> tv0 `generalizes` PairTV lhs rhs
    U.ProjV1 p -> case p of
      PairTV x _ -> tv0 `generalizes` x
      _ -> throwError InconsistentType
    U.ProjV2 p -> case p of
      PairTV _ y -> tv0 `generalizes` y
      _ -> throwError InconsistentType

    U.Arith op lhs rhs -> case op of
      U.Arctan2 -> (++) <$> realFun lhs <*> realFun rhs
      _         -> binOp lhs rhs

    U.Ap1 f x -> case f of
      U.Abs -> case (x, tv0) of
        (NumTV xv, NumTV tv) -> pure [ xv `IsUsedLike` C_T, tv `IsDefinedLike` R_T ]
                                -- pure [ xv `IsUsedLike` C_T
                                --     , tv `IsDefinedLike` R_T ]
        _ -> throwError InconsistentType
      U.Neg -> case (tv0, x) of
        (NumTV tv, NumTV x') -> pure [ tv `Generalizes` x']
        _ -> throwError InconsistentType
      U.Exp     -> fun x
      U.Log     -> fun x
      U.Sqrt    -> fun x
      U.Sin     -> fun x
      U.Cos     -> fun x
      U.Tan     -> fun x
      U.Sinh    -> fun x
      U.Cosh    -> fun x
      U.Tanh    -> fun x
      U.Arcsin  -> realFun x
      U.Arccos  -> realFun x
      U.Arctan  -> realFun x
      U.Arcsinh -> realFun x
      U.Arccosh -> realFun x
      U.Arctanh -> realFun x

      U.Arg     -> concat' [isComplex x, isReal tv0]
      U.Re      -> case (x, tv0) of
        (NumTV xv, NumTV tv) -> pure [ tv `IsDefinedLike` R_T
                                     , xv `IsUsedLike` C_T ] -- concat' [isComplex x, isReal tv0]
        _ -> throwError InconsistentType
      U.Im      -> case (x, tv0) of
        (NumTV xv, NumTV tv) -> pure [ tv `IsDefinedLike` R_T
                                     , xv `IsUsedLike` C_T ] -- concat' [isComplex x, isReal tv0]
        _ -> throwError InconsistentType
--      U.Im      -> concat' [isComplex x, isReal tv0]
      U.Conj    -> concat' [isComplex x, isComplex tv0]

    U.Or  lhs rhs -> do
      assertBoolean lhs
      assertBoolean rhs
      assertBoolean tv0
      pure []
    U.And lhs rhs -> do
      assertBoolean lhs
      assertBoolean rhs
      assertBoolean tv0
      pure []
    U.Not x -> do
      assertBoolean x
      assertBoolean tv0
      pure []

    U.ITE cond lhs rhs -> do
      assertBoolean cond
      binOp' lhs rhs

    U.RGB r g b -> do
      assertColor tv0
      concat' [isReal r, isReal g, isReal b]
    U.Blend s c1 c2 -> do
      assertColor c1
      assertColor c2
      assertColor tv0
      isReal s
    U.InvertRGB c -> do
      assertColor c
      assertColor tv0
      pure []

    U.Eql {} -> do
      assertBoolean tv0
      pure []
    U.NEq {} -> do
      assertBoolean tv0
      pure []

    U.Cmp {} -> do
      assertBoolean tv0
      pure []
