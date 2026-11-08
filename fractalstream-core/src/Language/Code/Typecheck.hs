-- | Elaboration of parsed code ("Language.Code.Syntax") into typed 'Code'.
--
-- Scoping is resolved here: a variable declaration or function definition
-- scopes over the rest of its block. Calls to user-defined functions are
-- expanded inline before typing: each call becomes a chain of local
-- bindings of (freshly renamed) parameters to the arguments, around a copy
-- of the function body. So a function is effectively typechecked anew at
-- each call, at the argument types used there.
module Language.Code.Typecheck
  ( ParsedCode
  , runParsedCode
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Typecheck
import Language.Value.Typecheck (atType, internalIterationLimit, InternalIterations, InternalStuck)
import Language.Value.Syntax
import Language.Value.Typing (synth, typeEnvOf, tupleParts)
import qualified GHC.TypeNats as TN
import Language.Code
import Language.Code.Syntax
import Language.Code.Typecheck.Functions
import Language.Code.Infer
import Language.Code.Procedures
import Language.Code.Stops
import Language.Code.Tuples
import Language.Draw
import Language.Parser.SourceRange

import qualified Data.Map as Map

------------------------------------------------------
-- Parsed code
------------------------------------------------------

-- | Parsed code, which has not yet been checked for
-- type correctness, scope correctness, etc
type ParsedCode = Stmt

-- | Check parsed code in the given environment.
runParsedCode :: EnvironmentProxy env -> ParsedCode -> Either TCError (Code env)
runParsedCode env c =
  case withEnvironment env (expandRuns c >>= expandStops >>= expandTuples >>= elab Map.empty env . inferDeclarations (typeEnvOf env) Map.empty) of
    TC x -> x

------------------------------------------------------
-- Statements
------------------------------------------------------

elab :: forall env. KnownEnvironment env => Funs -> EnvironmentProxy env -> Stmt -> TC (Code env)
elab funs env stmt@(Stmt sr node) = case node of

  SBlock body -> Block <$> elabStmts funs env body

  -- declarations only make sense as part of a block
  SDecl{} -> Block <$> elabStmts funs env [stmt]
  SFun{}  -> Block <$> elabStmts funs env [stmt]

  SSet n v -> do
    SomeSymbol name <- pure (someSymbolVal n)
    FoundVar ty pf <- findVar sr name env
    Set pf name <$> value funs env v ty

  SSetTable n isPoint ix v -> do
    SomeSymbol name <- pure (someSymbolVal n)
    FoundVar ty pf <- findVar sr name env
    case ty of
      TableType d ety -> withKnownType ety $ do
        let dims = fromIntegral (TN.fromSNat d) :: Int
        cell <- if isPoint
          then if dims == 2
            then CellNear <$> value funs env ix ComplexType
            else throwError (Advice sr "Only a 2-dimensional table has points: index this one with integers.")
          else case tupleParts dims ix of
            Just parts -> CellAt <$> mapM (\p -> value funs env p IntegerType) parts
            Nothing -> throwError (Advice sr ("`" ++ n ++ "` is indexed by " ++ show dims
                                              ++ " integers: `" ++ n ++ " @ " ++ example dims ++ " <- ...`."))
        TableWrite pf name d ety cell <$> value funs env v ety
      _ -> throwError (Surprise sr n (an $ SomeType ty) (Expected "a table"))

  SAllocate n sz corners -> do
    SomeSymbol name <- pure (someSymbolVal n)
    FoundVar ty pf <- findVar sr name env
    case ty of
      TableType d ety -> do
        let dims = fromIntegral (TN.fromSNat d) :: Int
        sizes <- case tupleParts dims sz of
          Just parts -> mapM (\p -> value funs env p IntegerType) parts
          Nothing -> throwError (Advice sr ("`" ++ n ++ "` has " ++ show dims ++ " dimensions, so its size is "
                                            ++ show dims ++ " integers: `allocate " ++ n ++ " of size "
                                            ++ example dims ++ "`."))
        (p, q) <- case corners of
          Just (p, q) -> (,) <$> value funs env p ComplexType <*> value funs env q ComplexType
          Nothing -> case viewerCorners sr env of
            Just (p, q) -> (,) <$> value funs env p ComplexType <*> value funs env q ComplexType
            Nothing -> throwError (Advice sr ("Only a viewer's tool knows its window: give the table's corners, `allocate "
                                              ++ n ++ " of size ... from p to q`."))
        pure (TableAllocate pf name d ety sizes p q)
      _ -> throwError (Surprise sr n (an $ SomeType ty) (Expected "a table"))

  SIf cond yes no ->
    IfThenElse <$> value funs env cond BooleanType
               <*> elab funs env yes
               <*> elab funs env no

  SLoop kind cond mlimit body -> case kind of
    LoopWhile   -> genericLoop funs False whileLoop mlimit body cond sr env
    LoopUntil   -> genericLoop funs True  whileLoop mlimit body cond sr env
    LoopDoWhile -> genericLoop funs False DoWhile mlimit body cond sr env
    LoopDoUntil -> genericLoop funs True  DoWhile mlimit body cond sr env
    where whileLoop c b = IfThenElse c (DoWhile c b) NoOp

  SIterate (PVar _ var) expr isWhile cond upto ->
    elab funs env (Stmt sr (SLoop (if isWhile then LoopWhile else LoopUntil)
                                  cond upto (Stmt sr (SSet var expr))))

  -- these are expanded away before elaboration ("Language.Code.Tuples")
  SIterate PTuple{} _ _ _ _ -> throwError (Internal (Advice sr "unexpanded tuple iterate"))
  SDeclTuple{} -> throwError (Internal (Advice sr "unexpanded tuple declaration"))
  SSetTuple{} -> throwError (Internal (Advice sr "unexpanded tuple assignment"))

  SPass -> pure NoOp

  -- these are expanded away before elaboration
  SProc{} -> throwError (Internal (Advice sr "unexpanded code definition"))
  SRun name _ -> throwError (Internal (Advice sr ("unexpanded run of " ++ name)))

  SDraw d -> DrawCommand <$> case d of
    DrawPointAt p -> DrawPoint env <$> point funs env p
    DrawCircleAt isFilled center radius ->
      DrawCircle env isFilled <$> value funs env radius RealType <*> point funs env center
    DrawRectFrom isFilled ul lr -> DrawRect env isFilled <$> point funs env ul <*> point funs env lr
    DrawLineFrom ul lr -> DrawLine env <$> point funs env ul <*> point funs env lr
    UseStroke c -> SetStroke env <$> value funs env c ColorType
    UseFill c -> SetFill env <$> value funs env c ColorType
    EraseAll -> pure (Clear env)
    WriteAt txt pt -> Write env <$> value funs env txt TextType <*> point funs env pt

  SForEach itemName listName body -> do
    SomeSymbol item <- pure (someSymbolVal itemName)
    SomeSymbol list <- pure (someSymbolVal listName)
    ListExists itemTy pfListPresent <- getListType sr list env
    DeclaredVar pfItemAbsent env' <- declareVar sr item itemTy env
    ForEach pfListPresent list (ListType itemTy) item pfItemAbsent env env'
      <$> elab funs env' body

  SWithFirst itemName predicate listName body fallback -> do
    SomeSymbol item <- pure (someSymbolVal itemName)
    SomeSymbol list <- pure (someSymbolVal listName)
    ListExists itemTy pfListPresent <- getListType sr list env
    DeclaredVar pfItemAbsent env' <- declareVar sr item itemTy env
    Lookup pfListPresent list (ListType itemTy) item pfItemAbsent env' env
      <$> value funs env' predicate BooleanType
      <*> elab funs env' body
      <*> traverse (elab funs env) fallback

-- | Elaborate the statements of a block. A declaration scopes over the
-- rest of the block, so it becomes the last statement of this block, with
-- the remaining statements nested inside it.
elabStmts :: forall env. KnownEnvironment env => Funs -> EnvironmentProxy env -> [Stmt] -> TC [Code env]
elabStmts funs env = \case
  [] -> pure []

  Stmt sr (SDecl n mty v) : rest -> do
    t <- case mty of
      Just t  -> pure t
      Nothing -> exprType <$> (synth (typeEnvOf env) =<< expandCalls funs v)
    withType t $ \ty -> do
      SomeSymbol name <- pure (someSymbolVal n)
      DeclaredVar _ env' <- declareVar sr name ty env
      c <- Let bindingEvidence name <$> value funs env v ty
                                    <*> (Block <$> elabStmts funs env' rest)
      pure [c]

  Stmt sr (SFun f params body) : rest -> do
    funs' <- defineFunction sr funs env f params body
    elabStmts funs' env rest

  s : rest -> (:) <$> elab funs env s <*> elabStmts funs env rest

------------------------------------------------------
-- Values
------------------------------------------------------

value :: forall env ty. (KnownEnvironment env, KnownType ty)
      => Funs -> EnvironmentProxy env -> Expr -> TypeProxy ty -> TC (Value '(env, ty))
value funs _ v ty = expandCalls funs v >>= \v' -> atType v' ty

-- | Something point-like: a pair of real numbers, or a (real or complex)
-- number, converted to a pair of reals.
point :: forall env. KnownEnvironment env
      => Funs -> EnvironmentProxy env -> Expr -> TC (Value '(env, 'Pair 'RealT 'RealT))
point funs env p = do
  p' <- expandCalls funs p
  isNumber <- (synth (typeEnvOf env) p' <&> \t -> exprType t `elem` [IntegerT, RealT, ComplexT])
                `catchError` \_ -> pure False
  if isNumber
    then C2R2 <$> atType p' ComplexType
    else atType p' (PairType RealType RealType) `catchTypeError` \_ ->
      throwError (Surprise (synRange p) "this"
                   "not a complex number or pair of real numbers"
                   (Expected "something point-like"))

------------------------------------------------------
-- Loops
------------------------------------------------------

genericLoop :: forall env
             . KnownEnvironment env
            => Funs
            -> Bool
            -> (forall env'. KnownEnvironment env' =>
                  Value '(env', 'BooleanT) -> Code env' -> Code env')
            -> Maybe Expr -> Stmt -> Expr -> SourceRange -> EnvironmentProxy env
            -> TC (Code env)
genericLoop funs negateCondition mkLoop mlimit body cond sr env =
  withFresh sr env IntegerType 0 $ \env' (counterName :: Proxy counterName) pf0 -> recallIsAbsent pf0 $ do

  limitValue <- value funs env' (fromMaybe (Syn sr (EVar internalIterationLimit)) mlimit) IntegerType

  withFresh sr env' IntegerType limitValue $ \env'' (limitName :: Proxy limitName) pf' -> recallIsAbsent pf' $ do

    pf <- findVarAtType sr counterName IntegerType env''
    let counter, limit ::
          Value '( '(limitName, 'IntegerT) ': '(counterName, 'IntegerT) ': env, 'IntegerT)
        counter = Var counterName IntegerType pf
        limit =   Var limitName IntegerType   (bindName limitName IntegerType pf')

    c <- value funs env'' cond BooleanType
    b <- elab funs env'' body
    let b' = Block [ b, Set pf counterName (counter + 1)]
        c' = And (if negateCondition then Not c else c) (counter `LTI` limit)
        iterations = Proxy @InternalIterations
        stuck      = Proxy @InternalStuck
    ipf <- findVarAtType sr iterations IntegerType env''
    spf <- findVarAtType sr stuck      BooleanType env''
    pure $ Block
      [ mkLoop c' b'
      , Set ipf iterations counter
      , Set spf stuck (Eql IntegerType counter limit) ]

-----------------
-- Utilities
-----------------

data ListExists name env where
  ListExists :: forall name itemTy env
              . (KnownSymbol name, KnownType itemTy)
             => TypeProxy itemTy
             -> NameIsPresent name ('ListT itemTy) env
             -> ListExists name env

getListType :: (MonadError TCError m, KnownSymbol name)
            => SourceRange
            -> Proxy name
            -> EnvironmentProxy env
            -> m (ListExists name env)
getListType sr name env = case lookupEnv' name env of
  Absent'{} -> throwError (MissingName sr (symbolVal name))
  Found' ty pf -> case ty of
    ListType itemTy -> pure (ListExists itemTy pf)
    _ -> throwError (Surprise sr (symbolVal name) (an $ SomeType ty) (Expected "a list"))

withFresh :: forall ty env m
           . MonadError TCError m
          => SourceRange
          -> EnvironmentProxy env
          -> TypeProxy ty
          -> Value '(env, ty)
          -> (forall fresh. KnownSymbol fresh => EnvironmentProxy ( '(fresh, ty) ': env)
                                              -> Proxy fresh
                                              -> NameIsAbsent fresh env
                                              -> m (Code ( '(fresh, ty) ': env)))
          -> m (Code env)
withFresh sr env ty value_ action = withEnvironment env $ do
  let tmpName = "[internal] fresh #" ++
        show (length $ fromEnvironment env (\_ _ -> ()))
  case someSymbolVal tmpName of
    SomeSymbol tmp -> case lookupEnv tmp ty env of
      Absent pf -> recallIsAbsent pf $ let_ value_ <$>
        action (bindNameEnv tmp ty pf env) tmp pf
      _ -> throwError (Internal $ AlreadyDefined sr tmpName)

-- | An example of a table index with some number of dimensions.
example :: Int -> String
example dims = if dims == 1 then "k" else "(" ++ intercalate ", " (take dims (["j", "k"] ++ map (("k" ++) . show) [3 :: Int ..])) ++ ")"

-- | The top left and bottom right corners of a viewer's window, from its
-- geometry (see 'Actor.Ensemble.viewerGeometry') and pixel size, as a
-- tool's code sees them; or 'Nothing' outside a viewer's tool.
viewerCorners :: SourceRange -> EnvironmentProxy env -> Maybe (Expr, Expr)
viewerCorners sr env = do
  let names = envToMap env
      has n = n `Map.member` names
      v n = Syn sr (EVar n)
      num x = Syn sr (ELit (show x) (SomeHaskellType RealType x))
      bin op a b = Syn sr (EBin op a b)
      im = Syn sr (ELit "i" (SomeHaskellType ComplexType (0 :+ 1)))
      halfW = bin OpMul (bin OpDiv (v "viewer_width") (num 2)) (v "[internal] px")
      halfH = bin OpMul (bin OpDiv (v "viewer_height") (num 2)) (v (if has "[internal] py" then "[internal] py" else "[internal] px"))
      corner x y = bin OpAdd x (bin OpMul y im)
  guard (all has ["viewer_width", "viewer_height", "[internal] px"])
  (cx, cy) <- if has "viewer_center"
    then pure (Syn sr (EApp "re" (v "viewer_center")), Syn sr (EApp "im" (v "viewer_center")))
    else if has "viewer_center_x" && has "viewer_center_y"
      then pure (v "viewer_center_x", v "viewer_center_y")
      else Nothing
  pure ( corner (bin OpSub cx halfW) (bin OpAdd cy halfH)
       , corner (bin OpAdd cx halfW) (bin OpSub cy halfH) )
