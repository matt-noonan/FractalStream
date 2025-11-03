module Language.Code.Typecheck where

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Typecheck
import Language.Value.Parser
import Language.Code
import Language.Draw
import Language.Parser.SourceRange
import Language.Value.Typecheck (internalIterationLimit, InternalIterations, InternalStuck)

import qualified Data.Map as Map

------------------------------------------------------
-- Parsed code
------------------------------------------------------

-- | Parse code, which has not yet been checked for
-- type correctness, scope correctness, etc
newtype ParsedCode = ParsedCode
  (forall env. EnvironmentProxy env -> TC (Code env))

-- | Code which has been checked for type & scope
-- correctness.
type CheckedCode = SourceRange -> (forall env. KnownEnvironment env => EnvironmentProxy env -> TC (Code env))

atEnv :: EnvironmentProxy env -> ParsedCode -> TC (Code env)
atEnv env (ParsedCode f) = f env

------------------------------------------------------
-- Code environment checking
------------------------------------------------------

tcBlock :: [ParsedCode] -> CheckedCode
tcBlock body _sr env = Block <$> traverse (atEnv env) body

tcLet :: String -> FSType -> ParsedValue -> ParsedCode -> CheckedCode
tcLet n t v c sr env = withType t $ \ty -> do
  SomeSymbol name <- pure (someSymbolVal n)
  DeclaredVar _ env' <- declareVar sr name ty env
  Let bindingEvidence name <$> atType v ty <*> atEnv env' c

tcSet :: String -> ParsedValue -> CheckedCode
tcSet n v sr env = do
  SomeSymbol name <- pure (someSymbolVal n)
  FoundVar ty pf <- findVar sr name env
  Set pf name <$> atType v ty

tcGenericLoop :: Bool
              -> (forall env. KnownEnvironment env =>
                    Value '(env, 'BooleanT) -> Code env -> Code env)
              -> Splices -> Maybe ParsedValue -> ParsedCode -> ParsedValue -> CheckedCode
tcGenericLoop negateCondition mkLoop splices mlimit body cond sr (env :: EnvironmentProxy env) =
  withFresh sr env  IntegerType 0 $ \env' (counterName :: Proxy counterName) pf0 -> recallIsAbsent pf0 $ do

  limitValue <- case mlimit of
    Just (ParsedValue _ limitFun) -> limitFun IntegerType
    Nothing -> case Map.lookup internalIterationLimit splices of
      Just (ParsedValue _ limitFun)  -> limitFun IntegerType
      Nothing -> throwError $ internal $
               Advice sr "The script's iteration limit was not defined."

  withFresh sr env' IntegerType limitValue $ \env'' (limitName :: Proxy limitName) pf' -> recallIsAbsent pf' $ do

    pf <- findVarAtType sr counterName IntegerType env''
    let counter, limit ::
          Value '( '(limitName, 'IntegerT) ': '(counterName, 'IntegerT) ': env, 'IntegerT)
        counter = Var counterName IntegerType pf
        limit =   Var limitName IntegerType   (bindName limitName IntegerType pf')

    c <- atType cond BooleanType
    b <- atEnv env'' body
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

tcWhile, tcUntil :: Splices -> ParsedValue -> Maybe ParsedValue -> ParsedCode -> CheckedCode
tcDoWhile, tcDoUntil :: Splices -> Maybe ParsedValue -> ParsedCode -> ParsedValue -> CheckedCode

tcWhile s c l b =
  tcGenericLoop False (\cond body -> IfThenElse cond (DoWhile cond body) NoOp) s l b c
tcUntil s c l b =
  tcGenericLoop True  (\cond body -> IfThenElse cond (DoWhile cond body) NoOp) s l b c
tcDoWhile = tcGenericLoop False DoWhile
tcDoUntil = tcGenericLoop True  DoWhile

tcIfThenElse :: ParsedValue -> ParsedCode -> ParsedCode -> CheckedCode
tcIfThenElse cond yes no _sr env =
  IfThenElse <$> atType cond BooleanType
             <*> atEnv env yes
             <*> atEnv env no

tcIterate :: Splices -> String -> ParsedValue -> Bool -> ParsedValue -> Maybe ParsedValue -> CheckedCode
tcIterate splices var expr isWhile cond upto sr env = do
  let body = ParsedCode (\e -> withEnvironment e $ tcSet var expr sr e)
      tc = if isWhile then tcWhile else tcUntil
  tc splices cond upto body sr env

tcPoint :: KnownEnvironment env => ParsedValue -> TC (Value '(env, 'Pair 'RealT 'RealT))
tcPoint p@(ParsedValue sr _) =
  tryEachType (Surprise sr "this"
               "not a complex number or pair of real numbers"
               (Expected "something point-like"))
    [ atType p (PairType RealType RealType)
    , C2R2 <$> atType p ComplexType ]

tcDrawPoint :: ParsedValue -> CheckedCode
tcDrawPoint v _sr env = DrawCommand . DrawPoint env <$> tcPoint v

tcDrawCircle :: Bool -> ParsedValue -> ParsedValue -> CheckedCode
tcDrawCircle isFilled center radius _sr env =
  DrawCommand <$> (DrawCircle env isFilled <$> atType radius RealType <*> tcPoint center)

tcDrawRect :: Bool -> ParsedValue -> ParsedValue -> CheckedCode
tcDrawRect isFilled ul lr _sr env =
  DrawCommand <$> (DrawRect env isFilled <$> tcPoint ul <*> tcPoint lr)

tcDrawLine :: ParsedValue -> ParsedValue -> CheckedCode
tcDrawLine ul lr _sr env =
  DrawCommand <$> (DrawLine env <$> tcPoint ul <*> tcPoint lr)

tcSetStroke :: ParsedValue -> CheckedCode
tcSetStroke c _sr env = DrawCommand . SetStroke env <$> atType c ColorType

tcSetFill :: ParsedValue -> CheckedCode
tcSetFill c _sr env = DrawCommand . SetFill env <$> atType c ColorType

tcClear :: CheckedCode
tcClear _sr env = pure (DrawCommand $ Clear env)

tcWrite :: ParsedValue -> ParsedValue -> CheckedCode
tcWrite txt pt _sr env = DrawCommand <$> (Write env <$> atType txt TextType <*> tcPoint pt)

tcListFor :: String -> String -> ParsedCode -> CheckedCode
tcListFor itemName listName body sr env = do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)

  ListExists itemTy pfListPresent <- getListType sr list env
  DeclaredVar pfItemAbsent env' <- declareVar sr item itemTy env
  ForEach pfListPresent list (ListType itemTy) item pfItemAbsent env env'
    <$> atEnv env' body

tcListWith :: String
           -> ParsedValue
           -> String
           -> ParsedCode
           -> Maybe ParsedCode
           -> CheckedCode
tcListWith itemName predicate listName body fallback sr env = do
  SomeSymbol item <- pure (someSymbolVal itemName)
  SomeSymbol list <- pure (someSymbolVal listName)
  ListExists itemTy pfListPresent <- getListType sr list env
  DeclaredVar pfItemAbsent env' <- declareVar sr item itemTy env
  Lookup pfListPresent list (ListType itemTy) item pfItemAbsent env' env
    <$> atType predicate BooleanType
    <*> atEnv env' body
    <*> traverse (atEnv env) fallback

-----------------
-- Utilities
-----------------

data ListExists name env where
  ListExists :: forall name itemTy env
              . (KnownSymbol name, KnownType itemTy)
             => TypeProxy itemTy
             -> NameIsPresent name ('ListT itemTy) env
             -> ListExists name env

getListType :: KnownSymbol name
            => SourceRange
            -> Proxy name
            -> EnvironmentProxy env
            -> TC (ListExists name env)
getListType sr name env = case lookupEnv' name env of
  Absent'{} -> throwError (MissingName sr (symbolVal name))
  Found' ty pf -> case ty of
    ListType itemTy -> pure (ListExists itemTy pf)
    _ -> throwError (Surprise sr (symbolVal name) (an $ SomeType ty) (Expected "a list"))

withFresh :: forall ty env
           . SourceRange
          -> EnvironmentProxy env
          -> TypeProxy ty
          -> Value '(env, ty)
          -> (forall fresh. KnownSymbol fresh => EnvironmentProxy ( '(fresh, ty) ': env)
                                              -> Proxy fresh
                                              -> NameIsAbsent fresh env
                                              -> TC (Code ( '(fresh, ty) ': env)))
          -> TC (Code env)
withFresh sr env ty value action = withEnvironment env $ do
  let tmpName = "[internal] fresh #" ++
        show (length $ fromEnvironment env (\_ _ -> ()))
  case someSymbolVal tmpName of
    SomeSymbol tmp -> case lookupEnv tmp ty env of
      Absent pf -> recallIsAbsent pf $ let_ value <$>
        action (bindNameEnv tmp ty pf env) tmp pf
      _ -> throwError (Internal $ AlreadyDefined sr tmpName)
