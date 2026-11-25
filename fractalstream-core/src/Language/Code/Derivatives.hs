-- | Derivatives through statements: @diff(v, x)@, with @v@ worked out
-- from @x@ by earlier statements (loops, branches and all), is the
-- derivative of @v@'s value with respect to @x@'s.
--
-- This is forward-mode differentiation, done to the code before it's
-- typed. Each variable that depends on @x@, and that a @diff@ needs the
-- derivative of, gets a companion holding that derivative (named
-- @d v/d x@), updated just before each assignment to the variable:
--
-- > d z/d c := 0          z := 0
-- > (in a loop:)
-- > d z/d c <- 2 z (d z/d c) + 1
-- > z <- z^2 + c
--
-- and @diff(z, c)@ becomes @d z/d c@. Only the variables some @diff@
-- needs get companions, so code without one is unchanged. A second
-- derivative, @diff(diff(v, x), x)@ (or @diff(v, x, 2)@), follows the
-- companions the inner one made, in a second round.
--
-- What it follows, and doesn't:
--
-- * Conditions aren't differentiated: an @if@ picks which assignments
--   happen, so the derivative is that of the branch taken.
-- * @x@ mustn't change between when a variable is worked out and the
--   @diff@ (the derivative would be with respect to an older @x@): that's
--   an error, saying to differentiate with respect to a copy.
-- * Derivatives are taken wherever they exist (see
--   "Language.Value.Differentiate"): an integer worked out from @x@ (by
--   @round@, say) is a constant, as are @round@, @floor@ and the like.
--   A value that depends on @x@ through an item of a list or table is an
--   error saying so.
-- * With respect to a complex @x@, the derivative is the Wirtinger
--   derivative ∂, and a variable whose value isn't holomorphic in @x@
--   (worked out with @|z|@, @re@, @conj@, ...) gets a second companion
--   holding its ∂̄ (named @dbar v/d x@), which the chain rule needs.
--
-- @partial(e, x)@ differentiates @e@ alone, every other variable a
-- constant, and is left for typing, as is the @diff@ that @f'(z)@ expands
-- to.
module Language.Code.Derivatives
  ( expandDerivatives
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Type
import Language.Typecheck
import Language.Parser.SourceRange
import Language.Value.Syntax
import Language.Value.Typing (TypeEnv(..))
import Language.Value.Differentiate
import Language.Code.Syntax
import Language.Code.Previous (mapStmtExprs)
import Language.Code.Typecheck.Functions (FunDef(..), Funs, expandCalls, freeVars)

-- | Expand every @diff@ whose variable is one of the code's (or its
-- environment's), innermost first.
expandDerivatives :: TypeEnv -> Stmt -> TC Stmt
expandDerivatives tenv = go (0 :: Int)
  where
    go n s = case innermostDiffs s of
      [] -> pure s
      (x, sr) : _
        | n > 64 -> throwError (Internal (Advice sr "expandDerivatives: too many rounds"))
        | otherwise -> forVariable tenv x sr s >>= go (n + 1)

-- | The @diff@s whose arguments have no @diff@ in them, by their
-- variables (and where one of them is). A @diff@ by a variable bound
-- inside an expression (a function's parameter) isn't one of them.
innermostDiffs :: Stmt -> [(String, SourceRange)]
innermostDiffs s = concatMap (inExpr Set.empty) (stmtExprs s)
  where
    inExpr bound (Syn sr node) = case node of
      EDiff (Syn _ (EVar x)) e
        | not (hasDiff e), not (Set.member x bound) -> [(x, sr)]
      ELet n v b -> inExpr bound v ++ inExpr (Set.insert n bound) b
      ELambda ps b -> inExpr (bound <> Set.fromList ps) b
      ERemove lst n p -> inExpr bound lst ++ inExpr (Set.insert n bound) p
      EFind lst n p dflt -> inExpr bound lst ++ inExpr (Set.insert n bound) p ++ inExpr bound dflt
      ETransform lst n f -> inExpr bound lst ++ inExpr (Set.insert n bound) f
      _ -> foldMap (inExpr bound) node
    hasDiff (Syn _ node) = case node of
      EDiff{} -> True
      _ -> any hasDiff node

-- | Every expression in a statement, and in the statements inside it, but
-- not one-line functions' bodies (where the variables are parameters).
stmtExprs :: Stmt -> [Expr]
stmtExprs st = execState (mapStmtExprs (\e -> modify (e :) $> e) (dropFunctions st)) []
  where
    dropFunctions (Stmt sr node) = Stmt sr $ case node of
      SBlock body -> SBlock (map dropFunctions body)
      SFun f ps _ -> SFun f ps (Syn sr (EVar f))
      SIf c yes no -> SIf c (dropFunctions yes) (dropFunctions no)
      SLoop k c lim body -> SLoop k c lim (dropFunctions body)
      SForEach i l body -> SForEach i l (dropFunctions body)
      SWithFirst i p l body fb -> SWithFirst i p l (dropFunctions body) (dropFunctions <$> fb)
      other -> other

------------------------------------------------------
-- One variable
------------------------------------------------------

forVariable :: TypeEnv -> String -> SourceRange -> Stmt -> TC Stmt
forVariable tenv x xsr s = do
  let funs = allFunctions s
      types = tyVars tenv <> declaredTypes s
  tyX <- case Map.lookup x types of
    Just t | t `elem` [RealT, ComplexT] -> pure t
    _ -> throwError (DiffInputMustBeVariable xsr)
  edges <- assignmentsIn funs s
  let -- what depends on x: anything assigned from it, or from what is
      deps = fixpoint (\d -> d <> Set.fromList [ v | (v, uses) <- edges, not (Set.null (uses `Set.intersection` d)) ])
                      (Set.singleton x)
      dependsOn = Set.delete x deps
      -- (a pair of numbers, as a tuple assignment's value is, too)
      numericType = \case
        RealT -> True
        ComplexT -> True
        Pair a b -> inPair a && inPair b
        _ -> False
      inPair = \case
        IntegerT -> True
        t -> numericType t
      numeric v = maybe False numericType (Map.lookup v types)
  -- the diffs by x (innermost), with their arguments' calls expanded
  diffs <- forM [ (dsr, e) | (y, dsr, e) <- concatMap innermostOf (stmtExprs s), y == x ] $ \(dsr, e) ->
    (dsr,) <$> expandCalls funs e
  let -- what needs a companion: what the diffs use that depends on x, and
      -- (since its derivative uses theirs) what that's worked out from
      fromDiffs = Set.unions [ valueDeps e | (_, e) <- diffs ] `Set.intersection` dependsOn
      usesOf v = Set.unions [ uses | (w, uses) <- edges, w == v ]
      needed = Set.filter numeric (fixpoint (\n -> n <> Set.unions (map usesOf (Set.toList n)) `Set.intersection` dependsOn) fromDiffs)
      companion v = "d " ++ v ++ "/d " ++ x
      companionBar v = "dbar " ++ v ++ "/d " ++ x
      -- (an integer worked out from x, as by `round`, only jumps, so it's
      -- a constant)
      isInteger v = Map.lookup v types == Just IntegerT
      denvWith bars = DiffEnv x (tyX == RealT) (Map.fromSet companion needed)
                              (Set.filter (not . isInteger) dependsOn) (Map.fromSet companionBar bars)
      companionOf = \case
        Pair a b -> Pair (companionOf a) (companionOf b)
        ComplexT -> ComplexT
        _ -> if tyX == ComplexT then ComplexT else RealT   -- (an integer in a pair, too)
      companionType v = maybe RealT companionOf (Map.lookup v types)
      -- (a variable with a companion already, from an earlier round, keeps it)
      fresh = Set.filter (\v -> Map.notMember (companion v) types) needed
  -- what needs a companion for its ∂̄ too: what's assigned something
  -- whose ∂̄ isn't zero, as far as that goes (in a loop, from itself)
  assigned <- assignedExprs funs needed s
  let growBars bars = do
        more <- forM assigned $ \(v, e) ->
          differentiatePair (denvWith bars) e <&> \(_, mbar) -> [ v | isJust mbar ]
        let bars' = bars <> Set.fromList (concat more)
        if bars' == bars then pure bars else growBars bars'
  bars <- if tyX == ComplexT then growBars Set.empty else pure Set.empty
  let denv = denvWith bars
      freshBars = Set.filter (\v -> Map.notMember (companionBar v) types) bars
  checkUnchanged funs x needed (needed `Set.intersection` Map.keysSet (tyVars tenv)) s
  s' <- withCompanions funs denv companion companionBar companionType fresh freshBars s
  -- companions of the environment's variables (those the code assigns)
  let envOnes = [ v | v <- Set.toList fresh, Map.member v (tyVars tenv) ]
      st = Stmt xsr
      zeroOf t = Syn xsr (ELit "0" (case t of
        ComplexT -> SomeHaskellType ComplexType 0
        _ -> SomeHaskellType RealType 0))
      top = [ st (SDecl (c v) (Just (companionType v)) (zeroOf (companionType v)))
            | v <- envOnes, c <- companion : [ companionBar | Set.member v freshBars ] ]
  s'' <- mapStmtExprs (replaceDiffs funs denv tyX) s'
  pure (if null top then s'' else st (SBlock (top ++ [s''])))
  where
    innermostOf (Syn sr node) = case node of
      EDiff (Syn _ (EVar y)) e | not (hasDiffIn e) -> [(y, sr, e)]
      _ -> foldMap innermostOf node
    hasDiffIn (Syn _ node) = case node of
      EDiff{} -> True
      _ -> any hasDiffIn node

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a = let a' = f a in if a' == a then a else fixpoint f a'

-- | The one-line functions the code defines (their names are unique).
allFunctions :: Stmt -> Funs
allFunctions s = funs
  where
    funs = Map.fromList [ (f, FunDef ps body funs) | Stmt _ (SFun f ps body) <- everyStmt s ]

-- | The declared types of the code's variables.
declaredTypes :: Stmt -> Map String FSType
declaredTypes s = Map.fromList [ (n, t) | Stmt _ (SDecl n (Just t) _) <- everyStmt s ]

everyStmt :: Stmt -> [Stmt]
everyStmt st@(Stmt _ node) = st : case node of
  SBlock body -> concatMap everyStmt body
  SIf _ yes no -> everyStmt yes ++ everyStmt no
  SLoop _ _ _ body -> everyStmt body
  SForEach _ _ body -> everyStmt body
  SWithFirst _ _ _ body fb -> everyStmt body ++ foldMap everyStmt fb
  _ -> []

-- | The assignments to the variables, with their calls expanded.
assignedExprs :: Funs -> Set String -> Stmt -> TC [(String, Expr)]
assignedExprs funs vars s = fmap concat . forM (everyStmt s) $ \(Stmt _ node) -> case node of
  SDecl v _ e | Set.member v vars -> one v e
  SSet v e | Set.member v vars -> one v e
  SIterate (PVar _ v) e _ _ _ | Set.member v vars -> one v e
  _ -> pure []
  where
    one v e = expandCalls funs e <&> \e' -> [(v, e')]

-- | What each assignment assigns, and what it's worked out from.
assignmentsIn :: Funs -> Stmt -> TC [(String, Set String)]
assignmentsIn funs s = fmap concat . forM (everyStmt s) $ \(Stmt _ node) -> case node of
  SDecl v _ e -> one v e
  SSet v e -> one v e
  SIterate (PVar _ v) e _ _ _ -> one v e
  SForEach item list _ -> pure [(item, Set.singleton list)]
  SWithFirst item _ list _ _ -> pure [(item, Set.singleton list)]
  SSetTable t ix v -> pure [(t, Map.keysSet (freeVars ix <> freeVars v))]
  _ -> pure []
  where
    one v e = expandCalls funs e <&> \e' -> [(v, valueDeps e')]

------------------------------------------------------
-- That x doesn't change under a diff
------------------------------------------------------

-- | What's known at a point of the code: which of the variables needing
-- companions were worked out from x as it is now, and whether x has been
-- changed from itself (as an iteration does).
data Fresh = Fresh (Set String) Bool
  deriving Eq

meet :: Fresh -> Fresh -> Fresh
meet (Fresh a i) (Fresh b j) = Fresh (Set.intersection a b) (i || j)

checkUnchanged :: Funs -> String -> Set String -> Set String -> Stmt -> TC ()
checkUnchanged funs x needed initiallyFresh s = void (walk True (Fresh initiallyFresh False) s)
  where
    walk report st (Stmt _ node) = case node of
      SBlock body -> foldM (walk report) st body
      SDecl v _ e -> assign report st v e
      SSet v e -> assign report st v e
      SIf c yes no -> do
        checkDiffs report st c
        meet <$> walk report st yes <*> walk report st no
      SLoop kind c lim body -> loop report st (kind `elem` [LoopDoWhile, LoopDoUntil]) (c : maybeToList lim) body
      SIterate (PVar psr v) e isWhile c lim ->
        walk report st (Stmt psr (SLoop (if isWhile then LoopWhile else LoopUntil) c lim
                                    (Stmt psr (SSet v e))))
      SForEach _ _ body -> loop report st False [] body
      SWithFirst _ p _ body fb -> do
        checkDiffs report st p
        b <- walk report st body
        maybe (pure (meet st b)) (fmap (meet b) . walk report st) fb
      _ -> st <$ mapM_ (checkDiffs report st) (stmtExprs (Stmt NoSourceRange node))

    -- a loop's body runs from what's known before it, or after a pass
    loop report st atLeastOnce conds body = do
      let settle k at = do
            after <- walk False at body
            let at' = meet st after
            if at' == at || k > (32 :: Int) then pure at else settle (k + 1) at'
      at <- settle 0 st
      mapM_ (checkDiffs report at) conds
      after <- walk report at body
      pure (if atLeastOnce then after else meet st after)

    assign report st@(Fresh fresh iterated) v e = do
      checkDiffs report st e
      e' <- expandCalls funs e
      let uses = valueDeps e'
      pure $ if v == x
        then Fresh Set.empty (iterated || Set.member x uses)
        else if Set.member v needed
          then Fresh ((if (uses `Set.intersection` needed) `Set.isSubsetOf` fresh
                         then Set.insert v else Set.delete v) fresh) iterated
          else st

    checkDiffs report (Fresh fresh iterated) e = when report $
      forM_ (diffsIn e) $ \(dsr, arg) -> do
        arg' <- expandCalls funs arg
        let uses = valueDeps arg'
        case Set.toList ((uses `Set.intersection` needed) `Set.difference` fresh) of
          v : _ -> advise dsr
            ("`" ++ v ++ "` was worked out from an earlier value of `" ++ x ++ "` (`" ++ x
             ++ "` changes after it), so `diff` can't follow it: differentiate with respect to a \
                \copy of `" ++ x ++ "` made before `" ++ v ++ "` is worked out (`" ++ x ++ "0 := "
             ++ x ++ "`).")
          [] -> when (iterated && Set.member x uses) $ advise dsr
            ("`" ++ x ++ "` changes before this, from its own value (as in a loop), so `diff` \
             \would only be with respect to its latest value: differentiate with respect to a \
             \copy made first (`" ++ x ++ "0 := " ++ x ++ "`), and work from that.")

    diffsIn (Syn sr node) = case node of
      EDiff (Syn _ (EVar y)) a | y == x -> [(sr, a)]
      _ -> foldMap diffsIn node

------------------------------------------------------
-- Rewriting
------------------------------------------------------

-- | The code, with each variable that needs a companion updating it
-- just before each assignment.
--
-- A variable with a companion for its ∂̄ too updates both from the
-- values before the assignment: ∂̄ is worked out into a temporary first,
-- since ∂'s companion might be part of it.
withCompanions :: Funs -> DiffEnv -> (String -> String) -> (String -> String) -> (String -> FSType)
               -> Set String -> Set String -> Stmt -> TC Stmt
withCompanions funs denv companion companionBar companionType needed bars = stmt
  where
    stmt (Stmt sr node) = case node of
      SBlock body -> Stmt sr . SBlock . concat <$> mapM spliced body
      _ -> spliced (Stmt sr node) <&> \case
        [one] -> one
        many' -> Stmt sr (SBlock many')

    derivativesOf e = differentiatePair denv =<< expandCalls funs e
    zeroAt sr = Syn sr (ELit "0" (SomeHaskellType IntegerType 0))

    -- (the companions' updates, from the values before the assignment)
    updates sr v e = derivativesOf e <&> \(de, mdbe) ->
      if Set.member v bars
        then let tmp = "[internal] " ++ companionBar v
             in [ Stmt sr (SBlock [ Stmt sr (SDecl tmp (Just (companionType v)) (fromMaybe (zeroAt sr) mdbe))
                                  , Stmt sr (SSet (companion v) de)
                                  , Stmt sr (SSet (companionBar v) (Syn sr (EVar tmp))) ]) ]
        else [ Stmt sr (SSet (companion v) de) ]

    spliced st@(Stmt sr node) = case node of
      SDecl v mty e | Set.member v needed -> do
        (de, mdbe) <- derivativesOf e
        when (isNothing mty) $
          advise sr ("I couldn't work out what type `" ++ v ++ "` is, to follow its derivative.")
        let decl c val = Stmt sr (SDecl c (Just (companionType v)) val)
        pure ([ decl (companion v) de ]
              ++ [ decl (companionBar v) (fromMaybe (zeroAt sr) mdbe) | Set.member v bars ]
              ++ [ st ])
      SSet v e | Set.member v needed -> (++ [st]) <$> updates sr v e
      SIterate (PVar psr v) e isWhile c lim | Set.member v needed -> do
        us <- updates psr v e
        pure [ Stmt sr (SLoop (if isWhile then LoopWhile else LoopUntil) c lim
                         (Stmt psr (SBlock (us ++ [ Stmt psr (SSet v e) ])))) ]
      SBlock{} -> (:[]) <$> stmt st
      SIf c yes no -> (\y n -> [Stmt sr (SIf c y n)]) <$> stmt yes <*> stmt no
      SLoop kind c lim body -> (\b -> [Stmt sr (SLoop kind c lim b)]) <$> stmt body
      SForEach item list body -> (\b -> [Stmt sr (SForEach item list b)]) <$> stmt body
      SWithFirst item p list body fb ->
        (\b f -> [Stmt sr (SWithFirst item p list b f)]) <$> stmt body <*> traverse stmt fb
      _ -> pure [st]

-- | Each innermost @diff@ by the variable, as the derivative.
replaceDiffs :: Funs -> DiffEnv -> FSType -> Expr -> TC Expr
replaceDiffs funs denv tyX = go
  where
    go e@(Syn sr node) = case node of
      EDiff (Syn _ (EVar y)) arg | y == deVar denv, not (hasDiff arg) -> do
        d <- differentiateWith denv =<< expandCalls funs arg
        pure (Syn sr (EWiden (if tyX == ComplexT then ComplexT else RealT) d))
      _ | hasDiff e -> Syn sr <$> traverse go node
        | otherwise -> pure e
    hasDiff (Syn _ node) = case node of
      EDiff{} -> True
      _ -> any hasDiff node
