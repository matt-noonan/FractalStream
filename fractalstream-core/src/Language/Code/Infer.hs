-- | Type inference for variables declared without a type (@x := e@).
--
-- Each such declaration gets a type variable, and every assignment to the
-- variable (including its initializer) contributes a lower bound: the
-- type synthesized for the assigned expression. The solution is the least
-- one: each variable starts out unknown, and is repeatedly raised to the
-- join of its current type and the types of everything assigned to it,
-- until nothing changes. So after
--
-- > z := 0
-- > iterate z -> z^2 + C until z escapes
--
-- @z@ is complex if @C@ is. Synthesis is done by the ordinary typing pass,
-- using the current guesses for the variables' types; an expression that
-- can't be synthesized yet (e.g. because it uses a variable whose type is
-- still unknown) is just skipped until a later round.
--
-- The solved types are filled in as if they had been written down, and the
-- result is elaborated as usual, which is where any type errors (including
-- assignments that don't fit the inferred type) are reported.
module Language.Code.Infer
  ( inferDeclarations
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map

import Language.Type
import Language.Typecheck
import Language.Value.Syntax
import Language.Value.Typing (TypeEnv(..), synth, lenientTypeEnv, structuralJoin)
import Language.Code.Syntax
import Language.Code.Typecheck.Functions

-- | What is known about the type of a variable in scope.
data TyRef
  = Known FSType
  | Meta Int          -- ^ the type of the n-th inferred declaration
  | ItemOf TyRef      -- ^ the item type of a list

type Scope = Map String TyRef

-- | A lower bound, @Bound n scope funs e@: the type of @e@ (in that scope) must
-- convert to the type of the n-th inferred declaration.
data Bound = Bound Int Scope Funs Expr

type Solution = Map Int FSType

-- | Fill in the types of all @x := e@ declarations that can be inferred.
-- Declarations whose type can't be determined are left alone (and will be
-- reported when the declaration is elaborated).
inferDeclarations :: TypeEnv -> Funs -> Stmt -> Stmt
inferDeclarations tenv funs s =
  let (build, (_, cs)) = runState (collect (Map.map Known (tyVars tenv)) funs s) (0, [])
  in build (solve (reverse cs))

------------------------------------------------------
-- Collecting constraints
------------------------------------------------------

type Collect = State (Int, [Bound])

constrain :: Int -> Scope -> Funs -> Expr -> Collect ()
constrain k scope funs e = modify (fmap (Bound k scope funs e :))

-- | An assignment constrains the variable's type, if it is being inferred.
assign :: Scope -> Funs -> String -> Expr -> Collect ()
assign scope funs n e = case Map.lookup n scope of
  Just (Meta k) -> constrain k scope funs e
  _ -> pure ()

collect :: Scope -> Funs -> Stmt -> Collect (Solution -> Stmt)
collect scope funs (Stmt sr node) = (Stmt sr .) <$> case node of

  SBlock body -> fmap SBlock <$> collectBlock scope funs body

  -- a lone declaration (not part of a block)
  SDecl{} -> fmap (lone node) <$> collectBlock scope funs [Stmt sr node]
  SFun{}  -> fmap (lone node) <$> collectBlock scope funs [Stmt sr node]

  SSet n e -> assign scope funs n e $> const node

  SIf c yes no -> do
    yes' <- collect scope funs yes
    no'  <- collect scope funs no
    pure (\sol -> SIf c (yes' sol) (no' sol))

  SLoop kind c lim body -> do
    body' <- collect scope funs body
    pure (\sol -> SLoop kind c lim (body' sol))

  SIterate n e _ _ _ -> assign scope funs n e $> const node

  SPass -> pure (const node)
  SDraw{} -> pure (const node)

  SForEach item list body -> do
    body' <- collect (withItem item list) funs body
    pure (\sol -> SForEach item list (body' sol))

  SWithFirst item p list body fallback -> do
    body' <- collect (withItem item list) funs body
    fallback' <- traverse (collect scope funs) fallback
    pure (\sol -> SWithFirst item p list (body' sol) (($ sol) <$> fallback'))

  where
    withItem item list = case Map.lookup list scope of
      Just ref -> Map.insert item (ItemOf ref) scope
      Nothing  -> scope
    lone orig = \case
      [Stmt _ n] -> n
      _ -> orig

collectBlock :: Scope -> Funs -> [Stmt] -> Collect (Solution -> [Stmt])
collectBlock scope funs = \case
  [] -> pure (const [])

  Stmt sr (SDecl n Nothing e) : rest -> do
    k <- gets fst
    modify (\(i, cs) -> (i + 1, cs))
    constrain k scope funs e
    rest' <- collectBlock (Map.insert n (Meta k) scope) funs rest
    pure (\sol -> Stmt sr (SDecl n (Map.lookup k sol) e) : rest' sol)

  s@(Stmt _ (SDecl n (Just t) _)) : rest ->
    fmap (s :) <$> collectBlock (Map.insert n (Known t) scope) funs rest

  s@(Stmt _ (SFun f params body)) : rest ->
    fmap (s :) <$> collectBlock scope (Map.insert f (FunDef params body funs) funs) rest

  s : rest -> do
    s' <- collect scope funs s
    rest' <- collectBlock scope funs rest
    pure (\sol -> s' sol : rest' sol)

------------------------------------------------------
-- Solving
------------------------------------------------------

solve :: [Bound] -> Solution
solve cs = go (0 :: Int) Map.empty
  where
    -- Each round can only raise types, and there are only finitely many
    -- types to raise them through, so this limit is never reached in
    -- practice; it is just a guard.
    go n sol
      | n > 100 || sol' == sol = sol
      | otherwise = go (n + 1) sol'
      where sol' = foldl' step sol cs

    step sol (Bound k scope funs e) = case synthIn sol scope funs e of
      Nothing -> sol
      Just t  -> Map.alter (Just . maybe t (\old -> fromMaybe old (structuralJoin old t))) k sol

synthIn :: Solution -> Scope -> Funs -> Expr -> Maybe FSType
synthIn sol scope funs e = case expandCalls funs e >>= synth tenv of
  TC (Right t) -> Just (exprType t)
  TC (Left _)  -> Nothing
  where
    tenv = lenientTypeEnv (Map.mapMaybe resolve scope)
    resolve = \case
      Known t  -> Just t
      Meta k   -> Map.lookup k sol
      ItemOf r -> resolve r >>= \case
        ListT t -> Just t
        _ -> Nothing
