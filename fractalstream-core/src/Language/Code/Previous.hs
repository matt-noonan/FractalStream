-- | @previous x@, in a loop: the value @x@ had at the start of the loop's
-- latest pass. In the loop's body, that's the pass running now, so
--
-- > repeat 20 times:
-- >   z <- f(z)
-- >   draw line from previous z to z
--
-- draws each step of an orbit. In the loop's condition, it's the pass
-- just finished. A loop that checks its condition before running the
-- body (@while@, @until c:@, and @iterate@) has made no pass at its
-- first check, and there @previous x@ is just @x@.
--
-- @previous@ belongs to the innermost loop around it (@for each@ counts,
-- and so does an @iterate@, for its step and its condition). Since
-- @run@s are expanded first ("Language.Code.Procedures"), a @previous@
-- in a @define@d block belongs to the loop around the @run@.
--
-- Like "Language.Code.Stops", this is a rewrite of the parsed code, done
-- before anything is typed. Each variable that a loop uses @previous@ of
-- gets a fresh variable, declared just before the loop and set at the
-- start of each pass:
--
-- > repeat:
-- >   z <- f(z)
-- >   draw line from previous z to z
-- > until z escapes
--
-- becomes, in a block of its own,
--
-- > prev := z
-- > repeat:
-- >   prev <- z
-- >   z <- f(z)
-- >   draw line from prev to z
-- > until z escapes
--
-- A @previous@ outside of any loop is left alone, for typing to report
-- as an error.
module Language.Code.Previous
  ( expandPrevious
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Typecheck (TC, advise)
import Language.Parser.SourceRange (SourceRange)
import Language.Code.Syntax
import Language.Value.Syntax

-- | Rewrite every @previous@ inside a loop.
expandPrevious :: Stmt -> TC Stmt
expandPrevious s = evalStateT (stmt s) 0

-- | The number of fresh names made so far.
type Fresh = StateT Int TC

fresh :: String -> Fresh String
fresh what = do
  k <- get
  put (k + 1)
  pure ("[internal] previous." ++ what ++ " #" ++ show k)

-- | Rewrite the loops in a statement, innermost first: by the time a loop
-- is rewritten, any @previous@ left in it is its own.
stmt :: Stmt -> Fresh Stmt
stmt (Stmt sr node) = case node of
  SBlock body -> Stmt sr . SBlock <$> mapM stmt body
  SIf c yes no -> Stmt sr <$> (SIf c <$> stmt yes <*> stmt no)
  SLoop kind c lim body -> stmt body >>= loop sr kind c lim
  SIterate p e isWhile c lim
    | hasPrevious c || hasPrevious e ->
        -- the same loop 'Language.Code.Typecheck' makes of an @iterate@
        -- (or "Language.Code.Tuples", of a tuple one)
        loop sr (if isWhile then LoopWhile else LoopUntil) c lim $ Stmt sr $ case p of
          PVar _ n -> SSet n e
          PTuple{} -> SSetTuple p e
  SForEach item list body -> do
    body' <- stmt body
    -- (the item is declared by the loop, so `previous item` is caught
    -- as a variable declared inside it)
    (saves, body'') <- ownPrevious (Set.singleton item) [] body'
    pure (inBlock sr saves (Stmt sr (SForEach item list (startingWith sr saves body''))))
  SWithFirst item p list body fallback ->
    Stmt sr <$> (SWithFirst item p list <$> stmt body <*> traverse stmt fallback)
  SProc name params body -> Stmt sr . SProc name params <$> stmt body
  _ -> pure (Stmt sr node)

-- | A loop (whose inner loops have been rewritten), with the @previous@s
-- in its condition and body rewritten.
loop :: SourceRange -> LoopKind -> Expr -> Maybe Expr -> Stmt -> Fresh Stmt
loop sr kind c lim body = do
  (saves, body') <- ownPrevious Set.empty [c] body
  let c' = rewriteWith saves c
  pure (inBlock sr saves (Stmt sr (SLoop kind c' lim (startingWith sr saves body'))))

-- | The variables a loop needs to save at the start of each pass (as
-- pairs of the fresh variable and the variable it saves), for the
-- @previous@s in the given expressions (its condition) and its body, and
-- the body with those @previous@s rewritten. It's an error to use
-- @previous@ of a variable declared inside the loop (or by it, given as
-- @declaredByLoop@): it has no value at the start of a pass.
ownPrevious :: Set String -> [Expr] -> Stmt -> Fresh ([(String, String)], Stmt)
ownPrevious declaredByLoop conds body = do
  let uses = concatMap previousUses conds ++ stmtPreviousUses body
      inside = declaredByLoop <> declaredIn body
  forM_ uses $ \(sr, v) -> when (v `Set.member` inside) $ lift $ advise sr $
    "`previous " ++ v ++ "` can't be used here: `" ++ v ++ "` is declared inside the loop, "
    ++ "so it has no value when a pass starts."
  saves <- forM (Set.toList (Set.fromList (map snd uses))) $ \v -> (, v) <$> fresh v
  body' <- mapStmtExprs (pure . rewriteWith saves) body
  pure (saves, body')

-- | Replace each @previous v@ with the variable saving @v@.
rewriteWith :: [(String, String)] -> Expr -> Expr
rewriteWith saves = go
  where
    byVar = Map.fromList [ (v, p) | (p, v) <- saves ]
    go (Syn sr node) = case node of
      EUn OpPrevious (Syn _ (EVar v)) | Just p <- Map.lookup v byVar -> Syn sr (EVar p)
      _ -> Syn sr (go <$> node)

-- | In a block of its own, the declarations of the saving variables (each
-- starting from the variable it saves), then the loop.
inBlock :: SourceRange -> [(String, String)] -> Stmt -> Stmt
inBlock _ [] s = s
inBlock sr saves s =
  Stmt sr (SBlock ([ Stmt sr (SDecl p Nothing (Syn sr (EVar v))) | (p, v) <- saves ] ++ [s]))

-- | A loop body that starts each pass by saving the variables.
startingWith :: SourceRange -> [(String, String)] -> Stmt -> Stmt
startingWith _ [] body = body
startingWith sr saves body =
  Stmt sr (SBlock ([ Stmt sr (SSet p (Syn sr (EVar v))) | (p, v) <- saves ] ++ [body]))

-- | Each @previous v@ in an expression, with where it is.
previousUses :: Expr -> [(SourceRange, String)]
previousUses (Syn sr node) = case node of
  EUn OpPrevious (Syn _ (EVar v)) -> [(sr, v)]
  _ -> foldMap previousUses node

stmtPreviousUses :: Stmt -> [(SourceRange, String)]
stmtPreviousUses s = execState (mapStmtExprs (\e -> modify (++ previousUses e) >> pure e) s) []

hasPrevious :: Expr -> Bool
hasPrevious = not . null . previousUses

-- | The variables (and functions, and blocks) declared anywhere in a
-- statement.
declaredIn :: Stmt -> Set String
declaredIn (Stmt _ node) = case node of
  SBlock body -> foldMap declaredIn body
  SDecl n _ _ -> Set.singleton n
  SDeclTuple p _ -> Set.fromList (patternNames p)
  SFun f _ _ -> Set.singleton f
  SIf _ yes no -> declaredIn yes <> declaredIn no
  SLoop _ _ _ body -> declaredIn body
  SForEach item _ body -> Set.insert item (declaredIn body)
  SWithFirst item _ _ body fallback -> Set.insert item (declaredIn body <> foldMap declaredIn fallback)
  SProc name _ body -> Set.insert name (declaredIn body)
  _ -> Set.empty

-- | Apply a function to every expression in a statement.
mapStmtExprs :: Monad m => (Expr -> m Expr) -> Stmt -> m Stmt
mapStmtExprs f (Stmt sr node) = Stmt sr <$> case node of
  SBlock body -> SBlock <$> mapM go body
  SDecl n ty e -> SDecl n ty <$> f e
  SSet n e -> SSet n <$> f e
  SDeclTuple p e -> SDeclTuple p <$> f e
  SSetTuple p e -> SSetTuple p <$> f e
  SFun g params e -> SFun g params <$> f e
  SIf c yes no -> SIf <$> f c <*> go yes <*> go no
  SLoop kind c lim body -> SLoop kind <$> f c <*> traverse f lim <*> go body
  SIterate p e isWhile c lim -> SIterate p <$> f e <*> pure isWhile <*> f c <*> traverse f lim
  SPass -> pure SPass
  SDraw d -> SDraw <$> case d of
    DrawPointAt p -> DrawPointAt <$> f p
    DrawCircleAt filled c r -> DrawCircleAt filled <$> f c <*> f r
    DrawRectFrom filled p q -> DrawRectFrom filled <$> f p <*> f q
    DrawLineFrom p q -> DrawLineFrom <$> f p <*> f q
    UseStroke c -> UseStroke <$> f c
    UseFill c -> UseFill <$> f c
    EraseAll -> pure EraseAll
    WriteAt t p -> WriteAt <$> f t <*> f p
  SSetTable n ix v -> SSetTable n <$> f ix <*> f v
  SResize n size fill -> SResize n <$> f size <*> traverse f fill
  SAllocate n size corners fill ->
    SAllocate n <$> f size <*> traverse (\(p, q) -> (,) <$> f p <*> f q) corners <*> traverse f fill
  SForEach item list body -> SForEach item list <$> go body
  SWithFirst item p list body fallback -> SWithFirst item <$> f p <*> pure list <*> go body <*> traverse go fallback
  SProc name params body -> SProc name params <$> go body
  SRun name args -> SRun name <$> traverse f args
  where go = mapStmtExprs f
