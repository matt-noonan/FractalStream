-- | @x stops@ in a loop's condition: true when @x@ is (approximately, as
-- by @=@) the same as it was at the start of the loop body's latest pass.
-- In a loop that checks its condition before running the body (@while@,
-- @until c:@, and @iterate@), it's false at the first check, since there
-- has been no pass yet.
--
-- This is a rewrite of the parsed code, done after @run@s are expanded
-- ("Language.Code.Procedures") and before anything is typed, so nothing
-- later has to know about @stops@. For each @x stops@ in a loop's
-- condition, the loop gets a fresh variable holding @x@'s value from the
-- start of the latest pass, and the condition compares against that:
--
-- > iterate z -> f(z) until z escapes or z stops
--
-- becomes, in a block of its own,
--
-- > prev := z
-- > started := false
-- > until z escapes or (started and z = prev):
-- >   prev <- z
-- >   started <- true
-- >   z <- f(z)
--
-- (where a loop that checks its condition only after a pass, @repeat:
-- ... until c@, doesn't need @started@). A @stops@ anywhere other than a
-- loop's condition is left alone, for typing to report as an error.
module Language.Code.Stops
  ( expandStops
  ) where

import FractalStream.Prelude

import Language.Type
import Language.Typecheck (TC)
import Language.Parser.SourceRange (SourceRange)
import Language.Code.Syntax
import Language.Value.Syntax

-- | Rewrite every @stops@ in a loop's condition.
expandStops :: Stmt -> TC Stmt
expandStops s = pure (evalState (stmt s) 0)

-- | The number of fresh names made so far.
type Fresh = State Int

fresh :: String -> Fresh String
fresh what = do
  k <- get
  put (k + 1)
  pure ("[internal] stops." ++ what ++ " #" ++ show k)

stmt :: Stmt -> Fresh Stmt
stmt (Stmt sr node) = case node of
  SBlock body -> Stmt sr . SBlock <$> mapM stmt body
  SIf c yes no -> Stmt sr <$> (SIf c <$> stmt yes <*> stmt no)
  SLoop kind c lim body -> stmt body >>= loop sr kind c lim
  SIterate n e isWhile c lim
    | hasStops c ->
        -- the same loop 'Language.Code.Typecheck' makes of an @iterate@
        loop sr (if isWhile then LoopWhile else LoopUntil) c lim (Stmt sr (SSet n e))
  SForEach item list body -> Stmt sr . SForEach item list <$> stmt body
  SWithFirst item p list body fallback ->
    Stmt sr <$> (SWithFirst item p list <$> stmt body <*> traverse stmt fallback)
  SProc name params body -> Stmt sr . SProc name params <$> stmt body
  _ -> pure (Stmt sr node)

-- | A loop, with any @stops@ in its condition rewritten.
loop :: SourceRange -> LoopKind -> Expr -> Maybe Expr -> Stmt -> Fresh Stmt
loop sr kind c lim body
  | not (hasStops c) = pure (Stmt sr (SLoop kind c lim body))
  | otherwise = do
      started <- fresh "started"
      let checksFirst = kind `elem` [LoopWhile, LoopUntil]
      (c', prevs) <- runStateT (rewrite (if checksFirst then Just started else Nothing) c) []
      let at = Stmt sr
          bool b = Syn sr (ELit (if b then "`true`" else "`false`")
                                (SomeHaskellType BooleanType b))
          decls = [ at (SDecl p Nothing x) | (p, x) <- prevs ]
               ++ [ at (SDecl started (Just BooleanT) (bool False)) | checksFirst ]
          saves = [ at (SSet p x) | (p, x) <- prevs ]
               ++ [ at (SSet started (bool True)) | checksFirst ]
      pure (at (SBlock (decls ++ [at (SLoop kind c' lim (at (SBlock (saves ++ [body]))))])))

-- | Replace each @x stops@ with a comparison against a fresh variable,
-- collecting each variable and the expression it should hold.
rewrite :: Maybe String -> Expr -> StateT [(String, Expr)] Fresh Expr
rewrite started (Syn sr node) = case node of
  EUn OpStops x -> do
    p <- lift (fresh "prev")
    modify (++ [(p, x)])
    let same = Syn sr (EBin OpApproxEq x (Syn sr (EVar p)))
    pure $ case started of
      Nothing -> same
      Just s  -> Syn sr (EBin OpAnd (Syn sr (EVar s)) same)
  _ -> Syn sr <$> traverse (rewrite started) node

hasStops :: Expr -> Bool
hasStops (Syn _ node) = case node of
  EUn OpStops _ -> True
  _ -> any hasStops node
