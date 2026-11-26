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
  , declaredIn
  , mapStmtExprs
  , mapStmtRanges
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Typecheck (TC, advise)
import Language.Parser.SourceRange (SourceRange)
import Data.Functor.Identity (Identity(..))
import Language.Code.Syntax
import Language.Value.Syntax
import Language.Type (SomeHaskellType(..), TypeProxy(..))

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
    body0 <- stmt body
    -- (`previous` of the item, or of a part of it, is the list's item
    -- before: see 'previousItems')
    (counter, body') <- previousItems sr item list body0
    (saves, body'') <- ownPrevious (Set.singleton item) [] body'
    let loopStmt = inBlock sr saves (Stmt sr (SForEach item list (startingWith sr saves body'')))
    pure $ case counter of
      Nothing -> loopStmt
      Just k -> Stmt sr (SBlock [ Stmt sr (SDecl k Nothing (intLit sr 0)), loopStmt ])
  SWithFirst item p list body fallback ->
    Stmt sr <$> (SWithFirst item p list <$> stmt body <*> traverse stmt fallback)
  SProc name params body -> Stmt sr . SProc name params <$> stmt body
  -- (functions with bodies are expanded after this, so a @previous@ in
  -- one belongs to a loop in its body, and one in a function given as an
  -- argument belongs to the loop around the call: see
  -- "Language.Code.FunctionBlocks")
  SFunction name params body -> Stmt sr . SFunction name params <$> stmt body
  _ -> pure (Stmt sr node)

-- | @previous item@ in a @for each@ loop, or @previous a@ for a part of
-- an item taken apart (@for each (a, b) in list@, which the parser makes
-- a loop over a hidden item whose body starts with @(a, b) := item@): the
-- item before this one, or, on the first pass, this one (as @previous v@
-- is @v@ itself at a @while@ loop's first check). The loop counts its
-- passes, and each pass starts by declaring the list's item before,
-- taken apart the same way:
--
-- > k := 0
-- > for each p in list:
-- >   k <- k + 1
-- >   prev := list @ (if k = 1 then 1 else k - 1)
-- >   draw line from prev to p
--
-- Gives the counter's name, if the loop needs one, and the body.
previousItems :: SourceRange -> String -> String -> Stmt -> Fresh (Maybe String, Stmt)
previousItems sr item list body = do
  let mpat = case body of
        Stmt _ (SBlock (Stmt _ (SDeclTuple p (Syn _ (EVar i))) : _)) | i == item -> Just p
        _ -> Nothing
      itemVars = Set.insert item (maybe Set.empty (Set.fromList . patternNames) mpat)
      used = Set.fromList [ v | (_, v) <- stmtPreviousUses body, v `Set.member` itemVars ]
  if Set.null used then pure (Nothing, body) else do
    k <- fresh "pass"
    prevItem <- fresh item
    renames <- forM (maybe [] patternNames mpat) $ \v -> (v,) <$> fresh v
    let byVar = Map.fromList ((item, prevItem) : renames)
        rename = \case
          PVar r n -> PVar r (Map.findWithDefault n n byVar)
          PTuple r a b -> PTuple r (rename a) (rename b)
        rewrite (Syn r node) = case node of
          EUn OpPrevious (Syn _ (EVar v)) | Just p <- Map.lookup v byVar -> Syn r (EVar p)
          _ -> Syn r (rewrite <$> node)
        var n = Syn sr (EVar n)
        before = Syn sr (EITE (Syn sr (EBin OpExactEq (var k) (intLit sr 1)))
                              (intLit sr 1)
                              (Syn sr (EBin OpSub (var k) (intLit sr 1))))
        prelude =
          [ Stmt sr (SSet k (Syn sr (EBin OpAdd (var k) (intLit sr 1))))
          , Stmt sr (SDecl prevItem Nothing (Syn sr (EIndex False (var list) before))) ]
          ++ [ Stmt sr (SDeclTuple (rename p) (var prevItem)) | Just p <- [mpat] ]
    body' <- mapStmtExprs (pure . rewrite) body
    pure (Just k, Stmt sr (SBlock (prelude ++ [body'])))

intLit :: SourceRange -> Integer -> Expr
intLit sr n = Syn sr (ELit (show n) (SomeHaskellType IntegerType (fromIntegral n)))

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
  SFunction name _ body -> Set.insert name (declaredIn body)
  _ -> Set.empty

-- | Change every source range in a statement: its statements', its
-- expressions', and its patterns'.
mapStmtRanges :: (SourceRange -> SourceRange) -> Stmt -> Stmt
mapStmtRanges g = runIdentity . mapStmtExprs (pure . fmap g) . stmts
  where
    stmts (Stmt sr node) = Stmt (g sr) $ case node of
      SBlock body -> SBlock (map stmts body)
      SDeclTuple p e -> SDeclTuple (pat p) e
      SSetTuple p e -> SSetTuple (pat p) e
      SIf c yes no -> SIf c (stmts yes) (stmts no)
      SLoop kind c lim body -> SLoop kind c lim (stmts body)
      SIterate p e isWhile c lim -> SIterate (pat p) e isWhile c lim
      SForEach item list body -> SForEach item list (stmts body)
      SWithFirst item p list body fallback -> SWithFirst item p list (stmts body) (stmts <$> fallback)
      SProc name params body -> SProc name params (stmts body)
      SFunction name params body -> SFunction name params (stmts body)
      other -> other
    pat = \case
      PVar r n -> PVar (g r) n
      PTuple r a b -> PTuple (g r) (pat a) (pat b)

-- | Apply a function to every expression in a statement.
mapStmtExprs :: Monad m => (Expr -> m Expr) -> Stmt -> m Stmt
mapStmtExprs f (Stmt sr node) = Stmt sr <$> case node of
  SBlock body -> SBlock <$> mapM go body
  SDecl n ty e -> SDecl n ty <$> f e
  SSet n e -> SSet n <$> f e
  SDeclTuple p e -> SDeclTuple p <$> f e
  SSetTuple p e -> SSetTuple p <$> f e
  SDeclHomogeneous ns e -> SDeclHomogeneous ns <$> f e
  SFun g params e -> SFun g params <$> f e
  SIf c yes no -> SIf <$> f c <*> go yes <*> go no
  SLoop kind c lim body -> SLoop kind <$> f c <*> traverse f lim <*> go body
  SIterate p e isWhile c lim -> SIterate p <$> f e <*> pure isWhile <*> f c <*> traverse f lim
  SPass -> pure SPass
  SDraw d -> SDraw <$> case d of
    DrawPointAt p -> DrawPointAt <$> f p
    DrawCircleAt filled c r unit -> DrawCircleAt filled <$> f c <*> f r <*> pure unit
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
  SFunction name params body -> SFunction name params <$> go body
  SRun name args -> SRun name <$> traverse f args
  where go = mapStmtExprs f
