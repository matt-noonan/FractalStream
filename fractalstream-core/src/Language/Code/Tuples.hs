-- | Tuple assignments and declarations, rewritten into ordinary ones.
--
-- > x, y <- e1, e2          -- or (x, y) <- (e1, e2)
--
-- sets @x@ and @y@ at once: both values are worked out before either is
-- set, so @x, y <- y, x@ swaps them. It becomes, in the enclosing block,
--
-- > t1 := e1
-- > t2 := e2
-- > x <- t1
-- > y <- t2
--
-- with fresh names for the @t@s. A tuple declaration (@x, y := e1, e2@)
-- is the same, with declarations in place of the assignments, so that its
-- variables are in scope for the rest of the block. Patterns can nest,
-- @((a, b), c) <- ...@, and tuples nest to the right, as pairs do. Where
-- the value isn't written as a tuple of the same shape (a pair-valued
-- variable, say), it's held in one fresh variable and taken apart with
-- projections ('EProj'), which no script can write. A tuple @iterate@
-- (@iterate (x, y) -> (e1, e2) until c@) is the loop
-- "Language.Code.Typecheck" makes of an @iterate@, with a tuple
-- assignment as its body.
--
-- This runs after @run@s and @stops@ are expanded, and before anything is
-- typed, so nothing later has to know about tuples of variables.
module Language.Code.Tuples
  ( expandTuples
  ) where

import FractalStream.Prelude

import qualified Data.Set as Set

import Language.Typecheck (TC, advise)
import Language.Parser.SourceRange (SourceRange)
import Language.Code.Syntax
import Language.Value.Syntax

-- | Rewrite every tuple assignment, declaration, and @iterate@.
expandTuples :: Stmt -> TC Stmt
expandTuples s = evalStateT (stmt s) 0

-- | Counting the fresh names made so far.
type Fresh = StateT Int TC

fresh :: Fresh String
fresh = do
  k <- get
  put (k + 1)
  pure ("[internal] tuple #" ++ show k)

stmt :: Stmt -> Fresh Stmt
stmt s@(Stmt sr _) = spliced s <&> \case
  [s'] -> s'
  ss -> Stmt sr (SBlock ss)

-- | A statement, as the statements to splice into its block in its place.
spliced :: Stmt -> Fresh [Stmt]
spliced (Stmt sr node) = case node of
  SBlock body -> one . SBlock . concat <$> mapM spliced body
  SDeclTuple p e -> assignments sr p e (\n x -> SDecl n Nothing x)
  SSetTuple p e -> assignments sr p e SSet
  SIterate p@PTuple{} e isWhile c lim -> do
    body <- assignments sr p e SSet
    pure [Stmt sr (SLoop (if isWhile then LoopWhile else LoopUntil) c lim (Stmt sr (SBlock body)))]
  SIf c yes no -> one <$> (SIf c <$> stmt yes <*> stmt no)
  SLoop kind c lim body -> one . SLoop kind c lim <$> stmt body
  SForEach item list body -> one . SForEach item list <$> stmt body
  SWithFirst item p list body fallback ->
    one <$> (SWithFirst item p list <$> stmt body <*> traverse stmt fallback)
  SProc name params body -> one . SProc name params <$> stmt body
  _ -> pure [Stmt sr node]
  where
    one n = [Stmt sr n]

-- | The statements for a tuple assignment (or declaration, by @mk@):
-- each part of the value into a fresh variable, then each of those into
-- its variable.
assignments :: SourceRange -> Pattern -> Expr -> (String -> Expr -> StmtF) -> Fresh [Stmt]
assignments sr p e mk = do
  case duplicate (patternNames p) of
    Just n -> lift (advise (patternRange p) ("`" ++ n ++ "` appears more than once here."))
    Nothing -> pure ()
  (temps, finals) <- parts p e
  pure ([ Stmt sr (SDecl t Nothing x) | (t, x) <- temps ]
        ++ [ Stmt sr (mk n x) | (n, x) <- finals ])
  where
    -- the fresh variables to declare, and what each pattern variable gets
    parts :: Pattern -> Expr -> Fresh ([(String, Expr)], [(String, Expr)])
    parts pat x = case (pat, x) of
      (PTuple _ a b, Syn _ (EPair xa xb)) -> do
        (ta, fa) <- parts a xa
        (tb, fb) <- parts b xb
        pure (ta ++ tb, fa ++ fb)
      _ -> do
        t <- fresh
        pure ([(t, x)], projections pat (Syn (synRange x) (EVar t)))

    -- each pattern variable, with the part of a value it gets
    projections :: Pattern -> Expr -> [(String, Expr)]
    projections pat x = case pat of
      PVar _ n -> [(n, x)]
      PTuple psr a b -> projections a (Syn psr (EProj False x))
                     ++ projections b (Syn psr (EProj True x))

duplicate :: [String] -> Maybe String
duplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (n : ns)
      | n `Set.member` seen = Just n
      | otherwise = go (Set.insert n seen) ns
