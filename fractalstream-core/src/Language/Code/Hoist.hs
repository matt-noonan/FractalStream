-- | Splitting a viewer's code into the part that's the same for every
-- pixel, which can be worked out once per render, and the rest.
--
-- A viewer's code runs once per pixel, but some of it often depends only
-- on the configuration: building a list of points of a critical orbit,
-- say, or a value from a @\@common@ block. 'splitHoistable' picks out
-- statements like that, from the top level of the code (looking into
-- blocks there, such as an expanded @run@), so that they can be run once,
-- before the pixels, and their results handed to the rest of the code as
-- if they were configuration values.
--
-- Only statements that every pixel runs anyway are taken (none from
-- inside an @if@ or a loop that depends on the pixel), so running them
-- once does no more work, and meets no more errors, than running them
-- for every pixel did. A statement can be taken from after statements
-- that depend on the pixel, as long as moving it ahead of them makes no
-- difference: it mustn't read anything they set, or set anything they
-- read or set.
--
-- This works on code whose @run@s, @stops@ and tuple assignments have
-- been expanded and whose declarations' types have been inferred (see
-- "Language.Code.Typecheck"), so every declaration says its type.
module Language.Code.Hoist
  ( Split(..)
  , splitHoistable
  , hoistedIterations
  , hoistedStuck
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Type
import Language.Code.Syntax
import Language.Code.Procedures (renameStmt)
import Language.Parser.SourceRange (SourceRange(..))
import Language.Value.Syntax
import Language.Value.Builtins (internalIterations, internalStuck)

-- | Code split in two: what can be run once, and the rest.
data Split = Split
  { splitPrefix  :: Stmt
    -- ^ The statements that can be run once, in order, followed by
    -- setting each of the 'splitOutputs' to its result.
  , splitOutputs :: [(String, FSType)]
    -- ^ Where the prefix leaves its results, with their types: for each
    -- variable it declares that the rest uses, a variable named after
    -- it ('hoistedName'), declared outside the prefix (and, if the prefix
    -- has loops, 'hoistedIterations' and 'hoistedStuck').
  , splitRest    :: Stmt
    -- ^ The rest of the code, to run for each pixel. It starts by
    -- declaring each variable it uses from the prefix, as a copy of the
    -- prefix's result: so each pixel starts from that value, whatever
    -- the pixel before did with the variable.
  }
  deriving Show

-- | The name of the variable the prefix leaves a variable's value in.
hoistedName :: String -> String
hoistedName n = "[hoisted] " ++ n

-- | Where the prefix leaves its loops' iteration count and whether the
-- last one got stuck, for the rest to start from (as it would have, had
-- they run first for each pixel).
hoistedIterations, hoistedStuck :: String
hoistedIterations = "[hoisted] iteration count"
hoistedStuck = "[hoisted] stuck"

-- | Split code (with @run@s, @stops@, and tuples expanded, and types
-- inferred) into a prefix that doesn't depend on the given names, which
-- vary from pixel to pixel, and the rest; or 'Nothing' if no work can
-- be moved into a prefix. A statement depends on a name if it reads it,
-- or reads anything set by a statement that does (and so on), or calls
-- a function defined using one.
splitHoistable :: Set String -> Stmt -> Maybe Split
splitHoistable varying0 code
  | not (any (isWork . fst) taken) = Nothing
  | otherwise = Just Split
      { splitPrefix = Stmt (stmtRange code) (SBlock (prefixStmts ++ results ++ loopResults))
      , splitOutputs = [ (hoistedName n, ty) | (n, ty) <- outputs ] ++ loopOutputs
      , splitRest = Stmt (stmtRange code) (SBlock (copies ++ restStmts))
      }
  where
    decls = declarationCounts code
    flat = flatten decls code
    (taken, restStmts) = go varying0 Set.empty Set.empty Set.empty flat

    -- (a function definition goes in both: the prefix may call it, and so
    -- may the rest)
    prefixStmts = map fst taken

    restNames = foldMap stmtNames restStmts
    outputs = [ (n, ty) | (s, True) <- taken, Stmt _ (SDecl n (Just ty) _) <- [s]
                        , n `Set.member` restNames ]
    var = Syn NoSourceRange . EVar
    results = [ Stmt NoSourceRange (SSet (hoistedName n) (var n)) | (n, _) <- outputs ]
    copies = [ Stmt NoSourceRange (SDecl n (Just ty) (var (hoistedName n))) | (n, ty) <- outputs ]

    hasLoops = any (hasLoop . fst) taken
    loopOutputs | hasLoops = [(hoistedIterations, IntegerT), (hoistedStuck, BooleanT)]
                | otherwise = []
    loopResults
      | hasLoops = [ Stmt NoSourceRange (SSet hoistedIterations (Syn NoSourceRange (EVar internalIterations)))
                   , Stmt NoSourceRange (SSet hoistedStuck (Syn NoSourceRange (EVar internalStuck))) ]
      | otherwise = []

    -- Walk the statements in order. @varying@: the names whose values
    -- (may) differ from pixel to pixel; @keptReads@, @keptWrites@: the
    -- names read and set by the statements left for the rest so far;
    -- @own@: the variables the prefix has declared so far. Each
    -- statement taken for the prefix is paired with whether it was also
    -- kept (a function definition, which goes in both).
    go :: Set String -> Set String -> Set String -> Set String -> [Stmt] -> ([(Stmt, Bool)], [Stmt])
    go _ _ _ _ [] = ([], [])
    go varying keptReads keptWrites own (s : rest)
      | isFun s, independent =
          -- a function that doesn't depend on the pixel: both can use it
          let (t, r) = go varying keptReads keptWrites own rest
          in ((s, False) : t, s : r)
      | not (isFun s), hoistable s, independent
      , Set.null (writes `Set.intersection` (keptReads <> keptWrites <> varying0))
        -- (the prefix only sets its own variables, not the
        -- configuration's, say, which every pixel starts from)
      , writes `Set.isSubsetOf` (own <> Map.keysSet (declarationCounts s) <> loopBookkeeping) =
          let (t, r) = go varying keptReads keptWrites (own <> Set.fromList (definedNames s)) rest
          in ((s, True) : t, r)
      | otherwise =
          -- left for the rest: what it sets now varies, as does a
          -- function it defines
          let (t, r) = go (varying <> writes) (keptReads <> sReads) (keptWrites <> writes) own rest
          in (t, s : r)
      where
        sReads = stmtNames s
        writes = stmtWrites s
        independent = Set.null (sReads `Set.intersection` varying) && not (stmtHasAutocolor s)

    isWork (Stmt _ node) = case node of
      SFun{} -> False
      SPass -> False
      _ -> True

    isFun (Stmt _ SFun{}) = True
    isFun _ = False

-- | Whether a statement can be moved into the prefix at all (given that
-- it doesn't depend on the pixel): it mustn't draw, or change a table,
-- and a declaration must have a type the prefix can hand on.
hoistable :: Stmt -> Bool
hoistable (Stmt _ node) = case node of
  SDecl _ (Just ty) _ -> canHandOn ty
  SDecl _ Nothing _ -> False
  SSet{} -> True
  SIf _ yes no -> hoistableInside yes && hoistableInside no
  SLoop _ _ _ body -> hoistableInside body
  SIterate{} -> True
  SForEach _ _ body -> hoistableInside body
  SWithFirst _ _ _ body fallback -> hoistableInside body && all hoistableInside fallback
  SBlock body -> all hoistableInside body
  SPass -> True
  _ -> False
  where
    -- (declarations inside are local to the prefix, so any type will do)
    hoistableInside s@(Stmt _ n) = case n of
      SDecl{} -> True
      SFun{} -> True
      _ -> hoistable s

-- | The types of values that can be handed from the prefix to the rest
-- as if they were configuration values.
canHandOn :: FSType -> Bool
canHandOn = \case
  BooleanT -> True
  IntegerT -> True
  RealT -> True
  ComplexT -> True
  ColorT -> True
  TextT -> True
  ListT _ -> True
  _ -> False

hasLoop :: Stmt -> Bool
hasLoop (Stmt _ node) = case node of
  SLoop{} -> True
  SIterate{} -> True
  SBlock body -> any hasLoop body
  SIf _ yes no -> hasLoop yes || hasLoop no
  SForEach _ _ body -> hasLoop body
  SWithFirst _ _ _ body fallback -> hasLoop body || any hasLoop fallback
  _ -> False

-- | The names a statement sets or declares (including, for a loop, the
-- iteration count and whether it got stuck).
stmtWrites :: Stmt -> Set String
stmtWrites (Stmt _ node) = case node of
  SBlock body -> foldMap stmtWrites body
  SDecl n _ _ -> Set.singleton n
  SSet n _ -> Set.singleton n
  SDeclTuple p _ -> Set.fromList (patternNames p)
  SSetTuple p _ -> Set.fromList (patternNames p)
  SFun f _ _ -> Set.singleton f
  SIf _ yes no -> stmtWrites yes <> stmtWrites no
  SLoop _ _ _ body -> loopBookkeeping <> stmtWrites body
  SIterate p _ _ _ _ -> loopBookkeeping <> Set.fromList (patternNames p)
  SForEach item _ body -> Set.insert item (stmtWrites body)
  SWithFirst item _ _ body fallback -> Set.insert item (stmtWrites body <> foldMap stmtWrites fallback)
  SSetTable n _ _ -> Set.singleton n
  SResize n _ _ -> Set.singleton n
  SAllocate n _ _ _ -> Set.singleton n
  SProc name _ _ -> Set.singleton name
  SPass -> Set.empty
  SDraw{} -> Set.empty
  SRun{} -> Set.empty

-- | What a loop sets, besides its own variables.
loopBookkeeping :: Set String
loopBookkeeping = Set.fromList [internalIterations, internalStuck]

-- | Whether a statement uses an autocolor table, which can add a point:
-- what it gives depends on the order points are seen in, so it can't be
-- moved.
stmtHasAutocolor :: Stmt -> Bool
stmtHasAutocolor (Stmt _ node) = case node of
  SBlock body -> any stmtHasAutocolor body
  SDecl _ _ e -> ex e
  SSet _ e -> ex e
  SDeclTuple _ e -> ex e
  SSetTuple _ e -> ex e
  SFun _ _ e -> ex e
  SIf c yes no -> ex c || stmtHasAutocolor yes || stmtHasAutocolor no
  SLoop _ c lim body -> ex c || any ex lim || stmtHasAutocolor body
  SIterate _ e _ c lim -> ex e || ex c || any ex lim
  SForEach _ _ body -> stmtHasAutocolor body
  SWithFirst _ p _ body fallback -> ex p || stmtHasAutocolor body || any stmtHasAutocolor fallback
  _ -> False
  where
    ex :: Expr -> Bool
    ex (Syn _ e) = case e of
      EAutocolor{} -> True
      _ -> any ex e

-- | How many times each name is declared, anywhere in the code.
declarationCounts :: Stmt -> Map String Int
declarationCounts = Map.fromListWith (+) . map (, 1) . declared
  where
    declared (Stmt _ node) = case node of
      SBlock body -> concatMap declared body
      SDecl n _ _ -> [n]
      SDeclTuple p _ -> patternNames p
      SFun f _ _ -> [f]
      SIf _ yes no -> declared yes ++ declared no
      SLoop _ _ _ body -> declared body
      SForEach item _ body -> item : declared body
      SWithFirst item _ _ body fallback -> item : declared body ++ foldMap declared fallback
      SProc name _ body -> name : declared body
      _ -> []

-- | The top level of the code, as a list of statements, with the blocks
-- in it spliced in. A variable the block declares that's also declared
-- somewhere else is renamed, to a fresh name, so that splicing the block
-- in doesn't change what any name refers to. (A name can't be declared
-- again where it's in scope, so every use of it in the rest of the block
-- is a use of the block's own.) A block that defines a function whose
-- name is declared elsewhere is left as it is.
flatten :: Map String Int -> Stmt -> [Stmt]
flatten decls code = evalState (top code) (0 :: Int)
  where
    top = \case
      Stmt _ (SBlock body) -> concat <$> mapM inner body
      s -> pure [s]
    clashes n = Map.findWithDefault 0 n decls > 1
    inner s = case s of
      Stmt _ (SBlock body)
        | not (any clashes [ f | Stmt _ (SFun f _ _) <- body ]) -> do
            renames <- forM (filter clashes (concatMap definedNames body)) $ \n -> do
              k <- state (\i -> (i, i + 1))
              pure (n, n ++ " [hoisting " ++ show k ++ "]")
            concat <$> mapM inner (map (renameStmt (Map.fromList renames)) body)
      _ -> pure [s]
