{-# language UndecidableInstances #-}
-- | The surface (untyped) syntax of value expressions, as produced by the
-- parser. Unlike 'Language.Value.Value', nothing here is indexed by type or
-- environment: an 'Expr' is just a tree annotated with source ranges.
-- Typing ("Language.Value.Typing") decorates the same tree with an 'FSType'
-- on every node, and elaboration ("Language.Value.Elaborate") turns that
-- decorated tree into a typed 'Language.Value.Value'.
module Language.Value.Syntax
  ( Syn(..)
  , synRange
  , ExprF(..)
  , BinOp(..)
  , UnOp(..)
  , Color3Op(..)
  , Expr
  , TExpr
  , mkExpr
  , exprType
  , stripTypes
  , exprNames
  , textLiteral
  , integerLiteral
  ) where

import qualified Data.Set as Set
import Data.Set (Set)


import Language.Type
import Language.Parser.SourceRange

-- | A tree of @f@-shaped nodes, each carrying an annotation @a@.
data Syn f a = Syn { synAnn :: a, synNode :: f (Syn f a) }

deriving instance (Show a, Show (f (Syn f a))) => Show (Syn f a)

instance Functor f => Functor (Syn f) where
  fmap g (Syn a n) = Syn (g a) (fmap (fmap g) n)

class HasRange a where range :: a -> SourceRange
instance HasRange SourceRange where range = id
instance HasRange (SourceRange, t) where range = fst

synRange :: HasRange a => Syn f a -> SourceRange
synRange = range . synAnn

data BinOp
  = OpAdd | OpSub | OpMul | OpDiv | OpIDiv | OpPow | OpMod
  | OpOr | OpAnd
  | OpExactEq     -- ^ @≡@
  | OpApproxEq    -- ^ @=@
  | OpApproxNEq   -- ^ @≠@
  | OpLt | OpGt | OpLe | OpGe
  deriving (Eq, Show)

data UnOp
  = OpNeg | OpNot | OpAbs
  | OpEscapes | OpVanishes
  | OpStops
    -- ^ @x stops@: only meaningful in a loop's condition, where
    -- "Language.Code.Stops" rewrites it away before typing
  | OpDark | OpLight | OpInvert
  deriving (Eq, Show)

data Color3Op = OpBlend | OpCycle | OpRGB | OpHSV | OpHSL
  deriving (Eq, Show)

data ExprF r
  = ELit String SomeHaskellType
    -- ^ A constant, with a description used in error messages
  | EVar String
  | ECast r FSType
  | EITE r r r
  | EPair r r
  | EList [r]
  | EBin BinOp r r
  | EUn UnOp r
  | EApp String r
    -- ^ A named unary builtin function (@sin@, @re@, @round@, @rainbow@, ...)
  | EColor3 Color3Op r r r
  | EAutocolor AutocolorMode r r
    -- ^ @autocolor(n, p)@, @nearest_autocolor(n, p)@, or
    -- @get_autocolor(n, p)@ (by mode); see "Actor.Autocolor".
    -- The group number @n@ has to be an integer literal, which typing
    -- checks (see 'integerLiteral')
  | EDiff r r
  | EText [r]
  | EJoin [r]
  | EAppend r r
  | EPrepend r r
  | ERemove r String r
  | EFind r String r r
  | ETransform r String r
  | ERange r r
  | ELength r
  | EIndex Bool r r
  | ETableIndex r [r]
  | ETablePoint r r
    -- ^ @T \@ point z@: a 2-dimensional table's cell nearest a point.
  | ETableSize r
    -- ^ @table_size(T)@: an integer, or a tuple of one per dimension.
    -- ^ A table, indexed by one integer per dimension (made from an
    -- 'EIndex' by type checking).
  | ELet String r r
    -- ^ A local binding, scoped over the second expression
  | ECall String [r]
    -- ^ A call to a user-defined function; these are expanded into 'ELet's
    -- (see "Language.Code.Typecheck") before typing
  | EDerivCall String Int [r]
    -- ^ @f'(x)@, @f''(x)@, ...: a derivative of a user-defined function of
    -- one variable, by the number of primes; expanded into a 'EDiff' (see
    -- "Language.Code.Typecheck.Functions") before typing
  | EWiden FSType r
    -- ^ The value, converted to at least the given type
  | EProj Bool r
    -- ^ The first (@False@) or second (@True@) part of a pair. No script
    -- can write one: tuple assignments and tuple norms use them (see
    -- "Language.Code.Tuples").
  | EAdvice String
    -- ^ A parse that is accepted only to produce a helpful error
  deriving (Show, Functor, Foldable, Traversable)

-- | Values straight out of the parser.
type Expr = Syn ExprF SourceRange

-- | Values after typing: every node knows its own type.
type TExpr = Syn ExprF (SourceRange, FSType)

mkExpr :: SourceRange -> ExprF Expr -> Expr
mkExpr = Syn

exprType :: TExpr -> FSType
exprType = snd . synAnn

stripTypes :: TExpr -> Expr
stripTypes = fmap fst

-- | Every variable and function name mentioned in an expression (an
-- over-approximation of what it depends on: binders are ignored).
exprNames :: Syn ExprF a -> Set String
exprNames (Syn _ node) = here <> foldMap exprNames node
  where
    here = case node of
      EVar n -> Set.singleton n
      ECall f _ -> Set.singleton f
      EDerivCall f _ _ -> Set.singleton f
      _ -> Set.empty

-- | The value of an expression that's just an integer literal.
integerLiteral :: Syn ExprF a -> Maybe Integer
integerLiteral (Syn _ (ELit _ (SomeHaskellType IntegerType n))) = Just (fromIntegral n)
integerLiteral _ = Nothing

-- | The text of an expression that's just a text literal (@"like this"@).
textLiteral :: Syn ExprF a -> Maybe String
textLiteral (Syn _ (ELit _ (SomeHaskellType TextType s))) = Just s
textLiteral _ = Nothing
