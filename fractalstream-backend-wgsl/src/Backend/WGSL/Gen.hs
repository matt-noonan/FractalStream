-- | The WGSL generator's monad, and where values live.
--
-- As in "Backend.Wasm.Gen", every value the generator computes goes into
-- fresh names: here, WGSL @let@s (or @var@s, for variables and for what
-- an @if@ computes), so that each computation reads named operands and
-- the shader's compiler does the rest.
module Backend.WGSL.Gen
  ( Gen
  , runGen
  , Loc(..)
  , GLoc
  , wgslType
  , emit
  , capture
  , nested
  , fresh
  , letOf
  , varLike
  , copyInto
  , force
  , parts
  , unsupported
  , precision
  ) where

import FractalStream.Prelude

import Backend.WGSL.Runtime (Precision(..))
import Language.Type (FSType)

-- | Where a value lives: a name (of a @let@ or @var@) of its WGSL type,
-- or a pair's parts. A variable whose type a kernel can't have (a list,
-- say) is 'LNone', with what it is, so that only code that uses it is
-- declined.
data Loc
  = LBool String      -- ^ bool
  | LInt String       -- ^ i32
  | LReal String      -- ^ R
  | LComplex String   -- ^ C
  | LColor String     -- ^ vec3u, a byte a channel
  | LPair Loc Loc
  | LNone String
  | LAbsC String
    -- ^ the absolute value of this complex number (C), not computed yet:
    --   a comparison computes it as it needs to (see 'force')
  deriving Show

-- | Contexts of 'Loc's, for each variable in scope.
data GLoc :: Symbol -> FSType -> Exp Type
type instance Eval (GLoc name t) = Loc

wgslType :: Loc -> Maybe String
wgslType = \case
  LBool _ -> Just "bool"
  LInt _ -> Just "i32"
  LReal _ -> Just "R"
  LComplex _ -> Just "C"
  LColor _ -> Just "vec3u"
  _ -> Nothing

data GenState = GenState
  { gsNext  :: Int
  , gsLines :: [String]   -- ^ the current block's lines, newest first
  }

newtype Gen a = Gen (ReaderT Precision (StateT GenState (Either String)) a)
  deriving newtype (Functor, Applicative, Monad, MonadError String)

-- | Run a generator, giving its lines.
runGen :: Precision -> Gen a -> Either String (a, [String])
runGen p (Gen m) = do
  (x, GenState{..}) <- runStateT (runReaderT m p) (GenState 0 [])
  pure (x, reverse gsLines)

precision :: Gen Precision
precision = Gen ask

emit :: String -> Gen ()
emit l = Gen (modify (\st -> st { gsLines = l : gsLines st }))

-- | Run a generator, giving back the lines it emitted instead of emitting
-- them.
capture :: Gen a -> Gen (a, [String])
capture (Gen m) = Gen $ do
  outer <- gets gsLines
  modify (\st -> st { gsLines = [] })
  x <- m
  inner <- gets gsLines
  modify (\st -> st { gsLines = outer })
  pure (x, reverse inner)

-- | Emit a block: the header line, the lines indented, and the closing
-- brace (with what follows it, as @} else {@).
nested :: String -> [String] -> Gen ()
nested header body = do
  emit header
  mapM_ (emit . ("  " ++)) body

fresh :: Gen String
fresh = Gen $ do
  n <- gets gsNext
  modify (\st -> st { gsNext = n + 1 })
  pure ("v" ++ show n)

-- | A fresh @let@ holding an expression.
letOf :: (String -> Loc) -> String -> Gen Loc
letOf wrap e = do
  v <- fresh
  emit ("let " ++ v ++ " = " ++ e ++ ";")
  pure (wrap v)

-- | A value, computed (an absolute value, 'LAbsC', is computed where
-- it's needed, scaled so as not to overflow).
force :: Loc -> Gen Loc
force = \case
  LAbsC z -> letOf LReal ("cabs(" ++ z ++ ")")
  LPair a b -> LPair <$> force a <*> force b
  l -> pure l

-- | Fresh @var@s shaped like a value (uninitialized).
varLike :: Loc -> Gen Loc
varLike = force >=> \case
  LPair a b -> LPair <$> varLike a <*> varLike b
  LNone what -> pure (LNone what)
  l -> do
    v <- fresh
    ty <- maybe (throwError ("internal error: no type for " ++ show l)) pure (wgslType l)
    emit ("var " ++ v ++ ": " ++ ty ++ ";")
    pure (rename v l)
  where
    rename v = \case
      LBool _ -> LBool v
      LInt _ -> LInt v
      LReal _ -> LReal v
      LComplex _ -> LComplex v
      LColor _ -> LColor v
      l -> l

-- | The names a value lives in.
parts :: Loc -> [String]
parts = \case
  LBool a -> [a]
  LInt a -> [a]
  LReal a -> [a]
  LComplex a -> [a]
  LColor a -> [a]
  LPair a b -> parts a ++ parts b
  LNone _ -> []
  LAbsC _ -> []

-- | Assign a value to variables of the same shape.
copyInto :: Loc -> Loc -> Gen ()
copyInto src0 dst = do
  src <- force src0
  let (from, to) = (parts src, parts dst)
  when (length from /= length to) $
    throwError ("internal error: copying a value into one of another type ("
                ++ show src ++ " into " ++ show dst ++ ")")
  -- (all read before any is written, in case they overlap)
  tmps <- forM from $ \f -> do { t <- fresh; emit ("let " ++ t ++ " = " ++ f ++ ";"); pure t }
  zipWithM_ (\t d -> emit (d ++ " = " ++ t ++ ";")) tmps to

-- | Decline: the GPU backend can't draw this yet (GPU_PLAN.md,
-- "Declining").
unsupported :: String -> Gen a
unsupported what = throwError ("not on the GPU yet: " ++ what)
