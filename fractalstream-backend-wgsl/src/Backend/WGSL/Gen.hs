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
  , noteListSet
  , reclaiming
  , nested
  , fresh
  , letOf
  , letName
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
  | LList String      -- ^ u32: the list's address (see Backend.WGSL.Runtime)
  | LTable String     -- ^ u32: the table's descriptor (see Backend.WGSL.Runtime)
  | LNone String
  | LAbsC String
    -- ^ the absolute value of this complex number (C), not computed yet:
    --   a comparison computes it as it needs to (see 'force')
  | LRange String String
    -- ^ the integers from one i32 to another, as a list not made yet:
    --   @for each@ counts through them (see 'force')
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
  LList _ -> Just "u32"
  LTable _ -> Just "u32"
  _ -> Nothing

data GenState = GenState
  { gsNext  :: Int
  , gsLines :: [String]   -- ^ the current block's lines, newest first
  , gsListSet :: Bool     -- ^ whether a variable has been set to a list
  }

newtype Gen a = Gen (ReaderT Precision (StateT GenState (Either String)) a)
  deriving newtype (Functor, Applicative, Monad, MonadError String)

-- | Run a generator, giving its lines.
runGen :: Precision -> Gen a -> Either String (a, [String])
runGen p (Gen m) = do
  (x, GenState{..}) <- runStateT (runReaderT m p) (GenState 0 [] False)
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

-- | A variable was set to a list (or a tuple with one): a list made in a
-- loop's body may outlive its iteration.
noteListSet :: Gen ()
noteListSet = Gen (modify (\st -> st { gsListSet = True }))

-- | A loop body's lines, which give back the arena they used at the end
-- of each iteration if no list they make can outlive it (none of them
-- set a variable to a list): so that a loop of thousands of iterations,
-- each making a small list (@for each k in range(1, 10)@), doesn't run
-- out. The body gets where to put that, before its last lines (its test).
reclaiming :: (Gen () -> Gen a) -> Gen (a, [String])
reclaiming body = do
  saved <- fresh
  outer <- Gen (gets gsListSet)
  Gen (modify (\st -> st { gsListSet = False }))
  (x, ls) <- capture (body (emit ("bump = " ++ saved ++ ";")))
  inner <- Gen (gets gsListSet)
  Gen (modify (\st -> st { gsListSet = outer || inner }))
  pure $ if inner
    then (x, filter (/= ("bump = " ++ saved ++ ";")) ls)
    else (x, ("let " ++ saved ++ " = bump;") : ls)

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
  -- (a range, made into a list after all: as Backend.WGSL.Data's listRange)
  LRange lo hi -> do
    n <- letName (hi ++ " - " ++ lo ++ " + 1")
    h <- fresh
    p <- fresh
    k <- fresh
    emit ("var " ++ h ++ ": u32 = 0u;")
    nested ("if (" ++ n ++ " > 0) {")
      [ "let " ++ p ++ " = alloc(1u + u32(" ++ n ++ "));"
      , "if (" ++ p ++ " != 0u) {"
      , "  sw(" ++ p ++ ", u32(" ++ n ++ "));"
      , "  for (var " ++ k ++ " = 0; " ++ k ++ " < " ++ n ++ "; " ++ k ++ "++) { sw(" ++ p ++ " + 1u + u32(" ++ k ++ "), bitcast<u32>(" ++ lo ++ " + " ++ k ++ ")); }"
      , "  " ++ h ++ " = " ++ p ++ ";"
      , "}" ]
    emit "}"
    pure (LList h)
  LPair a b -> LPair <$> force a <*> force b
  l -> pure l

-- | A fresh @let@ holding an expression: its name.
letName :: String -> Gen String
letName e = do
  v <- fresh
  emit ("let " ++ v ++ " = " ++ e ++ ";")
  pure v

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
      LList _ -> LList v
      LTable _ -> LTable v
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
  LList a -> [a]
  LTable a -> [a]
  LNone _ -> []
  LAbsC _ -> []
  LRange _ _ -> []

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
