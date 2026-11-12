-- | The code generator's monad, and where values live.
--
-- Wasm is a stack machine, but the generator keeps every value in locals
-- (a 'Loc'): each computation reads its operands' locals and writes its
-- result to fresh ones. The engine's optimizing compiler does the
-- register allocation, so the extra locals cost nothing in the end, and
-- the code generator never has to track what's on the stack.
module Backend.Wasm.Gen
  ( Gen
  , runGen
  , GenResult(..)
  , Loc(..)
  , WLoc
  , Arena(..)
  , setArena
  , getArena
  , AutocolorParams(..)
  , setAutocolor
  , getAutocolor
  , allowDrawing
  , drawingAllowed
  , autocolorLookupFn
  , newLocal
  , emit
  , capture
  , unsupported
  , freshLike
  , copyInto
  , locals
    -- * Math functions, imported from the host
  , mathImports
  , mathFn
  ) where

import FractalStream.Prelude

import Backend.Wasm.Encode
import Language.Type (FSType)

import Data.List (elemIndex)

-- | Where a value lives: in one local per part. A truth value is an i32
-- (0 or 1), an integer an i32, a real number an f64, a complex number two
-- f64s, a color three i32s (one per byte, 0 to 255), a pair its parts'
-- locals, and a list an i32 (the address of its first node, or 0 for the
-- empty list: see "Backend.Wasm.List"), and a text an i32 too (the
-- address of its node, or 0 for the empty text: see "Backend.Wasm.Text").
data Loc
  = LBool Int
  | LInt Int
  | LReal Int
  | LComplex Int Int
  | LColor Int Int Int
  | LPair Loc Loc
  | LList Int
  | LText Int
  deriving Show

-- | The locals holding a kernel's scratch memory (its arena), where lists
-- are made: the next free address, the end, and the flags for running
-- out of room and for indexing a list out of range (as in the LLVM
-- backend: either makes the pixel magenta).
data Arena = Arena
  { arBump :: Int
  , arEnd :: Int
  , arOverflow :: Int
  , arBadIndex :: Int
  }

-- | Contexts of 'Loc's, for each variable in scope.
data WLoc :: Symbol -> FSType -> Exp Type
type instance Eval (WLoc name t) = Loc

-- | The locals holding the copy of the autocolor snapshot a kernel was
-- given (0 for none), and its list of misses (see
-- @fractalstream-backend-wasm/libm/fsautocolor.c@).
data AutocolorParams = AutocolorParams
  { apSnapshot :: Int
  , apMisses :: Int
  }

data GenState = GenState
  { gsNext   :: Int          -- ^ the next local's index
  , gsLocals :: [ValType]    -- ^ the locals so far, newest first
  , gsInstrs :: [Instr]      -- ^ the current block's instructions, newest first
  , gsArena  :: Maybe Arena  -- ^ where lists can be made, if anywhere
  , gsAutocolor :: Maybe AutocolorParams
  , gsDrawing :: Bool        -- ^ whether draw commands can be compiled (a tool's handler)
  }

newtype Gen a = Gen (StateT GenState (Either String) a)
  deriving newtype (Functor, Applicative, Monad, MonadError String)

data GenResult a = GenResult
  { grValue  :: a
  , grLocals :: [ValType]    -- ^ the locals after the parameters, in order
  , grBody   :: [Instr]
  }

-- | Run a generator for a function with the given parameters.
runGen :: [ValType] -> Gen a -> Either String (GenResult a)
runGen params (Gen m) = do
  (x, GenState{..}) <- runStateT m (GenState (length params) [] [] Nothing Nothing False)
  pure (GenResult x (reverse gsLocals) (reverse gsInstrs))

setArena :: Arena -> Gen ()
setArena a = Gen (modify (\st -> st { gsArena = Just a }))

getArena :: Gen Arena
getArena = Gen (gets gsArena) >>= maybe (unsupported "lists here") pure

setAutocolor :: AutocolorParams -> Gen ()
setAutocolor a = Gen (modify (\st -> st { gsAutocolor = Just a }))

getAutocolor :: Gen AutocolorParams
getAutocolor = Gen (gets gsAutocolor) >>= maybe (unsupported "autocolor here") pure

-- | Let draw commands be compiled: they go into the arena (see
-- "Backend.Wasm.Code"), so it has to be set too.
allowDrawing :: Gen ()
allowDrawing = Gen (modify (\st -> st { gsDrawing = True }))

drawingAllowed :: Gen ()
drawingAllowed = Gen (gets gsDrawing) >>= \ok -> unless ok (unsupported "drawing here")

newLocal :: ValType -> Gen Int
newLocal t = Gen $ do
  st@GenState{..} <- get
  put st { gsNext = gsNext + 1, gsLocals = t : gsLocals }
  pure gsNext

emit :: [Instr] -> Gen ()
emit is = Gen (modify (\st -> st { gsInstrs = reverse is ++ gsInstrs st }))

-- | Run a generator, giving back the instructions it emitted instead of
-- emitting them (for the body of a block, a loop, or an if).
capture :: Gen a -> Gen (a, [Instr])
capture (Gen m) = Gen $ do
  outer <- gets gsInstrs
  modify (\st -> st { gsInstrs = [] })
  x <- m
  inner <- gets gsInstrs
  modify (\st -> st { gsInstrs = outer })
  pure (x, reverse inner)

-- | Give up on compiling something: the caller can fall back to another
-- backend.
unsupported :: String -> Gen a
unsupported what = throwError ("the wasm backend can't compile " ++ what ++ " yet")

-- | Fresh locals shaped like the given value's.
freshLike :: Loc -> Gen Loc
freshLike = \case
  LBool _ -> LBool <$> newLocal I32
  LInt _ -> LInt <$> newLocal I32
  LReal _ -> LReal <$> newLocal F64
  LComplex _ _ -> LComplex <$> newLocal F64 <*> newLocal F64
  LColor{} -> LColor <$> newLocal I32 <*> newLocal I32 <*> newLocal I32
  LPair a b -> LPair <$> freshLike a <*> freshLike b
  LList _ -> LList <$> newLocal I32
  LText _ -> LText <$> newLocal I32

-- | The locals a value lives in.
locals :: Loc -> [Int]
locals = \case
  LBool a -> [a]
  LInt a -> [a]
  LReal a -> [a]
  LComplex a b -> [a, b]
  LColor r g b -> [r, g, b]
  LPair a b -> locals a ++ locals b
  LList a -> [a]
  LText a -> [a]

-- | Copy a value into another of the same shape. (Every part is read
-- before any is written, so it's right even if the two share locals.)
copyInto :: Loc -> Loc -> Gen ()
copyInto src dst = do
  let from = locals src
      to = locals dst
  when (length from /= length to) $
    throwError ("internal error: copying a value into one of another type ("
                ++ show src ++ " into " ++ show dst ++ ")")
  emit (map localGet from ++ map localSet (reverse to))

------------------------------------------------------------------------

-- | The math functions a kernel imports (from module @math@), with their
-- arities; the host supplies them (JavaScript's @Math@, and @%@ for
-- @fmod@).
mathImports :: [(String, Int)]
mathImports =
  [ ("sin", 1), ("cos", 1), ("tan", 1)
  , ("sinh", 1), ("cosh", 1), ("tanh", 1)
  , ("asin", 1), ("acos", 1), ("atan", 1)
  , ("asinh", 1), ("acosh", 1), ("atanh", 1)
  , ("exp", 1), ("log", 1)
  , ("pow", 2), ("atan2", 2), ("hypot", 2), ("fmod", 2)
  ]

-- | The index of the imported autocolor lookup (module @autocolor@, after
-- the math functions): @lookup(snapshot, mode, group, x, y, misses) ->
-- 0x00RRGGBB@.
autocolorLookupFn :: Int
autocolorLookupFn = length mathImports

-- | The index of an imported math function.
mathFn :: String -> Int
mathFn f = fromMaybe (error ("internal error: no math function " ++ f))
                     (elemIndex f (map fst mathImports))
