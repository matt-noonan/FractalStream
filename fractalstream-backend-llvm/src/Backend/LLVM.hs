{-# language OverloadedStrings, ForeignFunctionInterface, AllowAmbiguousTypes, UndecidableInstances, RankNTypes #-}
module Backend.LLVM
  ( JITFun(..)
  , ToForeignFun(..)
  , type LLVMJit
  , invoke
  , invoke'
  , withCompiledCode
  , withJIT
  , withJittedViewer
  , llvmToolRunner
  , compiledToolExec
  , runJX
  , type JX
  , mkKernelFun
  , type KernelFun
  ) where

import qualified Data.ByteString.Char8 as BS

import FractalStream.Prelude

import LLVM.Module
import LLVM.Context hiding (Context)
import LLVM.PassManager
import LLVM.OrcJIT
import LLVM.Target
import LLVM.Linking
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.Relocation as Reloc
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (bracket, finally)

import Foreign.LibFFI
import Foreign.C.Types
import Foreign.C.String (CString)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)

import Backend.LLVM.Code
import Backend.LLVM.Operand (listNodeStride)

import Language.Value
import Language.Value.Evaluator (HaskellValue)
import Language.Value.Transform
import Language.Code
import Language.Code.Parser (parseCode, noCommonDefs, ppFullError)
import Language.Code.InterpretIO (interpretToIOWithLastValues, ScalarIORefM)
import Language.Draw
import Actor.Viewer
import Actor.Event (ToolExec, buildHandlerWith, snapshotEventArgs, EventArgument(..), EventArgument_)
import Data.Color

import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import qualified Data.Map.Strict as Map

import Foreign hiding (void)

import Text.Disassembler.X86Disassembler

data JITFun (env :: Environment) (ret :: FSType) where
  JITFun :: EnvironmentProxy env -> TypeProxy ret -> FunPtr () -> JITFun env ret

type JX = FunPtr (Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ())

runJX :: (Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ())
      -> Ptr Word8 -> Int32 -> Int32 -> Complex Double -> Int32 -> Double -> Complex Double -> IO ()
runJX go outPtr blockSize subsamples (dx :+ dy) maxIters maxRadius (x :+ y) = do
  allocaArray @Double 2 $ \dz -> do
      pokeArray dz [dx,dy]
      go outPtr blockSize subsamples dz maxIters maxRadius x y

{-
foreign import ccall "dynamic"
  mkJX :: JX -> Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ()
-}

type KernelFun =  Ptr Word8 -- output buffer of 8-bit / channel rgb triples
               -> Int32 -- width and height of block to generate
               -> Int32 -- number of subsamples per output pixel
               -> Double -- step size dx
               -> Double -- step size dy
               -> Double -- initial x value
               -> Double -- initial y value
               -> Ptr () -- opaque context
               -> IO ()

foreign import ccall "dynamic"
  mkKernelFun :: FunPtr KernelFun -> KernelFun

class ToForeignFun (env :: Environment) (ret :: FSType) where
  type AsForeignFun env ret :: *
  toForeignFun :: (Context HaskellValue env -> IO (HaskellType ret))
               -> AsForeignFun env ret

instance ToForeignFun '[] ret where
  type AsForeignFun '[] ret = IO (HaskellType ret)
  toForeignFun f = f EmptyContext

instance (KnownSymbol name, KnownType t, ToForeignFun env ret, NotPresent name env)
    => ToForeignFun ( '(name,t) ': env) ret where
  type AsForeignFun ( '(name,t) ': env) ret = HaskellType t -> AsForeignFun env ret
  toForeignFun f x = toForeignFun @env @ret (f . Bind (Proxy @name) (typeProxy @t) x)

invoke :: JITFun env ret -> Context HaskellValue env -> IO (HaskellType ret)
invoke (JITFun _ rt f) ctx = do
  (args, frees) <- unzip <$> fromContextM toFFIArg ctx
  allocaArray @Double 2 $ \ret -> do   -- FIXME: allocate the correct type!
    callFFI f retVoid (argPtr ret : args)
    sequence_ frees
    fromFFIRetArg rt ret

invoke' :: forall env ret
         . ToForeignFun env ret
        => EnvironmentProxy env
        -> TypeProxy ret
        -> JITFun env ret
        -> AsForeignFun env ret
invoke' _ _ f = toForeignFun @env @ret (invoke f)

toFFIArg :: Proxy (name :: Symbol)
         -> TypeProxy ty
         -> HaskellType ty
         -> IO (Arg, IO ())
toFFIArg _ t v = case t of
  IntegerType -> pure (argInt32 (fromIntegral v), pure ())
  RealType    -> pure (argCDouble (CDouble v), pure ())
  ComplexType -> do
    let x :+ y = v
    z <- mallocArray 2
    pokeArray z [x,y]
    pure (argPtr z, free z)
  ColorType -> do
    let (r, g, b) = colorToRGB v
    c <- mallocArray 3
    pokeArray c [r,g,b]
    pure (argPtr c, free c)
  ListType itemTy -> do
    (headPtr, cleanup) <- buildListBuffer itemTy v
    pure (argPtr headPtr, cleanup)
  TextType -> do
    str <- GHC.newCString utf8 v
    pure (argPtr str, free str)
  BooleanType -> pure (argInt8 (if v then 1 else 0), pure ())
  _ -> error ("todo: toFFIArg " ++ showType t)

-- | Like 'toFFIArg', but for tool handlers, which may /set/ config variables.
-- Scalar values (Integer/Real/Boolean/Complex/Color) are passed as pointers and
-- bound in/out by the compiled kernel, so the caller's cell holds the final
-- value after the call.  The returned write-back action reads that cell and, if
-- the value changed and the argument is settable, propagates it back to the
-- host.  Text and lists are passed as a pointer to a cell holding a pointer
-- (to a C string, or to the list's head node), which the kernel may redirect
-- to a new string or list in the arena.  The set of in/out types must match
-- 'Backend.LLVM.Code.toolInOut'.
toFFIArgInOut :: TypeProxy ty
              -> EventArgument ty
              -> HaskellType ty
              -> IO (Arg, IO () {- free -}, IO () {- write-back -})
toFFIArgInOut t earg v = case t of
  IntegerType -> do
    p <- malloc :: IO (Ptr Int32)
    poke p (fromIntegral v)
    pure (argPtr p, free p, peek p >>= \v' -> setBack IntegerType (fromIntegral v'))
  RealType -> do
    p <- malloc :: IO (Ptr Double)
    poke p v
    pure (argPtr p, free p, peek p >>= \v' -> setBack RealType v')
  BooleanType -> do
    p <- malloc :: IO (Ptr Word8)
    poke p (if v then 1 else 0)
    pure (argPtr p, free p, peek p >>= \w -> setBack BooleanType (w /= 0))
  ComplexType -> do
    let x :+ y = v
    z <- mallocArray 2
    pokeArray z [x, y]
    let writeBack = peekArray 2 z >>= \case
          [x', y'] -> setBack ComplexType (x' :+ y')
          _        -> pure ()
    pure (argPtr z, free z, writeBack)
  ColorType -> do
    let (r, g, b) = colorToRGB v
    c <- mallocArray 3
    pokeArray c [r, g, b]
    let writeBack = peekArray 3 c >>= \case
          [r', g', b'] -> setBack ColorType (rgbToColor (r', g', b'))
          _            -> pure ()
    pure (argPtr c, free c, writeBack)
  TextType -> do
    str <- GHC.newCString utf8 v
    cell <- malloc :: IO (Ptr CString)
    poke cell str
    -- Must run while the arena (which may hold the new string) is still live.
    let writeBack = peek cell >>= \str' ->
          when (str' /= str) (GHC.peekCString utf8 str' >>= setBack TextType)
    pure (argPtr cell, free cell >> free str, writeBack)
  ListType itemTy -> do
    (headPtr, cleanup) <- buildListBuffer itemTy v
    cell <- malloc :: IO (Ptr (Ptr Word8))
    poke cell headPtr
    -- Compiled code never modifies a list's nodes in place, so the list
    -- only changed if the cell now points somewhere else. Must run while
    -- the arena (which may hold the new list) is still live.
    let writeBack = peek cell >>= \headPtr' ->
          when (headPtr' /= headPtr) (peekList itemTy headPtr' >>= setBack t)
    pure (argPtr cell, free cell >> cleanup, writeBack)
  _ -> do
    (arg, free') <- toFFIArg (Proxy @"tool-arg") t v
    pure (arg, free', pure ())
 where
  setBack ty newV = case argSetValue earg of
    Just setter | Scalar ty v /= Scalar ty newV -> setter ty newV
    _ -> pure ()

-- | Allocate and populate a contiguous buffer of linked-list nodes for a
-- Haskell list.  Returns the head pointer (null for empty) and a cleanup action.
-- Node layout (stride = listNodeStride itemTy):
--   bytes 0-7:  next pointer (null = end of list)
--   bytes 8+:   element data
buildListBuffer :: TypeProxy t -> [HaskellType t] -> IO (Ptr Word8, IO ())
buildListBuffer _      []    = pure (nullPtr, pure ())
buildListBuffer itemTy items = do
  let n      = length items
      stride = listNodeStride itemTy
  buf <- mallocBytes (n * stride) :: IO (Ptr Word8)
  cleanups <- forM (zip [0 .. n - 1] items) $ \(i, item) -> do
    let nodeBase = buf `plusPtr` (i * stride)
        nextRaw  = if i == n - 1
                   then nullPtr
                   else buf `plusPtr` ((i + 1) * stride) :: Ptr Word8
    poke (castPtr nodeBase :: Ptr (Ptr Word8)) nextRaw
    pokeListElem itemTy (nodeBase `plusPtr` 8) item
  pure (buf, sequence_ cleanups >> free buf)

-- | Write element data for a single list node at the given byte address.
-- Returns a cleanup action for anything it allocated (sub-lists and text).
pokeListElem :: TypeProxy t -> Ptr Word8 -> HaskellType t -> IO (IO ())
pokeListElem t ptr item = case t of
  BooleanType -> done $ poke (castPtr ptr :: Ptr Word8)
                             (if item then 1 else 0 :: Word8)
  IntegerType -> done $ poke (castPtr ptr :: Ptr Int32)
                             (fromIntegral item :: Int32)
  RealType    -> done $ poke (castPtr ptr :: Ptr Double) item
  ComplexType -> done $ do
    let x :+ y = item
    poke (castPtr ptr              :: Ptr Double) x
    poke (castPtr (ptr `plusPtr` 8) :: Ptr Double) y
  ColorType   -> done $ do
    let (r, g, b) = colorToRGB item
    poke ptr r
    poke (ptr `plusPtr` 1) g
    poke (ptr `plusPtr` 2) b
  ListType itemTy' -> do
    -- Nested list: recursively build and store the head pointer.
    (headPtr, cleanup) <- buildListBuffer itemTy' item
    poke (castPtr ptr :: Ptr (Ptr Word8)) headPtr
    pure cleanup
  TextType -> do
    str <- GHC.newCString utf8 item
    poke (castPtr ptr :: Ptr CString) str
    pure (free str)
  _ -> done (pure ())  -- unsupported element types: leave zeroed
 where
  done action = action >> pure (pure ())

-- | Read back a list built by 'buildListBuffer' or by compiled code (in the
-- arena), starting from its head node (null for the empty list).
peekList :: TypeProxy t -> Ptr Word8 -> IO [HaskellType t]
peekList itemTy node
  | node == nullPtr = pure []
  | otherwise = (:) <$> peekListElem itemTy (node `plusPtr` 8)
                    <*> (peek (castPtr node) >>= peekList itemTy)

-- | Read the element data of a single list node; the inverse of
-- 'pokeListElem'.
peekListElem :: TypeProxy t -> Ptr Word8 -> IO (HaskellType t)
peekListElem t ptr = case t of
  BooleanType -> (/= 0) <$> peek (castPtr ptr :: Ptr Word8)
  IntegerType -> fromIntegral <$> peek (castPtr ptr :: Ptr Int32)
  RealType    -> peek (castPtr ptr)
  ComplexType -> (:+) <$> peek (castPtr ptr) <*> peek (castPtr (ptr `plusPtr` 8))
  ColorType   -> do
    [r, g, b] <- peekArray 3 ptr
    pure (rgbToColor (r, g, b))
  ListType itemTy' -> peek (castPtr ptr) >>= peekList itemTy'
  TextType -> peek (castPtr ptr) >>= GHC.peekCString utf8
  _ -> error ("internal error: can't read a list of " ++ showType t)

fromFFIRetArg :: TypeProxy ty
              -> Ptr Double
              -> IO (HaskellType ty)
fromFFIRetArg t ptr = case t of
  IntegerType -> fromIntegral <$> peek (castPtr @_ @Int32 ptr)
  RealType    -> peek (castPtr ptr)
  ComplexType -> do
    [x,y] <- peekArray 2 (castPtr ptr)
    pure (x :+ y)
  ColorType -> do
    [cr,cg,cb] <- peekArray 3 (castPtr ptr)
    pure (rgbToColor (cr, cg, cb))
  BooleanType -> do
    v <- peek (castPtr @_ @Int8 ptr)
    pure (v /= 0)
  _ -> error ("todo: fromFFIRetArg " ++ showType t)

-- | Stride in bytes per pixel for each FSType in prep arrays.
prepArrayStride :: TypeProxy t -> Int
prepArrayStride = \case
  BooleanType -> 1
  IntegerType -> 4
  RealType    -> 8
  ComplexType -> 16
  ColorType   -> 3
  _           -> 4  -- stub for List/Text

-- | Run @action@ with one zeroed @Ptr Word8@ prep array per variable in
-- @prepOutputEnv@.  The arrays are stack-allocated and zero-initialised.
withPrepArrays :: EnvironmentProxy env -> Int -> ([Ptr Word8] -> IO r) -> IO r
withPrepArrays EmptyEnvProxy _ action = action []
withPrepArrays (BindingProxy _name ty env') nPixels action =
  let sz = nPixels * prepArrayStride ty
  in allocaBytes sz $ \ptr -> do
    fillBytes ptr 0 sz
    withPrepArrays env' nPixels $ \restPtrs ->
      action (ptr : restPtrs)

-- | Write a single Haskell value into a prep array at the given byte offset.
writeToPrepArray :: TypeProxy t -> Ptr Word8 -> Int -> HaskellType t -> IO ()
writeToPrepArray ty ptr offset val = case ty of
  BooleanType -> pokeByteOff ptr offset (if val then (1 :: Word8) else 0)
  IntegerType -> pokeByteOff ptr offset (fromIntegral val :: Int32)
  RealType    -> pokeByteOff ptr offset (val :: Double)
  ComplexType -> let re :+ im = val
                 in pokeByteOff ptr offset re >> pokeByteOff ptr (offset + 8) im
  ColorType   -> let (r, g, b) = colorToRGB val
                 in pokeByteOff ptr offset r
                 >> pokeByteOff ptr (offset + 1) g
                 >> pokeByteOff ptr (offset + 2) b
  _ -> pure ()

-- | For each variable in @prepOutputEnv@, look up its last-assigned value
-- from the interpreter's tracking map and write it to the corresponding
-- prep array at pixel index @pixelIdx@.
writePrepOutputsFromMap
  :: EnvironmentProxy prepOutputEnv
  -> [Ptr Word8]
  -> Map.Map String SomeHaskellType
  -> Int
  -> IO ()
writePrepOutputsFromMap EmptyEnvProxy [] _ _ = pure ()
writePrepOutputsFromMap (BindingProxy name ty env') (ptr:ptrs) vals pixelIdx = do
  let n = symbolVal name
      byteOffset = pixelIdx * prepArrayStride ty
  case Map.lookup n vals of
    Just (SomeHaskellType ty' val) -> writeToPrepArray ty' ptr byteOffset val
    Nothing                        -> pure ()
  writePrepOutputsFromMap env' ptrs vals pixelIdx
writePrepOutputsFromMap _ _ _ _ = pure ()

-- | No-op draw handler for use in the Haskell prep pass.
noPrepDraw :: DrawHandler ScalarIORefM
noPrepDraw = DrawHandler (\_ -> pure ())

-- | CPS wrapper that exposes the prep output environment proxy from a 'PrepScript'.
-- Avoids existential escape by keeping the proxy in the continuation's scope.
withPrepEnvProxy :: Maybe (PrepScript env)
                 -> (forall prepOutputEnv. EnvironmentProxy prepOutputEnv -> IO r)
                 -> IO r
withPrepEnvProxy Nothing                       k = k EmptyEnvProxy
withPrepEnvProxy (Just (PrepScript proxy _))   k = k proxy

withJittedViewer :: forall env t. (MissingViewerArgs env, KnownEnvironment env)
                 => LLVMJit
                 -> Maybe (PrepScript env)
                 -> Code (ViewerEnv env)
                 -> (ViewerFunction env -> IO t) -> IO t
withJittedViewer (dylib, session, compileLayer, nextId, arenaPool) mPrepScript code0 action = do
  -- Do some basic AST-level optimizations first
  let code = transformValues backendRewrites code0
  name <- modifyMVar nextId (\n -> pure (n + 1, "kernel_" ++ show n))
  withPrepEnvProxy mPrepScript $ \prepEnvProxy -> do
    m <- either error pure (compileRenderer' prepEnvProxy (fromString name) code)
    withContext $ \ctx ->
      withModuleFromAST ctx m $ \md -> do
      let pm = CuratedPassSetSpec
               { optLevel = Just 2 -- "-O2"?
               , sizeLevel = Nothing
               , unitAtATime = Nothing
               , simplifyLibCalls = Just True
               , loopVectorize = Just True
               , superwordLevelParallelismVectorize = Nothing
               , useInlinerWithThreshold = Nothing
               , dataLayout = Nothing
               , targetLibraryInfo = Nothing
               , targetMachine = Nothing
               }
      withPassManager pm (`runPassManager` md)

      let dumpLLVM = False
          dumpAsm  = False
      when dumpLLVM $ do
        putStrLn "------------------------------------------------------------"
        asm' <- BS.unpack <$> moduleLLVMAssembly md
        putStrLn asm'

      withClonedThreadSafeModule md $ \tsm -> do
        addModule tsm dylib compileLayer
        lookupSymbol session compileLayer dylib (fromString name) >>= \case
          Left err -> error ("error JITing kernel: " ++ show err)
          Right (JITSymbol kernelFn _) -> do
            when dumpLLVM $
              putStrLn "------------------------------------------------------------"

            when dumpAsm $ do
              let dcfg = defaultConfig { confIn64BitMode = True }
              instrs <- disassembleBlockWithConfig dcfg (wordPtrToPtr kernelFn) 1024
              case instrs of
                Left err -> putStrLn ("disassembly error: " ++ show err)
                Right is -> forM_ is (\i -> putStrLn ("  " ++ showIntel i))

            let fn = castPtrToFunPtr (wordPtrToPtr kernelFn)

            -- Borrow an arena from the session-wide pool for the duration of each
            -- kernel call; bracket returns it even if AsyncCancelled is thrown.
            -- The pool (and the arena lifetime) is owned by withJIT, not here,
            -- because rendering outlives this action (see LLVMJit).
            action $ ViewerFunction $ \ViewerArgs{..} ->
              bracket (readChan arenaPool) (writeChan arenaPool) $ \arena -> do
                (colorArg, colorFree) <- toFFIArg (Proxy @"color") ColorType grey
                (args, frees) <- unzip <$> fromContextM toFFIArg vaArgs
                let nPixels = fromIntegral vaWidth * fromIntegral vaHeight :: Int
                    w = fromIntegral vaWidth  :: Int
                    h = fromIntegral vaHeight :: Int
                    (x0, y0) = vaPoint
                    (dx, dy) = vaStep
                withPrepArrays prepEnvProxy nPixels $ \prepPtrs -> do
                  -- Haskell prep pass: populate prep arrays before LLVM kernel
                  case mPrepScript of
                    Nothing -> pure ()
                    Just (PrepScript _ prepCode) -> do
                      forM_ (zip [0 .. h - 1] [y0, y0 - dy ..]) $ \(row, y) ->
                        forM_ (zip [0 .. w - 1] [x0, x0 + dx ..]) $ \(col, x) -> do
                          let pixelCtx :: Context HaskellValue (ViewerEnv env)
                              pixelCtx = Bind (Proxy @InternalX)  RealType  x
                                       $ Bind (Proxy @InternalY)  RealType  y
                                       $ Bind (Proxy @InternalDX) RealType  dx
                                       $ Bind (Proxy @InternalDY) RealType  dy
                                       $ Bind (Proxy @"color")    ColorType grey
                                       $ vaArgs
                          iorefs <- mapContextM (\_ _ -> newIORef) pixelCtx
                          (lastVals, _) <- execStateT
                            (interpretToIOWithLastValues noPrepDraw prepCode)
                            (Map.empty, iorefs)
                          writePrepOutputsFromMap prepEnvProxy prepPtrs lastVals
                            (row * w + col)
                  -- Call the LLVM kernel with the arena + prep arrays as raw pointers
                  let fullArgs = argPtr   vaBuffer
                               : argInt32 vaWidth
                               : argInt32 vaHeight
                               : argInt32 vaSubsamples
                               : argPtr   arena
                               : argInt32 (fromIntegral arenaCapacity)
                               : argCDouble (CDouble $ fst vaPoint)
                               : argCDouble (CDouble $ snd vaPoint)
                               : argCDouble (CDouble $ fst vaStep)
                               : argCDouble (CDouble $ snd vaStep)
                               : colorArg
                               : args ++ map argPtr prepPtrs
                  callFFI fn retVoid fullArgs
                sequence_ (colorFree : frees)

------------------------------------------------------------------------
-- Compiled tools
------------------------------------------------------------------------

foreign import ccall "wrapper"
  wrapClear :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "wrapper"
  wrapColor3 :: (Word8 -> Word8 -> Word8 -> IO ())
             -> IO (FunPtr (Word8 -> Word8 -> Word8 -> IO ()))
foreign import ccall "wrapper"
  wrapPoint :: (CDouble -> CDouble -> IO ())
            -> IO (FunPtr (CDouble -> CDouble -> IO ()))
foreign import ccall "wrapper"
  wrapLine :: (CDouble -> CDouble -> CDouble -> CDouble -> IO ())
           -> IO (FunPtr (CDouble -> CDouble -> CDouble -> CDouble -> IO ()))
foreign import ccall "wrapper"
  wrapCircle :: (Word8 -> CDouble -> CDouble -> CDouble -> IO ())
             -> IO (FunPtr (Word8 -> CDouble -> CDouble -> CDouble -> IO ()))
foreign import ccall "wrapper"
  wrapRect :: (Word8 -> CDouble -> CDouble -> CDouble -> CDouble -> IO ())
           -> IO (FunPtr (Word8 -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()))
foreign import ccall "wrapper"
  wrapWrite :: (CString -> CDouble -> CDouble -> IO ())
            -> IO (FunPtr (CString -> CDouble -> CDouble -> IO ()))
foreign import ccall "wrapper"
  wrapFormat :: (Word8 -> CDouble -> CDouble -> Ptr Word8 -> Int32 -> IO Int32)
             -> IO (FunPtr (Word8 -> CDouble -> CDouble -> Ptr Word8 -> Int32 -> IO Int32))

-- | The number formatter compiled tool code calls for 'ToText' (see
-- 'Backend.LLVM.Operand.ArenaState'): writes the text form of an integer
-- (tag 0), real number (1), or complex number (2) as a NUL-terminated UTF-8
-- string into the given buffer, truncated to fit, and returns its length.
-- Uses 'showValue', exactly as the interpreter does.
formatNumber :: Word8 -> CDouble -> CDouble -> Ptr Word8 -> Int32 -> IO Int32
formatNumber tag (CDouble x) (CDouble y) buf cap =
  GHC.withCStringLen utf8 str $ \(src, len) -> do
    let n = min len (fromIntegral cap - 1)
    copyBytes buf (castPtr src) n
    pokeByteOff buf n (0 :: Word8)
    pure (fromIntegral n)
  where
    str = case tag of
      0 -> showValue IntegerType (round x)
      1 -> showValue RealType x
      _ -> showValue ComplexType (x :+ y)

-- | Build the C callbacks for a 'DrawSink' (plus the number formatter), run the
-- action with them, and free them afterwards.
withDrawVTablePtrs
  :: DrawSink
  -> ( ( FunPtr (IO ())
       , FunPtr (Word8 -> Word8 -> Word8 -> IO ())
       , FunPtr (Word8 -> Word8 -> Word8 -> IO ())
       , FunPtr (CDouble -> CDouble -> IO ())
       , FunPtr (CDouble -> CDouble -> CDouble -> CDouble -> IO ())
       , FunPtr (Word8 -> CDouble -> CDouble -> CDouble -> IO ())
       , FunPtr (Word8 -> CDouble -> CDouble -> CDouble -> CDouble -> IO ())
       , FunPtr (CString -> CDouble -> CDouble -> IO ())
       , FunPtr (Word8 -> CDouble -> CDouble -> Ptr Word8 -> Int32 -> IO Int32) )
       -> IO a )
  -> IO a
withDrawVTablePtrs sink action = do
  clearFP  <- wrapClear  (dsClear sink)
  strokeFP <- wrapColor3 (\r g b -> dsStroke sink (rgbToColor (r, g, b)))
  fillFP   <- wrapColor3 (\r g b -> dsFill sink (rgbToColor (r, g, b)))
  pointFP  <- wrapPoint  (\x y -> dsPoint sink (realToFrac x, realToFrac y))
  lineFP   <- wrapLine   (\x1 y1 x2 y2 ->
                            dsLine sink (realToFrac x1, realToFrac y1)
                                        (realToFrac x2, realToFrac y2))
  circleFP <- wrapCircle (\f r x y ->
                            dsCircle sink (f /= 0) (realToFrac r)
                                          (realToFrac x, realToFrac y))
  rectFP   <- wrapRect   (\f x1 y1 x2 y2 ->
                            dsRect sink (f /= 0) (realToFrac x1, realToFrac y1)
                                        (realToFrac x2, realToFrac y2))
  writeFP  <- wrapWrite  (\txt x y -> do
                            str <- GHC.peekCString utf8 txt
                            dsWrite sink str (realToFrac x, realToFrac y))
  formatFP <- wrapFormat formatNumber
  action (clearFP, strokeFP, fillFP, pointFP, lineFP, circleFP, rectFP, writeFP, formatFP)
    `finally` (   freeHaskellFunPtr clearFP  >> freeHaskellFunPtr strokeFP
               >> freeHaskellFunPtr fillFP   >> freeHaskellFunPtr pointFP
               >> freeHaskellFunPtr lineFP   >> freeHaskellFunPtr circleFP
               >> freeHaskellFunPtr rectFP   >> freeHaskellFunPtr writeFP
               >> freeHaskellFunPtr formatFP )

-- | Execute a tool event handler by JIT-compiling it and calling it, with draw
-- commands routed back to the given sink.  Compiles once per invocation for now.
compiledToolExec :: LLVMJit -> DrawSink -> ToolExec
compiledToolExec (dylib, session, compileLayer, nextId, _) sink =
  \envp code -> do
    -- Compile the handler ONCE, when the dispatcher is built.  OrcJIT's
    -- 'addModule' hands a clone of the module to the session-wide JIT, so the
    -- resulting function pointer stays valid after these brackets close.
    name <- modifyMVar nextId (\n -> pure (n + 1, "tool_" ++ show n))

    -- Same AST pre-pass as viewers: turn constant-exponent powers (e.g. z²)
    -- into multiplications instead of generic exp/log complex powers, which
    -- otherwise lose precision and shift a Newton iteration off course.
    let code' = transformValues backendRewrites code

    m <- either error pure (compileToolHandler envp (fromString name) code')
    fn <- withContext $ \llctx ->
      withModuleFromAST llctx m $ \md -> do
        let pm = CuratedPassSetSpec
                 { optLevel = Just 2
                 , sizeLevel = Nothing
                 , unitAtATime = Nothing
                 , simplifyLibCalls = Just True
                 , loopVectorize = Just True
                 , superwordLevelParallelismVectorize = Nothing
                 , useInlinerWithThreshold = Nothing
                 , dataLayout = Nothing
                 , targetLibraryInfo = Nothing
                 , targetMachine = Nothing
                 }
        _ <- withPassManager pm (`runPassManager` md)
        withClonedThreadSafeModule md $ \tsm -> do
          addModule tsm dylib compileLayer
          lookupSymbol session compileLayer dylib (fromString name) >>= \case
            Left err -> error ("error JITing tool handler: " ++ show err)
            Right (JITSymbol fnPtr _) ->
              pure (castPtrToFunPtr (wordPtrToPtr fnPtr) :: FunPtr ())

    -- The per-event invoker: snapshot args, then run the handler, retrying
    -- with a bigger arena if it runs out.
    pure $ \ctx -> snapshotEventArgs ctx >>= \case
      Nothing -> pure ()
      Just haskArgs ->
        let attempt capacity = runToolOnce fn sink capacity ctx haskArgs >>= \case
              ToolSucceeded -> pure ()
              ToolBadIndex -> fail
                "A list index was out of range, so this tool's changes were discarded."
              ToolOverflowed
                | capacity < toolArenaMaxCapacity -> attempt (2 * capacity)
                | otherwise -> fail $ concat
                    [ "This tool's text and lists used more than "
                    , show (toolArenaMaxCapacity `div` (1024 * 1024))
                    , " MB of scratch memory, so its changes were discarded." ]
        in attempt toolArenaInitialCapacity

-- | Tools get their own arena, separate from the viewers' pool: it starts
-- at 1 MB and doubles whenever a call runs out, up to 256 MB. (Tool code
-- that builds up a list with repeated @append@s uses memory quadratic in
-- the list's length, since each append copies the list.)
toolArenaInitialCapacity, toolArenaMaxCapacity :: Int
toolArenaInitialCapacity = 1024 * 1024
toolArenaMaxCapacity     = 256 * 1024 * 1024

-- | How a single run of a compiled tool handler went.
data ToolRun = ToolSucceeded | ToolOverflowed | ToolBadIndex

-- | Run a compiled tool handler once, with an arena of the given size.
-- Unless it succeeded, the run has no effect at all: the handler's draw
-- commands are held back until it has finished, and its variable changes
-- are only written back after a successful run, so after an overflow the
-- handler can simply be run again with a bigger arena.
runToolOnce :: FunPtr () -> DrawSink -> Int
            -> Context EventArgument_ env -> Context HaskellValue env
            -> IO ToolRun
runToolOnce fn sink capacity ctx haskArgs = do
  drawn <- newIORef []
  withDrawVTablePtrs (bufferedSink drawn sink) $ \(clearFP, strokeFP, fillFP, pointFP, lineFP, circleFP, rectFP, writeFP, formatFP) ->
    bracket (mallocBytes capacity) free $ \arena ->
    allocaBytes 1 $ \overflowPtr -> do
      poke overflowPtr (0 :: Word8)
      (envArgs, envFrees, envWriteBacks) <-
        unzip3 <$> fromContextM (\_ ty (earg, v) -> toFFIArgInOut ty earg v)
                                (zipContext ctx haskArgs)
      let fullArgs =
            [ argPtr (castFunPtrToPtr clearFP)
            , argPtr (castFunPtrToPtr strokeFP)
            , argPtr (castFunPtrToPtr fillFP)
            , argPtr (castFunPtrToPtr pointFP)
            , argPtr (castFunPtrToPtr lineFP)
            , argPtr (castFunPtrToPtr circleFP)
            , argPtr (castFunPtrToPtr rectFP)
            , argPtr (castFunPtrToPtr writeFP)
            , argPtr (castFunPtrToPtr formatFP)
            , argPtr arena
            , argInt32 (fromIntegral capacity)
            , argPtr overflowPtr ]
            ++ envArgs
      callFFI fn retVoid fullArgs
      status <- peek overflowPtr
      let ok = status == 0
      -- Propagate any config-variable Sets back to the host (e.g. the
      -- Select tool sets the coordinate) while the arena is still live,
      -- then free, then draw.
      when ok (sequence_ envWriteBacks)
      sequence_ envFrees
      when ok (readIORef drawn >>= sequence_ . reverse)
      -- An overflow takes priority: lists cut short by it can easily
      -- cause a bad index later on, which a bigger arena would avoid.
      pure $ if | ok               -> ToolSucceeded
                | testBit status 0 -> ToolOverflowed
                | otherwise        -> ToolBadIndex

-- | A sink that records draw commands (newest first) instead of drawing
-- them, to be replayed on the given sink later.
bufferedSink :: IORef [IO ()] -> DrawSink -> DrawSink
bufferedSink ref sink = DrawSink
  { dsClear  = later (dsClear sink)
  , dsStroke = later . dsStroke sink
  , dsFill   = later . dsFill sink
  , dsPoint  = later . dsPoint sink
  , dsLine   = \p q -> later (dsLine sink p q)
  , dsCircle = \f r p -> later (dsCircle sink f r p)
  , dsRect   = \f p q -> later (dsRect sink f p q)
  , dsWrite  = \t p -> later (dsWrite sink t p)
  }
  where later action = modifyIORef' ref (action :)

-- | A 'ToolRunner' that JIT-compiles tool event handlers via this JIT session.
llvmToolRunner :: LLVMJit -> ToolRunner
llvmToolRunner jit = ToolRunner $ \sink layer ctx handlers ->
  buildHandlerWith (compiledToolExec jit (sink layer)) ctx handlers

-- | Compile a closed script to a 'JITFun' and hand it to the given
-- continuation -- generic over the input environment and a single
-- @output@ binding the script writes its result to (so the same env also
-- has to declare that binding; see 'compile'). Only used by tests: real
-- rendering\/tool-handler code has its own, more specialized compilation
-- entry points ('compileRenderer''\/'compileToolHandler') with their own
-- calling conventions (block\/subsample loops, draw callbacks, a
-- caller-supplied arena), which this deliberately doesn't reproduce.
withCompiledCode :: forall env output t a
                   . ( KnownEnvironment env, KnownSymbol output, KnownType t
                     , Required output env ~ t
                     )
                  => Proxy output
                  -> TypeProxy t
                  -> EnvironmentProxy env
                  -> String
                  -> (JITFun env t -> IO a)
                  -> IO a
withCompiledCode outputProxy outputTy env src run = do
  code0 <- case parseCode env noCommonDefs src of
    Left (Left parseErr) -> error (ppFullError parseErr src)
    Left (Right tcErr)   -> error (ppFullError tcErr src)
    Right c               -> pure c
  -- Same pre-pass 'compiledToolExec'/'compileRenderer'' apply: constant-
  -- exponent powers (e.g. z^2) must become multiplications rather than
  -- the generic complex power (exp(n*log(z))), which is NaN at z=0 --
  -- exactly the z=0 starting point a Mandelbrot-style iteration uses.
  let code = transformValues backendRewrites code0
  m <- either error pure (compile outputProxy outputTy code)
  loadLibraryPermanently Nothing
  withContext $ \ctx ->
    withModuleFromAST ctx m $ \md -> do
      let pm = defaultCuratedPassSetSpec
      _ <- withPassManager pm (`runPassManager` md)

      withHostTargetMachine' $ \tm -> do
        withExecutionSession $ \session -> do
          withClonedThreadSafeModule md $ \tsm -> do
            let dylibName = "test_kernel_dylib"
            dylib <- createJITDylib session dylibName
            linker <- createRTDyldObjectLinkingLayer session
            compileLayer <- createIRCompileLayer session linker tm
            addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
            addModule tsm dylib compileLayer
            lookupSymbol session compileLayer dylib "kernel" >>= \case
              Left err -> error ("error JITing kernel: " ++ show err)
              Right (JITSymbol kernelFn _) ->
                run (JITFun env outputTy (castPtrToFunPtr (wordPtrToPtr kernelFn)))

-- | 1 MB arena per worker, reset per subsample inside the kernel.
arenaCapacity :: Int
arenaCapacity = 1024 * 1024

-- | The arena pool lives for the whole JIT session: the JIT'd kernels stay
-- resident until the session is torn down, and rendering runs on the wx event
-- loop *after* each 'withJittedViewer' action returns, so the arenas must
-- outlive any single viewer compile (and be shared across viewers).
type LLVMJit = (JITDylib, ExecutionSession, IRCompileLayer, MVar Int, Chan (Ptr Word8))

withJIT :: (LLVMJit -> IO t) -> IO t
withJIT action = do
  _ <- loadLibraryPermanently Nothing
  withHostTargetMachine' $ \tm -> do
    withExecutionSession $ \session -> do
      let dylibName = "kernel_dylib"
      dylib <- createJITDylib session dylibName
      linker <- createRTDyldObjectLinkingLayer session --resolve
      compileLayer <- createIRCompileLayer session linker tm
      addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
      nextId <- newMVar 0

      -- Fixed pool of arenas (one per capability), shared by all viewers.
      numWorkers <- getNumCapabilities
      arenas <- replicateM numWorkers (mallocBytes arenaCapacity :: IO (Ptr Word8))
      arenaPool <- newChan
      mapM_ (writeChan arenaPool) arenas
      -- On teardown, reading all arenas back blocks until every in-flight kernel
      -- call has returned its arena (each call holds one for exactly its
      -- duration via bracket below) and leaves the pool empty so none can start
      -- -- so freeing them and unmapping the session afterwards is safe.
      let drainAndFreeArenas =
            replicateM numWorkers (readChan arenaPool) >>= mapM_ free
      action (dylib, session, compileLayer, nextId, arenaPool)
        `finally` drainAndFreeArenas

withHostTargetMachine' :: (TargetMachine -> IO a) -> IO a
withHostTargetMachine' f = do
  initializeAllTargets
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.PIC CodeModel.JITDefault CodeGenOpt.Default f
