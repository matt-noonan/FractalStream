{-# language OverloadedStrings, ForeignFunctionInterface, AllowAmbiguousTypes, UndecidableInstances, RankNTypes #-}
module Backend.LLVM
  ( JITFun(..)
  , ToForeignFun(..)
  , type LLVMJit
  , invoke
  , invoke'
  -- , withCompiledCode
  , withJIT
  , withJittedViewer
  , llvmToolRunner
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

import Backend.LLVM.Code
import Backend.LLVM.Operand (listNodeStride)

import Language.Value
import Language.Value.Evaluator (HaskellValue)
import Language.Value.Transform
import Language.Code
import Language.Code.InterpretIO (interpretToIOWithLastValues, ScalarIORefM)
import Language.Draw
import Actor.Viewer
import Actor.Event (ToolExec, buildHandlerWith, snapshotEventArgs, EventArgument(..))
import Data.Color

import Data.IORef (newIORef)
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
  TextType -> pure (argInt32 0, pure ())
  BooleanType -> pure (argInt8 (if v then 1 else 0), pure ())
  _ -> error ("todo: toFFIArg " ++ showType t)

-- | Like 'toFFIArg', but for tool handlers, which may /set/ config variables.
-- Complex and Color values are passed as pointers and bound in/out by the
-- compiled kernel, so the caller's buffer holds the final value after the call.
-- The returned write-back action reads that buffer and, if the value changed
-- and the argument is settable, propagates it back to the host.  Other types
-- are passed by value (no write-back yet).
toFFIArgInOut :: TypeProxy ty
              -> EventArgument ty
              -> HaskellType ty
              -> IO (Arg, IO () {- free -}, IO () {- write-back -})
toFFIArgInOut t earg v = case t of
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
  forM_ (zip [0 .. n - 1] items) $ \(i, item) -> do
    let nodeBase = buf `plusPtr` (i * stride)
        nextRaw  = if i == n - 1
                   then nullPtr
                   else buf `plusPtr` ((i + 1) * stride) :: Ptr Word8
    poke (castPtr nodeBase :: Ptr (Ptr Word8)) nextRaw
    pokeListElem itemTy (nodeBase `plusPtr` 8) item
  pure (buf, free buf)

-- | Write element data for a single list node at the given byte address.
-- Returns an optional cleanup action for recursively allocated sub-lists.
pokeListElem :: TypeProxy t -> Ptr Word8 -> HaskellType t -> IO ()
pokeListElem t ptr item = case t of
  BooleanType -> poke (castPtr ptr :: Ptr Word8)
                      (if item then 1 else 0 :: Word8)
  IntegerType -> poke (castPtr ptr :: Ptr Int32)
                      (fromIntegral item :: Int32)
  RealType    -> poke (castPtr ptr :: Ptr Double) item
  ComplexType -> do
    let x :+ y = item
    poke (castPtr ptr              :: Ptr Double) x
    poke (castPtr (ptr `plusPtr` 8) :: Ptr Double) y
  ColorType   -> do
    let (r, g, b) = colorToRGB item
    poke ptr r
    poke (ptr `plusPtr` 1) g
    poke (ptr `plusPtr` 2) b
  ListType itemTy' -> do
    -- Nested list: recursively build and store the head pointer.
    (headPtr, _cleanup) <- buildListBuffer itemTy' item
    -- Note: _cleanup leaks for now; nested lists are uncommon.
    poke (castPtr ptr :: Ptr (Ptr Word8)) headPtr
  _ -> pure ()  -- unsupported element types: leave zeroed

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
  let code = transformValues (integerPowers . avoidSqrt) code0
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

-- | Build the seven C callbacks for a 'DrawSink', run the action with them, and
-- free them afterwards.
withDrawVTablePtrs
  :: DrawSink
  -> ( ( FunPtr (IO ())
       , FunPtr (Word8 -> Word8 -> Word8 -> IO ())
       , FunPtr (Word8 -> Word8 -> Word8 -> IO ())
       , FunPtr (CDouble -> CDouble -> IO ())
       , FunPtr (CDouble -> CDouble -> CDouble -> CDouble -> IO ())
       , FunPtr (Word8 -> CDouble -> CDouble -> CDouble -> IO ())
       , FunPtr (Word8 -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()) )
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
  action (clearFP, strokeFP, fillFP, pointFP, lineFP, circleFP, rectFP)
    `finally` (   freeHaskellFunPtr clearFP  >> freeHaskellFunPtr strokeFP
               >> freeHaskellFunPtr fillFP   >> freeHaskellFunPtr pointFP
               >> freeHaskellFunPtr lineFP   >> freeHaskellFunPtr circleFP
               >> freeHaskellFunPtr rectFP )

-- | Execute a tool event handler by JIT-compiling it and calling it, with draw
-- commands routed back to the given sink.  Compiles once per invocation for now.
compiledToolExec :: LLVMJit -> DrawSink -> ToolExec
compiledToolExec (dylib, session, compileLayer, nextId, arenaPool) sink =
  \envp ctx code -> snapshotEventArgs ctx >>= \case
    Nothing -> pure ()
    Just haskArgs ->
      withDrawVTablePtrs sink $ \(clearFP, strokeFP, fillFP, pointFP, lineFP, circleFP, rectFP) ->
      bracket (readChan arenaPool) (writeChan arenaPool) $ \arena ->
      allocaBytes 1 $ \overflowPtr -> do
        poke overflowPtr (0 :: Word8)
        (envArgs, envFrees, envWriteBacks) <-
          unzip3 <$> fromContextM (\_ ty (earg, v) -> toFFIArgInOut ty earg v)
                                  (zipContext ctx haskArgs)
        name <- modifyMVar nextId (\n -> pure (n + 1, "tool_" ++ show n))

        -- Same AST pre-pass as viewers: turn constant-exponent powers (e.g. z²)
        -- into multiplications instead of generic exp/log complex powers, which
        -- otherwise lose precision and shift a Newton iteration off course.
        let code' = transformValues (integerPowers . avoidSqrt) code

        m <- either error pure (compileToolHandler envp (fromString name) code')
        withContext $ \llctx ->
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
                Right (JITSymbol fnPtr _) -> do
                  let fn = castPtrToFunPtr (wordPtrToPtr fnPtr)
                      fullArgs =
                        [ argPtr (castFunPtrToPtr clearFP)
                        , argPtr (castFunPtrToPtr strokeFP)
                        , argPtr (castFunPtrToPtr fillFP)
                        , argPtr (castFunPtrToPtr pointFP)
                        , argPtr (castFunPtrToPtr lineFP)
                        , argPtr (castFunPtrToPtr circleFP)
                        , argPtr (castFunPtrToPtr rectFP)
                        , argPtr arena
                        , argInt32 (fromIntegral arenaCapacity)
                        , argPtr overflowPtr ]
                        ++ envArgs
                  callFFI fn retVoid fullArgs
                  -- Propagate any config-variable Sets back to the host
                  -- (e.g. the Select tool sets the coordinate), then free.
                  sequence_ envWriteBacks
                  sequence_ envFrees

-- | A 'ToolRunner' that JIT-compiles tool event handlers via this JIT session.
llvmToolRunner :: LLVMJit -> ToolRunner
llvmToolRunner jit = ToolRunner $ \sink layer ctx handlers ->
  buildHandlerWith (compiledToolExec jit (sink layer)) ctx handlers

{-
-- This is only used in tests
withCompiledCode :: forall env
                  . ( KnownEnvironment env
                    , Required "x" env ~ 'RealT
                    , NotPresent "x" (env `Without` "x")
                    , Required "y" env ~ 'RealT
                    , NotPresent "y" (env `Without` "y")
                    , Required "color" env ~ 'ColorT
                    , NotPresent "color" (env `Without` "color")
                    , NotPresent "#blockSize" env
                    , NotPresent "#subsamples" env
                    , NotPresent "#dz" env
                    )
                 => EnvironmentProxy env
                 -> String
                 -> ((Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ()) -> IO ())
                 -> IO ()
withCompiledCode env code run = do
  c <- case parseCode env Map.empty code of
         Left e  -> error (ppFullError e code)
         Right c -> pure c
  m <- either error pure (compileRenderer c)
  loadLibraryPermanently Nothing
  withContext $ \ctx ->
    withModuleFromAST ctx m $ \md -> do
      let pm = defaultCuratedPassSetSpec
      withPassManager pm (`runPassManager` md)
      asm' <- BS.unpack <$> moduleLLVMAssembly md
      putStrLn asm'

      withHostTargetMachine' $ \tm -> do
        withExecutionSession $ \session -> do
          withClonedThreadSafeModule md $ \tsm -> do
            let dylibName = "kernel_dylib"
            dylib <- createJITDylib session dylibName
            linker <- createRTDyldObjectLinkingLayer session --resolve
            compileLayer <- createIRCompileLayer session linker tm
            addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
            addModule tsm dylib compileLayer
            lookupSymbol session compileLayer dylib "kernel" >>= \case
              Left err -> error ("error JITing kernel: " ++ show err)
              Right (JITSymbol kernelFn _) -> do
                let fn = castPtrToFunPtr (wordPtrToPtr kernelFn)
                run (mkJX fn)
-}

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
