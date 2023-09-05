{-# language OverloadedStrings, ForeignFunctionInterface, AllowAmbiguousTypes, UndecidableInstances #-}
module Backend.LLVM
  ( JITFun(..)
  , ToForeignFun(..)
  , type LLVMJit
  , invoke
  , invoke'
  , withCompiledCode
  , withViewerCode
  , withJIT
  , withViewerCode'
  , runJX
  , type JX
  , mkKernelFun
  , type KernelFun
  ) where

import qualified Data.ByteString.Char8 as BS

import LLVM.Module
import LLVM.Context hiding (Context)
import LLVM.PassManager
import LLVM.OrcJIT
import LLVM.Target
import LLVM.Linking
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.Relocation as Reloc
import Control.Concurrent.MVar
import Data.String

import Foreign.LibFFI
import Foreign.C.Types

import Backend.LLVM.Code

import Language.Value
import Language.Value.Evaluator (HaskellTypeOfBinding)
import Language.Value.Transform
import Language.Code
import Language.Code.Parser
import Data.Color

import Foreign hiding (void)
import GHC.TypeLits

import Text.Disassembler.X86Disassembler
import Control.Monad (forM_)

data JITFun (env :: Environment) (ret :: FSType) where
  JITFun :: EnvironmentProxy env -> TypeProxy ret -> FunPtr () -> JITFun env ret

type JX = FunPtr (Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ())

runJX :: (Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ())
      -> Ptr Word8 -> Int32 -> Int32 -> Complex Double -> Int32 -> Double -> Complex Double -> IO ()
runJX go outPtr blockSize subsamples (dx :+ dy) maxIters maxRadius (x :+ y) = do
  allocaArray @Double 2 $ \dz -> do
      pokeArray dz [dx,dy]
      go outPtr blockSize subsamples dz maxIters maxRadius x y

foreign import ccall "dynamic"
  mkJX :: JX -> Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ()


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
  toForeignFun :: (Context HaskellTypeOfBinding env -> IO (HaskellType ret))
               -> AsForeignFun env ret

instance ToForeignFun '[] ret where
  type AsForeignFun '[] ret = IO (HaskellType ret)
  toForeignFun f = f EmptyContext

instance (KnownSymbol name, KnownType t, ToForeignFun env ret, NotPresent name env)
    => ToForeignFun ( '(name,t) ': env) ret where
  type AsForeignFun ( '(name,t) ': env) ret = HaskellType t -> AsForeignFun env ret
  toForeignFun f x = toForeignFun @env @ret (f . Bind (Proxy @name) (typeProxy @t) x)

invoke :: JITFun env ret -> Context HaskellTypeOfBinding env -> IO (HaskellType ret)
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

  _ -> error ("todo: toFFIArg " ++ showType t)

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

  _ -> error ("todo: fromFFIRetArg " ++ showType t)

withCompiledCode :: forall env
                  . ( KnownEnvironment env
                    , Required "x" env ~ 'RealT
                    , NotPresent "x" (env `Without` "x")
                    , Required "y" env ~ 'RealT
                    , NotPresent "y" (env `Without` "y")
                    , NotPresent "#blockSize" env
                    , NotPresent "#subsamples" env
                    , NotPresent "#dz" env
                    )
                 => EnvironmentProxy env
                 -> String
                 -> ((Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ()) -> IO ())
                 -> IO ()
withCompiledCode env code run = do
  c <- case parseCode (EP NoEffs) env EmptyContext ColorType code of
         Left e  -> error (show e)
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

withViewerCode :: forall x y dx dy env t
                  . ( KnownEnvironment env
                    , NotPresent "[internal argument] #blockSize" env
                    , NotPresent "[internal argument] #subsamples" env
                    , KnownSymbol x, KnownSymbol y
                    , KnownSymbol dx, KnownSymbol dy
                    , Required x env ~ 'RealT
                    , NotPresent x (env `Without` x)
                    , Required y env ~ 'RealT
                    , NotPresent y (env `Without` y)
                    , Required dx env ~ 'RealT
                    , NotPresent dx (env `Without` dx)
                    , Required dy env ~ 'RealT
                    , NotPresent dy (env `Without` dy)
                    )
                 => Proxy x
                 -> Proxy y
                 -> Proxy dx
                 -> Proxy dy
                 -> Code '[] env 'ColorT
                 -> ((Int32 -> Int32 -> Context HaskellTypeOfBinding env -> Ptr Word8 -> IO ())
                      -> IO t)
                 -> IO t
withViewerCode x y dx dy c action = do
  m <- either error pure (compileRenderer' "kernel" x y dx dy c)
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
                r <- action $ \blocksize subsamples argCtx buf -> do
                  (args, frees) <- unzip <$> fromContextM toFFIArg argCtx
                  let fullArgs = argPtr buf
                               : argInt32 blocksize
                               : argInt32 subsamples
                               : args
                  callFFI fn retVoid fullArgs
                  sequence_ frees

                putStrLn "************ WARNING, VIEWER CODE IS DEAD TO ME ****************"
                pure r

withViewerCode' :: forall x y dx dy env t
                  . ( KnownEnvironment env
                    , NotPresent "[internal argument] #blockSize" env
                    , NotPresent "[internal argument] #subsamples" env
                    , KnownSymbol x, KnownSymbol y
                    , KnownSymbol dx, KnownSymbol dy
                    , Required x env ~ 'RealT
                    , NotPresent x (env `Without` x)
                    , Required y env ~ 'RealT
                    , NotPresent y (env `Without` y)
                    , Required dx env ~ 'RealT
                    , NotPresent dx (env `Without` dx)
                    , Required dy env ~ 'RealT
                    , NotPresent dy (env `Without` dy)
                    )
                 => LLVMJit
                 -> Proxy x
                 -> Proxy y
                 -> Proxy dx
                 -> Proxy dy
                 -> Code '[] env 'ColorT
                 -> ((Int32 -> Int32 -> Context HaskellTypeOfBinding env -> Ptr Word8 -> IO ())
                      -> IO t)
                 -> IO t
withViewerCode' (dylib, session, compileLayer, nextId) x y dx dy c action = do

  let optimizedCode = transformValues (integerPowers . avoidSqrt) c

  name <- modifyMVar nextId (\n -> pure (n + 1, "kernel_" ++ show n))
  m <- either error pure (compileRenderer' (fromString name) x y dx dy optimizedCode)
  withContext $ \ctx ->
    withModuleFromAST ctx m $ \md -> do
    let pm = defaultCuratedPassSetSpec
    withPassManager pm (`runPassManager` md)
    asm' <- BS.unpack <$> moduleLLVMAssembly md

    putStrLn "------------------------------------------------------------"
    putStrLn asm'

    withClonedThreadSafeModule md $ \tsm -> do
      addModule tsm dylib compileLayer
      lookupSymbol session compileLayer dylib (fromString name) >>= \case
        Left err -> error ("error JITing kernel: " ++ show err)
        Right (JITSymbol kernelFn _) -> do
          putStrLn "------------------------------------------------------------"
          let dcfg = defaultConfig { confIn64BitMode = True }
          instrs <- disassembleBlockWithConfig dcfg (wordPtrToPtr kernelFn) 1024
          case instrs of
            Left err -> putStrLn ("disassembly error: " ++ show err)
            Right is -> forM_ is (\i -> putStrLn ("  " ++ showIntel i))

          let fn = castPtrToFunPtr (wordPtrToPtr kernelFn)
          action $ \blocksize subsamples argCtx buf -> do
            (args, frees) <- unzip <$> fromContextM toFFIArg argCtx
            let fullArgs = argPtr buf
                         : argInt32 blocksize
                         : argInt32 subsamples
                         : args
            callFFI fn retVoid fullArgs
            sequence_ frees

type LLVMJit = (JITDylib, ExecutionSession, IRCompileLayer, MVar Int)

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
      action (dylib, session, compileLayer, nextId)

withHostTargetMachine' :: (TargetMachine -> IO a) -> IO a
withHostTargetMachine' f = do
  initializeAllTargets
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.PIC CodeModel.JITDefault CodeGenOpt.Default f
