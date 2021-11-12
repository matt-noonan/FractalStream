{-# language OverloadedStrings, ForeignFunctionInterface, AllowAmbiguousTypes, UndecidableInstances #-}
module Backend.LLVM
  ( JITFun(..)
  , ToForeignFun(..)
  , invoke
  , invoke'
  , withCompiledCode
  , runJX
  , type JX
  ) where

import Data.Functor
import qualified Data.ByteString.Char8 as BS

import LLVM.Module
import LLVM.Context hiding (Context)
import LLVM.PassManager
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import LLVM.Target
import LLVM.Linking
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.Relocation as Reloc

import Foreign.LibFFI
import Foreign.C.Types

import Backend.LLVM.Code

import Language.Value
import Language.Value.Evaluator (HaskellTypeOfBinding)
import Language.Code
import Language.Code.Parser
import Data.Color

import Data.IORef
import qualified Data.Map as Map
import Foreign hiding (void)
import GHC.TypeLits

data JITFun (env :: Environment) (ret :: Type) where
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

class ToForeignFun (env :: Environment) (ret :: Type) where
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
                 -> ((Ptr Word8 -> Int32 -> Int32 -> Ptr Double -> Int32 -> Double -> Double -> Double -> IO ()) -> IO ()) -- ((Ptr Word8 -> Int32 -> Double -> Ptr Double -> IO ()) -> IO ()) -- (JITFun env t -> IO ())
                 -> IO ()
withCompiledCode env code run = do
  --let env = envProxy (Proxy @env)
  --    t = typeProxy @t
  c <- case parseCode (EP NoEffs) env ColorType code of
         Left e  -> error (show e)
         Right c -> pure c
  m <- either error pure (compileRenderer c)
  resolvers <- newIORef Map.empty
  loadLibraryPermanently Nothing
  withContext $ \ctx ->
    withModuleFromAST ctx m $ \md -> do
      asm <- BS.unpack <$> moduleLLVMAssembly md
      putStrLn asm
      let pm = defaultCuratedPassSetSpec
      withPassManager pm (`runPassManager` md)
      asm' <- BS.unpack <$> moduleLLVMAssembly md
      putStrLn asm'

      withExecutionSession $ \session -> do
        let resolve k = readIORef resolvers <&> (\rs -> rs Map.! k)
        withHostTargetMachine' $ \tm ->
          withObjectLinkingLayer session resolve $ \linker ->
          withIRCompileLayer linker tm $ \compileLayer ->
          withModuleKey session $ \mkey -> do
          let resolveSymbol = SymbolResolver $ \s ->
                findSymbol compileLayer s True >>= \case
                  Right v -> pure (Right v)
                  Left e  -> do
                    putStrLn ("didn't find " ++ show s ++ ", checking in process symbols")
                    jitSymbolAddress <- getSymbolAddressInProcess s
                    putStrLn ("  found " ++ show jitSymbolAddress)
                    pure $ if jitSymbolAddress == 0
                           then Left e
                           else let jitSymbolFlags = defaultJITSymbolFlags
                                                     { jitSymbolExported = True
                                                     , jitSymbolCommon = False
                                                     , jitSymbolAbsolute = False
                                                     , jitSymbolWeak = False }
                                in Right JITSymbol{..}
          withSymbolResolver session resolveSymbol $ \resolver -> do
            modifyIORef' resolvers (Map.insert mkey resolver)
            withModule compileLayer mkey md $ do
              sym <- mangleSymbol compileLayer "kernel"
              Right (JITSymbol kernelFn _) <- findSymbol compileLayer sym True
              let fn = castPtrToFunPtr (wordPtrToPtr kernelFn)
              run (mkJX fn) -- (JITFun env t fn)

withHostTargetMachine' :: (TargetMachine -> IO a) -> IO a
withHostTargetMachine' f = do
  initializeAllTargets
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.PIC CodeModel.JITDefault CodeGenOpt.Default f
