{-# language OverloadedStrings, ForeignFunctionInterface, QuasiQuotes, AllowAmbiguousTypes, UndecidableInstances #-}
module Backend.LLVMSpec (spec) where

import Test.Hspec hiding (Arg)
import Data.Functor
import qualified Data.ByteString.Char8 as BS

import LLVM.Module
import LLVM.Context hiding (Context)
import LLVM.PassManager
import LLVM.OrcJIT
import LLVM.OrcJIT.CompileLayer
import LLVM.Target

import Foreign.LibFFI
import Foreign.C.Types

import Backend.LLVM.Code

import Language.Value
import Language.Value.Evaluator (ScalarTypeOfBinding)
import Language.Code
import Language.Code.Parser
--import Data.Color

import Data.IORef
import qualified Data.Map as Map
import Foreign hiding (void)
import Text.RawString.QQ
import GHC.TypeLits

data JITFun (env :: Environment) (ret :: Type) where
  JITFun :: EnvironmentProxy env -> ScalarProxy ret -> FunPtr () -> JITFun env ret

class ToForeignFun (env :: Environment) (ret :: Type) where
  type AsForeignFun env ret :: *
  toForeignFun :: (Context ScalarTypeOfBinding env -> IO (ScalarType ret))
               -> AsForeignFun env ret

instance ToForeignFun '[] ret where
  type AsForeignFun '[] ret = IO (ScalarType ret)
  toForeignFun f = f EmptyContext

instance (KnownSymbol name, KnownType t, ToForeignFun env ret, NotPresent name env)
    => ToForeignFun ( '(name,t) ': env) ret where
  type AsForeignFun ( '(name,t) ': env) ret = ScalarType t -> AsForeignFun env ret
  toForeignFun f x = toForeignFun @env @ret (f . Bind (Proxy @name) (typeProxy @t) x)

invoke :: JITFun env ret -> Context ScalarTypeOfBinding env -> IO (ScalarType ret)
invoke (JITFun _ rt f) ctx = do
  args <- fromContextM toFFIArg ctx
  callFFI f (toFFIRetType rt) args

invoke' :: forall env ret
         . ToForeignFun env ret
        => EnvironmentProxy env
        -> ScalarProxy ret
        -> JITFun env ret
        -> AsForeignFun env ret
invoke' _ _ f = toForeignFun @env @ret (invoke f)

toFFIRetType :: ScalarProxy t -> RetType (ScalarType t)
toFFIRetType = \case
  IntegerProxy -> fromIntegral <$> retInt32
  RealProxy    -> (\(CDouble x) -> x) <$> retCDouble
  _ -> error "TODO"

toFFIArg :: Proxy (name :: Symbol)
         -> ScalarProxy ty
         -> ScalarType ty
         -> IO Arg
toFFIArg _ t v = case t of
  RealProxy    -> pure (argCDouble (CDouble v))
  IntegerProxy -> pure (argInt32 (fromIntegral v))
  _ -> error "todo"

withCompiledCode :: forall env t
                  . (KnownEnvironment env, KnownType t)
                 => String
                 -> (JITFun env t -> IO ())
                 -> IO ()
withCompiledCode code test = do
  let env = envProxy (Proxy @env)
      t = typeProxy @t
  c <- case parseCode (EP NoEffs) env t code of
         Left e  -> error (show e)
         Right c -> pure c
  m <- either error pure (compile c)
  resolvers <- newIORef Map.empty
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
        withObjectLinkingLayer session resolve $ \linker ->
          withHostTargetMachineDefault $ \tm ->
          withIRCompileLayer linker tm $ \compileLayer ->
          withModuleKey session $ \mkey -> do
          withSymbolResolver session (SymbolResolver (\s -> findSymbol compileLayer s True)) $ \resolver -> do
            modifyIORef' resolvers (Map.insert mkey resolver)
            withModule compileLayer mkey md $ do
              sym <- mangleSymbol compileLayer "kernel"
              Right (JITSymbol kernelFn _) <- findSymbol compileLayer sym True
              let fn = castPtrToFunPtr (wordPtrToPtr kernelFn)
              test (JITFun env t fn)

spec :: Spec
spec = do

  describe "when compiling programs to LLVM" $ do

    it "generates the expected code for simple programs" $ do
      let env = BindingProxy (Proxy @"x") RealProxy
              $ BindingProxy (Proxy @"y") RealProxy
              $ EmptyEnvProxy
      let prog1 = [r|
loop
  set x to x + 1
  x < y
x y
|]
      withCompiledCode prog1 $ \f -> do
        f1 <- invoke' env RealProxy f 1.5 5
        f2 <- invoke' env RealProxy f 1.5 2
        f1 `shouldBe` (5.5 * 5)
        f2 `shouldBe` (2.5 * 2)

    it "can compile a basic Mandelbrot set program" $ do
      let env = BindingProxy (Proxy @"C") ComplexProxy
              $ BindingProxy (Proxy @"maxRadius") RealProxy
              $ BindingProxy (Proxy @"maxIter") IntegerProxy
              $ EmptyEnvProxy
      let mandel = [r|
init z : C to 0
init k : Z to 0
loop
  set z to z z + C
  set k to k + 1
  re(z) re(z) + im(z) im(z) < maxRadius maxRadius and k < maxIter
if k = maxIter then
  0
else
  1
|]

      withCompiledCode mandel $ \f -> do
        v <- invoke' env RealProxy f (1 :+ 1) 10 100
        v `shouldBe` 2
