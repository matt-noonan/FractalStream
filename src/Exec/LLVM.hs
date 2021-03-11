{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecursiveDo              #-}

module Exec.LLVM
    ( computeMandel'
    ) where

import           Prelude                         hiding (and, mod)

import           Color.Color
import           Color.Colorize
import           Exec.Region
import           Lang.Numbers                    (C (..))


import           LLVM.AST
import qualified LLVM.AST                        as AST
import           LLVM.AST.AddrSpace
import           LLVM.Context
import           LLVM.Module
import           LLVM.Target

import           LLVM.OrcJIT
import           LLVM.OrcJIT.CompileLayer

import qualified Data.ByteString.Char8           as BS

import           Control.Monad.Except

import           Data.Int
--import           Data.Word
import           Control.Concurrent
import           Data.IORef
import qualified Data.Map.Strict                 as Map
import           Foreign                         hiding (void)
import           System.IO.Unsafe

import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate       as IP
import qualified LLVM.IRBuilder.Constant         as C
import           LLVM.IRBuilder.Instruction
import qualified LLVM.IRBuilder.Module           as M
import           LLVM.IRBuilder.Monad
import           LLVM.PassManager

computeMandel' :: Colorizer C -> [C] -> IO [Color]
computeMandel' col pts = do
    let colorize = runColorizer col
        maxIter  = 100

    -- make sure that the kernel has been built
    case mkker of () -> pure ()

    fn <- ker
    finalX_ <- mallocForeignPtr
    finalY_ <- mallocForeignPtr
    steps_  <- mallocForeignPtr

    withForeignPtr finalX_ $ \finalX ->
      withForeignPtr finalY_ $ \finalY ->
        withForeignPtr steps_ $ \steps ->
          forM pts $ \(C x y) -> do
            fn x y maxIter finalX finalY steps
            colorize <$> (classify maxIter <$> peek finalX <*> peek finalY <*> peek steps)


classify :: Int32 -> Double -> Double -> Int32 -> Result C
classify maxIter x y k = Result region (C x y) (fromIntegral k)
  where region = if k == maxIter then Interior else Exterior

foreign import ccall "dynamic"
  mkKernel :: FunPtr (Double -> Double -> Int32 -> Ptr Double -> Ptr Double -> Ptr Int32 -> IO ())
           -> Double -> Double -> Int32 -> Ptr Double -> Ptr Double -> Ptr Int32 -> IO ()

withTestModule :: AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withTestModule mod f = withContext $ \context -> withModuleFromAST context mod f

theKer :: MVar (Double -> Double -> Int32 -> Ptr Double -> Ptr Double -> Ptr Int32 -> IO ())
theKer = unsafePerformIO newEmptyMVar

keepAlive :: MVar ()
keepAlive = unsafePerformIO newEmptyMVar

--ker :: Double -> Double -> Ptr Double -> Ptr Double -> Ptr Int32 -> IO ()
--ker x y finalX finalY steps = do
ker :: IO (Double -> Double -> Int32 -> Ptr Double -> Ptr Double -> Ptr Int32 -> IO ())
ker = readMVar theKer

mkker :: ()
mkker = unsafePerformIO $ void $ forkIO $ do
  let amod = kernelModule
  putStrLn "Empty resolvers"
  resolvers <- newIORef Map.empty
  putStrLn "withTestModule"
  withTestModule amod $ \mod -> do
      asm <- moduleLLVMAssembly mod
      putStrLn "ASSEMBLY: "
      BS.putStrLn asm
      putStrLn "withHostTargetMachine"
      withHostTargetMachine $ \tm -> do
        let pass = defaultCuratedPassSetSpec
                   { optLevel = Just 2
                   , loopVectorize = Just True
                   }
        withPassManager pass (void . (`runPassManager` mod))
        asm' <- moduleLLVMAssembly mod
        putStrLn "ASSEMBLY': "
        BS.putStrLn asm'
        putStrLn "withExecutionSession"
        es <- createExecutionSession
        putStrLn "withObjectLinkingLayer"
        withObjectLinkingLayer es (\k -> fmap (\rs -> rs Map.! k) (readIORef resolvers)) $ \objectLayer -> do
            putStrLn "withIRCompileLayer"
            withIRCompileLayer objectLayer tm $ \compileLayer -> do
              putStrLn "withModuleKey"
              withModuleKey es $ \k -> do
                putStrLn "withSymbolResolver"
                withSymbolResolver es (SymbolResolver (\s -> findSymbol compileLayer s True)) $ \resolver -> do
                      modifyIORef' resolvers (Map.insert k resolver)
                      putStrLn "withModule"
                      withModule compileLayer k mod $ do
                        putStrLn "mangleSymbol"
                        mainSymbol <- mangleSymbol compileLayer "mandelbrot"
                        putStrLn ("findSymbol: " ++ show mainSymbol)
                        Right (JITSymbol mainFn _) <- findSymbol compileLayer mainSymbol True
                        putStrLn ("ok, cast and return; mainFn is " ++ show mainFn)

                        let fn = castPtrToFunPtr (wordPtrToPtr mainFn)
                        putStrLn ("  fn=" ++ show fn)
                        --mkKernel fn x y finalX finalY steps
                        putMVar theKer (mkKernel fn)
                        takeMVar keepAlive

double :: Type
double = FloatingPointType DoubleFP

int32 :: Type
int32 = IntegerType 32

ptr :: Type -> Type
ptr t = PointerType t (AddrSpace 0)

kernelModule :: AST.Module
kernelModule = M.buildModule "exampleModule" $ mdo
  M.function "mandelbrot" [ (double, "Cx")
                        , (double, "Cy")
                        , (int32, "maxIter")
                        , (ptr double, "finalX")
                        , (ptr double, "finalY")
                        , (ptr int32,  "steps") ] VoidType $ \[cx,cy,maxIter,finalX,finalY,steps] -> mdo
      entry <- block `named` "entry"
      -- initialize n <- 0, x <- 0.0, y <- 0.0
      n0 <- C.int32 0
      x0 <- C.double 0.0
      y0 <- C.double 0.0
      br loop

      loop <- block `named` "loop"
      n <- phi [(n0, entry), (n', loop)]
      x <- phi [(x0, entry), (x', loop)]
      y <- phi [(y0, entry), (y', loop)]

      x2 <- x `fmul` x
      y2 <- y `fmul` y
      xy <- x `fmul` y
      two <- C.double 2
      twoxy <- two `fmul` xy
      x2py2 <- x2 `fsub` y2
      x' <- x2py2 `fadd` cx
      y' <- twoxy `fadd` cy
      one <- C.int32 1
      n' <- n `add` one
      nOK <- icmp IP.ULT n' maxIter
      x'2 <- x' `fmul` x'
      y'2 <- y' `fmul` y'
      znorm2 <- x'2 `fadd` y'2
      f100 <- C.double 100
      zOK <- fcmp FP.OLT znorm2 f100
      bothOK <- nOK `and` zOK
      condBr bothOK loop exit

      exit <- block `named` "exit"
      store finalX 0 x'
      store finalY 0 y'
      store steps  0 n'

      retVoid
