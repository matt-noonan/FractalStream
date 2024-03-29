{-# language RecursiveDo, OverloadedStrings #-}
module Backend.LLVM.Code
  ( compile
  , compileRenderer
  , compileRenderer'
  ) where

import Backend.LLVM.Operand
import Backend.LLVM.Value

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Monad
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.AST.IntegerPredicate as P

import Control.Monad.Except
import Control.Monad.Reader

import Language.Type
import Language.Code
import Data.Indexed.Functor
import Fcf (Exp, Eval, Pure1)
import GHC.TypeLits
import Data.String

import Debug.Trace

toParameterList :: EnvironmentProxy env -> [(AST.Type, ParameterName)]
toParameterList = \case
  EmptyEnvProxy -> []
  BindingProxy name t env ->
    (toLLVMType t, ParameterName (fromString (symbolVal name))) : toParameterList env

compile :: forall env t
         . (KnownEnvironment env, KnownType t)
        => Code '[] env t
        -> Either String AST.Module
compile code = runExcept $
  buildModuleT "compiled code" $ do
    let retParam = (toLLVMPtrType (typeProxy @t), NoParameterName)
        params   = toParameterList (envProxy (Proxy @env))
    function "kernel" (retParam : params) AST.void $ \(retArg : rawArgs) -> do
      getExtern <- getGetExtern
      traceM ("making typedOperandPtr, return type is " ++ showType (typeProxy @t))
      traceM ("  retArg = " ++ show retArg)
      retPtr <- typedOperandPtr (typeProxy @t) retArg
      traceM ("ok, continuing... retPtr = " ++ show retPtr)
      args <- allocaArgs (envProxy (Proxy @env)) rawArgs
      rv <- runReaderT (compileCode getExtern code) args
      storeOperand rv retPtr
      retVoid

type RenderEnv env =
  (  '("#blockSize", 'IntegerT)
  ': '("#subsamples", 'IntegerT)
  ': '("#dz", 'ComplexT)
  ': env )

compileRenderer :: forall env
                 . ( KnownEnvironment (RenderEnv env)
                   , KnownEnvironment env
                   , Required "x" env ~ 'RealT
                   , NotPresent "x" (env `Without` "x")
                   , Required "y" env ~ 'RealT
                   , NotPresent "y" (env `Without` "y")
                   )
                => Code '[] env 'ColorT
                -> Either String AST.Module
compileRenderer code = runExcept $
  buildModuleT "compiled rendering kernel" $ do
    let retParam = (toLLVMPtrType ColorType, NoParameterName)
        params   = toParameterList (envProxy (Proxy @(RenderEnv env)))
        pfX = bindingEvidence @"x" @'RealT @env
        pfY = bindingEvidence @"y" @'RealT @env
    function "kernel" (retParam : params) AST.void $ \(retPtr : blockSizeArg : subsamplesArg : dzArg : rawArgs) -> do
      getExtern <- getGetExtern
      traceM ("ok... retPtr = " ++ show retPtr)
      --( Bind _ _ blockSizePtr
      --  ( Bind _ _ subsamplesPtr
      --    (Bind _ _ dzPtr
      --       args))) <- allocaArgs (envProxy (Proxy @(RenderEnv env))) rawArgs
      blockSizePtr <- allocaArg IntegerType blockSizeArg `named` "set up environment"
      subsamplesPtr <- allocaArg IntegerType subsamplesArg
      dzPtr <- allocaArg ComplexType dzArg
      args <- allocaArgs (envProxy (Proxy @env)) rawArgs
      mdo

        x0 <- derefOperand (getBinding args pfX) >>= \case
          RealOp v -> pure v
        y0 <- derefOperand (getBinding args pfY) >>= \case
          RealOp v -> pure v
        (dx, dy) <- derefOperand dzPtr >>= \case
          ComplexOp vx vy -> pure (vx, vy)
        blockSize <- derefOperand blockSizePtr >>= \case
          IntegerOp v -> pure v
        subsamples <- derefOperand subsamplesPtr >>= \case
          IntegerOp v -> pure v
        indexPtr <- alloca AST.i32 Nothing 0 `named` "set up loop variables"
        iPtr <- alloca AST.i32 Nothing 0
        jPtr <- alloca AST.i32 Nothing 0
        kPtr <- alloca AST.i32 Nothing 0
        xPtr <- alloca AST.double Nothing 0
        yPtr <- alloca AST.double Nothing 0

        -- index = 0;
        -- y = 0;
        -- for (i = 0; i < blockSize; ++i) {
        store indexPtr 0 (C.int32 0) `named` "initialize i loop"
        store yPtr 0 y0
        store iPtr 0 (C.int32 0)
        br pixelLoopY

        pixelLoopY <- block `named` "initialize j loop"

        --     x = 0;
        --     for (j = 0; j < blockSize; ++j) {
        store xPtr 0 x0
        store jPtr 0 (C.int32 0)
        br pixelLoopX

        pixelLoopX <- block `named` "initialize pixel loop"

        --       color_acc = (0,0,0);
        accR <- alloca AST.i32 Nothing 0
        accG <- alloca AST.i32 Nothing 0
        accB <- alloca AST.i32 Nothing 0
        store accR 0 (C.int32 0)
        store accG 0 (C.int32 0)
        store accB 0 (C.int32 0)
        store kPtr 0 (C.int32 0)
        br subsampleLoop

        --       for (k = 0; k < subsamples; ++k) {
        subsampleLoop <- block `named` "body of pixel loop"

        --           color_acc += user_kernel(x, y, ...);
        do
          xVal <- load xPtr 0
          yVal <- load yPtr 0
          -- FIXME: add in subdivided dx and dy for subsamples
          storeOperand (RealOp xVal) (getBinding args pfX)
          storeOperand (RealOp yVal) (getBinding args pfY)
          (cr0, cg0, cb0) <- runReaderT (compileCode getExtern code) args >>= \case
            ColorOp vr vg vb -> pure (vr, vg, vb)
          cr <- zext cr0 AST.i32
          cg <- zext cg0 AST.i32
          cb <- zext cb0 AST.i32
          do
            tmp1 <- load accR 0
            tmp2 <- add tmp1 cr
            store accR 0 tmp2
          do
            tmp1 <- load accG 0
            tmp2 <- add tmp1 cg
            store accG 0 tmp2
          do
            tmp1 <- load accB 0
            tmp2 <- add tmp1 cb
            store accB 0 tmp2
          do
            tmp1 <- load kPtr 0
            k <- add tmp1 (C.int32 1)
            store kPtr 0 k
            continue <- icmp P.ULT k subsamples
            condBr continue subsampleLoop exitSubsampleLoop

        --       }
        --       output[index++] = color_acc / subsamples;
        exitSubsampleLoop <- block `named` "exit pixel loop"
        do
          index <- load indexPtr 0
          do
            c <- load accR 0
            c' <- udiv c subsamples -- TODO: use log2(subsamples) and a shift?
            outPtr <- gep retPtr [C.int32 0, index]
            c'' <- trunc c' AST.i8
            store outPtr 0 c''
          do
            c <- load accG 0
            c' <- udiv c subsamples -- TODO: use log2(subsamples) and a shift?
            index' <- add index (C.int32 1)
            outPtr <- gep retPtr [C.int32 0, index']
            c'' <- trunc c' AST.i8
            store outPtr 0 c''
          do
            c <- load accB 0
            c' <- udiv c subsamples -- TODO: use log2(subsamples) and a shift?
            index' <- add index (C.int32 2)
            outPtr <- gep retPtr [C.int32 0, index']
            c'' <- trunc c' AST.i8
            store outPtr 0 c''

          index' <- add index (C.int32 3)
          store indexPtr 0 index'
        do -- x += dx
          tmp1 <- load xPtr 0
          tmp2 <- fadd tmp1 dx
          store xPtr 0 tmp2
        do -- j += 1
          tmp1 <- load jPtr 0
          tmp2 <- add tmp1 (C.int32 1)
          store jPtr 0 tmp2
          j <- load jPtr 0
          continue <- icmp P.ULT j blockSize
          condBr continue pixelLoopX exitPixelLoopX

        --    } // end j/x loop
        exitPixelLoopX <- block `named` "exit j loop"
        do -- y -= dy
          tmp1 <- load yPtr 0
          tmp2 <- fsub tmp1 dy
          store yPtr 0 tmp2
        do -- i += 1
          tmp1 <- load iPtr 0
          tmp2 <- add tmp1 (C.int32 1)
          store iPtr 0 tmp2
          i <- load iPtr 0
          continue <- icmp P.ULT i blockSize
          condBr continue pixelLoopY exitPixelLoopY

        -- } // end i/y loop
        exitPixelLoopY <- block `named` "exit i loop"
        retVoid

type RenderEnv' env =
  (  '("[internal argument] #blockWidth", 'IntegerT)
  ': '("[internal argument] #blockHeight", 'IntegerT)
  ': '("[internal argument] #subsamples", 'IntegerT)
  ': env )


compileRenderer' :: forall x y dx dy env
                 . ( KnownEnvironment env
                   , NotPresent "[internal argument] #blockWidth" env
                   , NotPresent "[internal argument] #blockHeight" env
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
                => AST.Name
                -> Proxy x
                -> Proxy y
                -> Proxy dx
                -> Proxy dy
                -> Code '[] env 'ColorT
                -> Either String AST.Module
compileRenderer' name _ _ _ _ code = runExcept $
  buildModuleT "compiled rendering kernel" $ do
    let retParam = (toLLVMPtrType ColorType, NoParameterName)
        params   = toParameterList (envProxy (Proxy @(RenderEnv' env)))
        pfX = bindingEvidence @x @'RealT @env
        pfY = bindingEvidence @y @'RealT @env
        pfdX = bindingEvidence @dx @'RealT @env
        pfdY = bindingEvidence @dy @'RealT @env
    function name (retParam : params) AST.void $ \(retPtr : blockWidthArg : blockHeightArg : subsamplesArg : rawArgs) -> do
      getExtern <- getGetExtern
      traceM ("ok... retPtr = " ++ show retPtr)
      blockWidthPtr <- allocaArg IntegerType blockWidthArg `named` "set up environment"
      blockHeightPtr <- allocaArg IntegerType blockHeightArg `named` "set up environment"
      subsamplesPtr <- allocaArg IntegerType subsamplesArg
      args <- allocaArgs (envProxy (Proxy @env)) rawArgs
      mdo

        x0 <- derefOperand (getBinding args pfX) >>= \case
          RealOp v -> pure v
        y0 <- derefOperand (getBinding args pfY) >>= \case
          RealOp v -> pure v
        dx <- derefOperand (getBinding args pfdX) >>= \case
          RealOp v -> pure v
        dy <- derefOperand (getBinding args pfdY) >>= \case
          RealOp v -> pure v
        blockWidth <- derefOperand blockWidthPtr >>= \case
          IntegerOp v -> pure v
        blockHeight <- derefOperand blockHeightPtr >>= \case
          IntegerOp v -> pure v
        subsamples <- derefOperand subsamplesPtr >>= \case
          IntegerOp v -> pure v
        indexPtr <- alloca AST.i32 Nothing 0 `named` "set up loop indices"
        iPtr <- alloca AST.i32 Nothing 0
        jPtr <- alloca AST.i32 Nothing 0
        kPtr <- alloca AST.i32 Nothing 0
        xPtr <- alloca AST.double Nothing 0
        yPtr <- alloca AST.double Nothing 0

        -- index = 0;
        -- y = 0;
        -- for (i = 0; i < blockSize; ++i) {
        store indexPtr 0 (C.int32 0) `named` "begin i loop"
        store yPtr 0 y0
        store iPtr 0 (C.int32 0)
        br pixelLoopY

        pixelLoopY <- block `named` "begin j loop"

        --     x = 0;
        --     for (j = 0; j < blockSize; ++j) {
        store xPtr 0 x0
        store jPtr 0 (C.int32 0)
        br pixelLoopX

        pixelLoopX <- block `named` "begin k loop"

        --       color_acc = (0,0,0);
        accR <- alloca AST.i32 Nothing 0
        accG <- alloca AST.i32 Nothing 0
        accB <- alloca AST.i32 Nothing 0
        store accR 0 (C.int32 0)
        store accG 0 (C.int32 0)
        store accB 0 (C.int32 0)
        store kPtr 0 (C.int32 0)
        br subsampleLoop

        --       for (k = 0; k < subsamples; ++k) {
        subsampleLoop <- block `named` "k loop body"

        --           color_acc += user_kernel(x, y, ...);
        do
          xVal <- load xPtr 0
          yVal <- load yPtr 0
          -- FIXME: add in subdivided dx and dy for subsamples
          storeOperand (RealOp xVal) (getBinding args pfX)
          storeOperand (RealOp yVal) (getBinding args pfY)
          (cr0, cg0, cb0) <- runReaderT (compileCode getExtern code) args >>= \case
            ColorOp vr vg vb -> pure (vr, vg, vb)
          cr <- zext cr0 AST.i32
          cg <- zext cg0 AST.i32
          cb <- zext cb0 AST.i32
          do
            tmp1 <- load accR 0
            tmp2 <- add tmp1 cr
            store accR 0 tmp2
          do
            tmp1 <- load accG 0
            tmp2 <- add tmp1 cg
            store accG 0 tmp2
          do
            tmp1 <- load accB 0
            tmp2 <- add tmp1 cb
            store accB 0 tmp2
          do
            tmp1 <- load kPtr 0
            k <- add tmp1 (C.int32 1)
            store kPtr 0 k
            continue <- icmp P.ULT k subsamples
            condBr continue subsampleLoop exitSubsampleLoop

        --       }
        --       output[index++] = color_acc / subsamples;
        exitSubsampleLoop <- block `named` "end k loop"
        do
          index <- load indexPtr 0
          do
            c <- load accR 0
            c' <- udiv c subsamples -- TODO: use log2(subsamples) and a shift?
            outPtr <- gep retPtr [C.int32 0, index]
            c'' <- trunc c' AST.i8
            store outPtr 0 c''
          do
            c <- load accG 0
            c' <- udiv c subsamples -- TODO: use log2(subsamples) and a shift?
            index' <- add index (C.int32 1)
            outPtr <- gep retPtr [C.int32 0, index']
            c'' <- trunc c' AST.i8
            store outPtr 0 c''
          do
            c <- load accB 0
            c' <- udiv c subsamples -- TODO: use log2(subsamples) and a shift?
            index' <- add index (C.int32 2)
            outPtr <- gep retPtr [C.int32 0, index']
            c'' <- trunc c' AST.i8
            store outPtr 0 c''

          index' <- add index (C.int32 3)
          store indexPtr 0 index'
        do -- x += dx
          tmp1 <- load xPtr 0
          tmp2 <- fadd tmp1 dx
          store xPtr 0 tmp2
        do -- j += 1
          tmp1 <- load jPtr 0
          tmp2 <- add tmp1 (C.int32 1)
          store jPtr 0 tmp2
          j <- load jPtr 0
          continue <- icmp P.ULT j blockWidth
          condBr continue pixelLoopX exitPixelLoopX

        --    } // end j/x loop
        exitPixelLoopX <- block `named` "end j loop"
        do -- y -= dy
          tmp1 <- load yPtr 0
          tmp2 <- fsub tmp1 dy
          store yPtr 0 tmp2
        do -- i += 1
          tmp1 <- load iPtr 0
          tmp2 <- add tmp1 (C.int32 1)
          store iPtr 0 tmp2
          i <- load iPtr 0
          continue <- icmp P.ULT i blockHeight
          condBr continue pixelLoopY exitPixelLoopY

        -- } // end i/y loop
        exitPixelLoopY <- block `named` "end i loop"
        retVoid


compileCode :: forall m env t
             . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
            => (String -> Operand)
            -> Code '[] env t
            -> ReaderT (Context OperandPtr env) m (Op t)
compileCode getExtern = indexedFold @(CtxOp m) @(Fix (CodeF '[] (Pure1 Value))) @(CodeF '[] (Pure1 Value)) $ \case

  Block _ body end -> sequence_ body >> end

  Pure _ v -> value_ getExtern v

  NoOp -> pure VoidOp

  Set pf _ t v -> do
    x <- value_ getExtern v
    ctx <- ask
    storeOperand x (withKnownType t (getBinding ctx pf))
    pure VoidOp

  SetBind pf _ t body -> do
    x <- body
    ctx <- ask
    storeOperand x (withKnownType t (getBinding ctx pf))
    pure VoidOp

  Let pf name t v _ body -> do
    x <- value_ getExtern v
    ptr <- allocaOp t
    storeOperand x ptr
    recallIsAbsent (absentInTail pf) $ do
      ctx <- Bind name t ptr <$> ask
      lift (runReaderT body ctx)

  LetBind pf name t val _ body -> do
    x <- val
    ptr <- allocaOp t
    storeOperand x ptr
    recallIsAbsent (absentInTail pf) $ do
      ctx <- Bind name t ptr <$> ask
      lift (runReaderT body ctx)

  IfThenElse VoidType cond yes no -> mdo
    c <- value_ getExtern cond >>= detypeOperand BooleanType
    condBr c yesLabel noLabel

    yesLabel <- block
    void yes
    br nextLabel

    noLabel <- block
    void no
    br nextLabel

    nextLabel <- block
    pure VoidOp

  IfThenElse t cond yes no -> mdo
    ptr <- allocaOp t
    c <- value_ getExtern cond >>= detypeOperand BooleanType
    condBr c yesLabel noLabel

    yesLabel <- block
    yesResult <- yes
    storeOperand yesResult ptr
    br nextLabel

    noLabel <- block
    noResult <- no
    storeOperand noResult ptr
    br nextLabel

    nextLabel <- block
    derefOperand ptr

  DoWhile body -> mdo
    br entry

    entry <- block
    test <- body >>= detypeOperand BooleanType
    condBr test entry exit

    exit <- block
    pure VoidOp

  _ -> error "TODO: unhandled Code constructor"

data CtxOp :: (* -> *) -> (Environment, FSType) -> Exp *
type instance Eval (CtxOp m et) =
  ReaderT (Context OperandPtr (Env et)) m (Op (Ty et))

allocaArgs :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
               => EnvironmentProxy env
               -> [Operand]
               -> m (Context OperandPtr env)
allocaArgs EmptyEnvProxy [] = pure EmptyContext
allocaArgs (BindingProxy name ty env) (op:ops) =
  Bind name ty <$> allocaArg ty op
               <*> allocaArgs env ops
allocaArgs _ _ =
  throwError "internal error: mismatched environment/args counts"

allocaArg :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
         => TypeProxy t
         -> Operand
         -> m (PtrOp t)
allocaArg t op = do
  ptr <- allocaOp t
  x <- typedOperand t op
  storeOperand x ptr
  pure ptr
