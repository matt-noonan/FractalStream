{-# language RecursiveDo, OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
{-# options_ghc -Wno-incomplete-uni-patterns #-}
module Backend.LLVM.Code
  ( --compile
--  , compileRenderer
   compileRenderer'
  ) where

import FractalStream.Prelude

import Actor.Viewer

import Backend.LLVM.Operand
import Backend.LLVM.Value

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Monad
import qualified LLVM.IRBuilder.Constant as C
import qualified LLVM.AST.IntegerPredicate as P

import Control.Monad.Fix

import qualified Data.Map.Strict as Map
import Unsafe.Coerce (unsafeCoerce)

import Language.Type
import Language.Code
import Data.Indexed.Functor

toParameterList :: EnvironmentProxy env -> [(AST.Type, ParameterName)]
toParameterList = \case
  EmptyEnvProxy -> []
  BindingProxy name t env ->
    (toLLVMType t, ParameterName (fromString (symbolVal name))) : toParameterList env

{-
compile :: forall env output t
         . (KnownEnvironment env, KnownSymbol output, KnownType t
           , Required output env ~ t)
        => Proxy output
        -> TypeProxy t
        -> Code env
        -> Either String AST.Module
compile _ outputTy code = runExcept $
  buildModuleT "compiled code" $ do
    let retParam = (toLLVMPtrType outputTy, NoParameterName)
        params   = toParameterList (envProxy (Proxy @env))
    function "kernel" (retParam : params) AST.void $ \(retArg : rawArgs) -> do
      getExtern <- getGetExtern
      traceM ("making typedOperandPtr, return type is " ++ showType outputTy)
      traceM ("  retArg = " ++ show retArg)
      retPtr <- typedOperandPtr outputTy retArg
      traceM ("ok, continuing... retPtr = " ++ show retPtr)
      args <- allocaArgs (envProxy (Proxy @env)) rawArgs
      runReaderT (compileCode getExtern code) args
      rv <- derefOperand (getBinding args (bindingEvidence @output @t @env))
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
                   , Required "color" env ~ 'ColorT
                   , NotPresent "color" (env `Without` "color")
                   )
                => Code env
                -> Either String AST.Module
compileRenderer code = runExcept $
  buildModuleT "compiled rendering kernel" $ do
    let retParam = (toLLVMPtrType ColorType, NoParameterName)
        params   = toParameterList (envProxy (Proxy @(RenderEnv env)))
        pfX = bindingEvidence @"x" @'RealT @env
        pfY = bindingEvidence @"y" @'RealT @env
        pfOutput = bindingEvidence @"color" @'ColorT @env
    function "kernel" (retParam : params) AST.void $ \(retPtr : blockSizeArg : subsamplesArg : dzArg : rawArgs) -> do
      getExtern <- getGetExtern
      traceM ("ok... retPtr = " ++ show retPtr)
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

          -- Allocate a color pointer, pass in to compileCode,
          -- read components out into cr0, cg0, cb0
          runReaderT (compileCode getExtern code) args
          (cr0, cg0, cb0) <- case getBinding args pfOutput of
            PtrOp (ColorOp outputR outputG outputB) ->
              (,,) <$> load outputR 0 <*> load outputG 0 <*> load outputB 0

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
-}

type RenderEnv' env =
  (  '(InternalBlockWidth,  'IntegerT)
  ': '(InternalBlockHeight, 'IntegerT)
  ': '(InternalSubsamples,  'IntegerT)
  ': ViewerEnv env )

type InternalBlockWidth  = "[llvm internal argument] #blockWidth"
type InternalBlockHeight = "[llvm internal argument] #blockHeight"
type InternalSubsamples  = "[llvm internal argument] #subsamples"

assertAbsent :: forall name env a. (KnownSymbol name)
             => Proxy name
             -> EnvironmentProxy env
             -> (NotPresent name env => Except String a)
             -> Except String a
assertAbsent name env action = case lookupEnv' name env of
    Absent' pf -> recallIsAbsent pf action
    _ -> throwError ("INTERNAL ERROR: llvm-internal argument `" ++ symbolVal name ++ "` re-defined.")

-- | Count the number of bindings in an environment proxy.
envLength :: EnvironmentProxy env -> Int
envLength = \case
  EmptyEnvProxy      -> 0
  BindingProxy _ _ e -> 1 + envLength e

-- | Generate one @ptr i8@ LLVM parameter per prep output variable.
toPrepParamList :: EnvironmentProxy env -> [(AST.Type, ParameterName)]
toPrepParamList = \case
  EmptyEnvProxy -> []
  BindingProxy name _t env ->
    ( AST.ptr AST.i8
    , ParameterName (fromString (symbolVal name ++ "_prep_array"))
    ) : toPrepParamList env

-- | An existential wrapping a typed operand pointer, for name-based lookup.
data SomePtrOp where
  SomePtrOp :: TypeProxy t -> PtrOp t -> SomePtrOp

-- | Build a map from variable name to its LLVM alloca pointer.
contextToArgMap :: Context OperandPtr env -> Map.Map String SomePtrOp
contextToArgMap = \case
  EmptyContext             -> Map.empty
  Bind name ty ptrOp rest -> Map.insert (symbolVal name) (SomePtrOp ty ptrOp) (contextToArgMap rest)

-- | Load a value of type @t@ from a flat byte array (@ptr i8@) at the given
-- pixel index.  Byte stride per pixel: Bool=1, Int=4, Real=8, Complex=16,
-- Color=3.
loadFromPrepArrayAt
  :: (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
  => TypeProxy t
  -> Operand    -- ^ base @ptr i8@ of the prep array
  -> Operand    -- ^ flat pixel index (i32)
  -> m (Op t)
loadFromPrepArrayAt ty arrayPtr pixelIdx = case ty of
  BooleanType -> do
    ptr <- gep arrayPtr [pixelIdx]
    val <- load ptr 0
    bit <- trunc val AST.i1
    pure (BooleanOp bit)
  IntegerType -> do
    byteOff <- mul pixelIdx (C.int32 4)
    ptr     <- gep arrayPtr [byteOff]
    ptr'    <- bitcast ptr (AST.ptr AST.i32)
    val     <- load ptr' 0
    pure (IntegerOp val)
  RealType -> do
    byteOff <- mul pixelIdx (C.int32 8)
    ptr     <- gep arrayPtr [byteOff]
    ptr'    <- bitcast ptr (AST.ptr AST.double)
    val     <- load ptr' 0
    pure (RealOp val)
  ComplexType -> do
    byteOff <- mul pixelIdx (C.int32 16)
    rePtr0  <- gep arrayPtr [byteOff]
    rePtr   <- bitcast rePtr0 (AST.ptr AST.double)
    reVal   <- load rePtr 0
    imOff   <- add byteOff (C.int32 8)
    imPtr0  <- gep arrayPtr [imOff]
    imPtr   <- bitcast imPtr0 (AST.ptr AST.double)
    imVal   <- load imPtr 0
    pure (ComplexOp reVal imVal)
  ColorType -> do
    byteOff <- mul pixelIdx (C.int32 3)
    rPtr    <- gep arrayPtr [byteOff]
    gOff    <- add byteOff (C.int32 1)
    gPtr    <- gep arrayPtr [gOff]
    bOff    <- add byteOff (C.int32 2)
    bPtr    <- gep arrayPtr [bOff]
    r <- load rPtr 0
    g <- load gPtr 0
    b <- load bPtr 0
    pure (ColorOp r g b)
  t -> throwError ("loadFromPrepArrayAt: unsupported type " ++ showType t)

-- | For each variable in @prepOutputEnv@, load its value from the
-- corresponding prep array at the given pixel index and overwrite its slot in
-- the arg context.
overwritePrepOutputs
  :: forall prepOutputEnv m
   . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m)
  => EnvironmentProxy prepOutputEnv
  -> [Operand]                 -- ^ one @ptr i8@ per var in @prepOutputEnv@
  -> Operand                   -- ^ flat pixel index (i32)
  -> Map.Map String SomePtrOp  -- ^ from 'contextToArgMap'
  -> m ()
overwritePrepOutputs EmptyEnvProxy [] _ _ = pure ()
overwritePrepOutputs (BindingProxy name ty env') (ptr:ptrs) pixelIdx argMap = do
  let n = symbolVal name
  case Map.lookup n argMap of
    Nothing -> (throwError ("INTERNAL ERROR: prep output `" ++ n ++ "` not in args") :: m ())
    Just (SomePtrOp ty' ptrOp) -> case sameHaskellType ty ty' of
      Nothing -> (throwError ("INTERNAL ERROR: prep output `" ++ n ++ "` type mismatch") :: m ())
      Just Refl -> do
        op <- loadFromPrepArrayAt ty ptr pixelIdx
        -- sameHaskellType proved t ~ t', so this coercion is safe.
        storeOperand op (unsafeCoerce ptrOp)
  overwritePrepOutputs env' ptrs pixelIdx argMap
overwritePrepOutputs _ _ _ _ =
  throwError "INTERNAL ERROR: mismatched prep output env/ptrs"

compileRenderer' :: forall env prepOutputEnv
                 . KnownEnvironment env
                => EnvironmentProxy prepOutputEnv
                -> AST.Name
                -> Code (ViewerEnv env)
                -> Either String AST.Module
compileRenderer' prepOutputEnv name code = runExcept $
  assertAbsent (Proxy @InternalBlockWidth)  (envProxy (Proxy @env)) $
  assertAbsent (Proxy @InternalBlockHeight) (envProxy (Proxy @env)) $
  assertAbsent (Proxy @InternalSubsamples)  (envProxy (Proxy @env)) $
  assertAbsent (Proxy @InternalX)           (envProxy (Proxy @env)) $
  assertAbsent (Proxy @InternalY)           (envProxy (Proxy @env)) $
  assertAbsent (Proxy @InternalDX)          (envProxy (Proxy @env)) $
  assertAbsent (Proxy @InternalDY)          (envProxy (Proxy @env)) $
  assertAbsent (Proxy @"color")             (envProxy (Proxy @env)) $
  buildModuleT "compiled rendering kernel" $ do
    let retParam     = (toLLVMPtrType ColorType, NoParameterName)
        allEnvParams = toParameterList (envProxy (Proxy @(RenderEnv' env)))
        -- RenderEnv' env = blockWidth ': blockHeight ': subsamples ': ViewerEnv env
        -- First 3 params are the internal render args; the rest are ViewerEnv.
        (internalRenderParams, viewerEnvParams) = splitAt 3 allEnvParams
        arenaParams  = [ (AST.ptr AST.i8, ParameterName "#arenaPtr")
                       , (AST.i32,        ParameterName "#arenaSize") ]
        params       = internalRenderParams ++ arenaParams ++ viewerEnvParams
        prepParams   = toPrepParamList prepOutputEnv
        pfX      = bindingEvidence @InternalX   @'RealT  @(ViewerEnv env)
        pfY      = bindingEvidence @InternalY   @'RealT  @(ViewerEnv env)
        pfdX     = bindingEvidence @InternalDX  @'RealT  @(ViewerEnv env)
        pfdY     = bindingEvidence @InternalDY  @'RealT  @(ViewerEnv env)
        pfOutput = bindingEvidence @"color"     @'ColorT @(ViewerEnv env)
        nViewerEnvArgs = envLength (envProxy (Proxy @(ViewerEnv env)))
    function name (retParam : params ++ prepParams) AST.void $ \allArgs -> do
      let (retPtr : blockWidthArg : blockHeightArg : subsamplesArg
                  : arenaPtrArg : arenaSizeArg : rest) = allArgs
          (rawArgs, prepArrayPtrs) = splitAt nViewerEnvArgs rest
      getExtern <- getGetExtern
      mdo

        _entry <- block `named` "set up environment"
        blockWidthPtr  <- allocaArg IntegerType blockWidthArg
        blockHeightPtr <- allocaArg IntegerType blockHeightArg
        subsamplesPtr  <- allocaArg IntegerType subsamplesArg
        -- Arena setup: bumpAlloca tracks the current allocation position.
        -- arenaEnd is constant = arenaBase + arenaSize.
        bumpAlloca <- alloca (AST.ptr AST.i8) Nothing 0
        store bumpAlloca 0 arenaPtrArg
        arenaEnd <- gep arenaPtrArg [arenaSizeArg]
        let arenaState = ArenaState bumpAlloca arenaEnd
        args <- allocaArgs (envProxy (Proxy @(ViewerEnv env))) rawArgs
        x0 <- derefOperand (getBinding args pfX)  >>= \case RealOp v -> pure v
        y0 <- derefOperand (getBinding args pfY)  >>= \case RealOp v -> pure v
        dx <- derefOperand (getBinding args pfdX) >>= \case RealOp v -> pure v
        dy <- derefOperand (getBinding args pfdY) >>= \case RealOp v -> pure v
        blockWidth  <- derefOperand blockWidthPtr  >>= \case IntegerOp v -> pure v
        blockHeight <- derefOperand blockHeightPtr >>= \case IntegerOp v -> pure v
        subsamples  <- derefOperand subsamplesPtr  >>= \case IntegerOp v -> pure v
        let argMap = contextToArgMap args
        indexPtr      <- alloca AST.i32 Nothing 0 `named` "set up loop indices"
        pixelIndexPtr <- alloca AST.i32 Nothing 0
        iPtr <- alloca AST.i32 Nothing 0
        jPtr <- alloca AST.i32 Nothing 0
        kPtr <- alloca AST.i32 Nothing 0
        xPtr <- alloca AST.double Nothing 0
        yPtr <- alloca AST.double Nothing 0
        br beginLoops

        beginLoops <- block `named` "begin i loop"
        store indexPtr 0 (C.int32 0)
        store pixelIndexPtr 0 (C.int32 0)
        store yPtr 0 y0
        store iPtr 0 (C.int32 0)
        br pixelLoopY

        pixelLoopY <- block `named` "begin j loop"
        store xPtr 0 x0
        store jPtr 0 (C.int32 0)
        br pixelLoopX

        pixelLoopX <- block `named` "begin k loop"
        accR <- alloca AST.i32 Nothing 0
        accG <- alloca AST.i32 Nothing 0
        accB <- alloca AST.i32 Nothing 0
        store accR 0 (C.int32 0)
        store accG 0 (C.int32 0)
        store accB 0 (C.int32 0)
        store kPtr 0 (C.int32 0)

        br subsampleLoop

        subsampleLoop <- block `named` "k loop body"
        do
          xVal <- load xPtr 0
          yVal <- load yPtr 0
          storeOperand (RealOp xVal) (getBinding args pfX)
          storeOperand (RealOp yVal) (getBinding args pfY)

          -- Reset the arena at the start of each subsample so each pixel's
          -- dynamic list allocations are independent.
          store bumpAlloca 0 arenaPtrArg

          -- Load prep output vars from their flat arrays before the kernel.
          pixelIndex <- load pixelIndexPtr 0
          overwritePrepOutputs prepOutputEnv prepArrayPtrs pixelIndex argMap

          runReaderT (compileCode getExtern arenaState code) args
          (cr0, cg0, cb0) <- case getBinding args pfOutput of
            PtrOp (ColorOp outputR outputG outputB) ->
              (,,) <$> load outputR 0 <*> load outputG 0 <*> load outputB 0

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

        exitSubsampleLoop <- block `named` "end k loop"
        do
          index <- load indexPtr 0
          do
            c <- load accR 0
            c' <- udiv c subsamples
            outPtr <- gep retPtr [C.int32 0, index]
            c'' <- trunc c' AST.i8
            store outPtr 0 c''
          do
            c <- load accG 0
            c' <- udiv c subsamples
            index' <- add index (C.int32 1)
            outPtr <- gep retPtr [C.int32 0, index']
            c'' <- trunc c' AST.i8
            store outPtr 0 c''
          do
            c <- load accB 0
            c' <- udiv c subsamples
            index' <- add index (C.int32 2)
            outPtr <- gep retPtr [C.int32 0, index']
            c'' <- trunc c' AST.i8
            store outPtr 0 c''

          index' <- add index (C.int32 3)
          store indexPtr 0 index'
          -- Advance the flat pixel index once per output pixel.
          pixelIndex <- load pixelIndexPtr 0
          pixelIndex' <- add pixelIndex (C.int32 1)
          store pixelIndexPtr 0 pixelIndex'
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

        exitPixelLoopY <- block `named` "end i loop"
        retVoid


compileCode :: forall m env
             . (MonadModuleBuilder m, MonadIRBuilder m, MonadError String m, MonadFix m)
            => (String -> Operand)
            -> ArenaState
            -> Code env
            -> ReaderT (Context OperandPtr env) m ()
compileCode getExtern arena = indexedFold @(OperandPtrContext m) $ \case

  Block body -> sequence_ body

  NoOp -> pure ()

  Set pf _ e -> do
    x <- value_ getExtern arena e
    ctx <- ask
    storeOperand x (withKnownType (typeOfValue e) (getBinding ctx pf))
    pure ()

  Let pf name val body -> do
    x <- value_ getExtern arena val
    let t = typeOfValue val
    ptr <- allocaOp t
    storeOperand x ptr
    recallIsAbsent (absentInTail pf) $ do
      ctx <- Bind name t ptr <$> ask
      lift (runReaderT body ctx)

  IfThenElse cond yes no -> mdo
    c <- value_ getExtern arena cond >>= detypeOperand BooleanType
    condBr c yesLabel noLabel

    yesLabel <- block
    void yes
    br nextLabel

    noLabel <- block
    void no
    br nextLabel

    nextLabel <- block
    pure ()

  DoWhile cond body -> mdo
    br loop

    loop <- block
    void body
    test <- value_ getExtern arena cond >>= detypeOperand BooleanType
    condBr test loop exit

    exit <- block
    pure ()

  -- | Iterate over each element of a list variable.
  ForEach pfList _listName (ListType itemTy) itemName pfNoItem _env _ body ->
    recallIsAbsent pfNoItem $ do
    ctx <- ask
    -- Load the head pointer from the list variable's stack slot.
    headPtr <- lift $ getListOp <$> derefOperand (getBinding ctx pfList)
    -- Allocate a stack slot to track the current node pointer.
    currPtrAlloca <- lift $ alloca (AST.ptr AST.i8) Nothing 0
    lift $ store currPtrAlloca 0 headPtr
    -- Allocate a stack slot for the current element.
    elemSlot <- lift $ allocaOp itemTy
    mdo
      lift $ br loopHead

      loopHead <- block `named` "foreach_head"
      currPtr <- lift $ load currPtrAlloca 0
      isNull  <- lift $ icmp P.EQ currPtr nullI8Ptr
      lift $ condBr isNull loopExit loopBody

      loopBody <- block `named` "foreach_body"
      elem_ <- lift $ loadListElem itemTy currPtr
      lift $ storeOperand elem_ elemSlot
      -- Run the body with the element bound in the context.
      lift $ runReaderT body (Bind itemName itemTy elemSlot ctx)
      nextPtr <- lift $ loadListNext currPtr
      lift $ store currPtrAlloca 0 nextPtr
      lift $ br loopHead

      loopExit <- block `named` "foreach_exit"
      pure ()

  -- | Find the first list element matching a predicate; run action or fallback.
  Lookup pfList _listName (ListType itemTy) itemName pfNoItem _extEnv _env
         predicate action fallback ->
    recallIsAbsent pfNoItem $ do
    ctx <- ask
    -- Load the head pointer from the list variable's stack slot.
    headPtr <- lift $ getListOp <$> derefOperand (getBinding ctx pfList)
    currPtrAlloca <- lift $ alloca (AST.ptr AST.i8) Nothing 0
    lift $ store currPtrAlloca 0 headPtr
    elemSlot <- lift $ allocaOp itemTy
    mdo
      lift $ br loopHead

      loopHead <- block `named` "lookup_head"
      currPtr <- lift $ load currPtrAlloca 0
      isNull  <- lift $ icmp P.EQ currPtr nullI8Ptr
      lift $ condBr isNull notFound loopBody

      loopBody <- block `named` "lookup_body"
      elem_ <- lift $ loadListElem itemTy currPtr
      lift $ storeOperand elem_ elemSlot
      -- Evaluate the predicate in the extended context.
      let extCtx = Bind itemName itemTy elemSlot ctx
      testBit <- lift $ runReaderT (value_ getExtern arena predicate) extCtx
                   >>= detypeOperand BooleanType
      lift $ condBr testBit foundBlock nextIter

      nextIter <- block `named` "lookup_next"
      nextPtr <- lift $ loadListNext currPtr
      lift $ store currPtrAlloca 0 nextPtr
      lift $ br loopHead

      foundBlock <- block `named` "lookup_found"
      lift $ runReaderT action extCtx
      lift $ br exit

      notFound <- block `named` "lookup_not_found"
      lift $ case fallback of
        Nothing  -> pure ()
        Just alt -> runReaderT alt ctx
      lift $ br exit

      exit <- block `named` "lookup_exit"
      pure ()

  _ -> error "unsupported command"


data OperandPtrContext :: (* -> *) -> Environment -> Exp *
type instance Eval (OperandPtrContext m env) =
  ReaderT (Context OperandPtr env) m ()

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
