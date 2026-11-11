-- | Compiling a viewer's code into a WebAssembly kernel module.
--
-- The kernel is @kernel(out, width, height, subsamples, args, x0, y0, dx,
-- dy) -> issues@: it renders a block of pixels into @out@ (RGB bytes),
-- exactly as the LLVM backend's kernel does (the same loops, stepping x
-- by dx and y by -dy, and each pixel the average of its subsamples), and
-- gives the 'Actor.Viewer.ViewerIssues' bits. The configuration's values
-- are read from @args@, laid out as 'argSize' says, one slot per variable
-- in the environment's order.
module Backend.Wasm.Code
  ( compileViewer
  , argSize
  ) where

import FractalStream.Prelude

import Backend.Wasm.Encode hiding (Block)
import Backend.Wasm.Gen
import Backend.Wasm.Value

import Actor.Viewer.Types (ViewerEnv, MissingViewerArgs)
import Language.Code
import Data.Indexed.Functor
import Data.Color (colorToRGB, grey)

data CtxCode :: Environment -> Exp Type
type instance Eval (CtxCode env) = ReaderT (Context WLoc env) Gen ()

code :: Code env -> ReaderT (Context WLoc env) Gen ()
code = indexedFold @CtxCode $ \case
  Block body -> sequence_ body

  NoOp -> pure ()

  Set pf _ e -> do
    x <- value e
    ctx <- ask
    lift (copyInto x (withKnownType (typeOfValue e) (getBinding ctx pf)))

  Let pf name val body -> do
    x <- value val
    x' <- lift (freshLike x)
    lift (copyInto x x')
    recallIsAbsent (absentInTail pf) $ do
      ctx <- Bind name (typeOfValue val) x' <$> ask
      lift (runReaderT body ctx)

  IfThenElse cond yes no -> value cond >>= \case
    LBool c -> do
      (_, yesCode) <- captured yes
      (_, noCode) <- captured no
      lift (emit [localGet c, IfElse yesCode noCode])
    l -> lift (throwError ("internal error: a condition of the wrong shape: " ++ show l))

  -- (the body, then the test: again while it's true)
  DoWhile cond body -> do
    (_, loopCode) <- captured $ do
      body
      value cond >>= \case
        LBool c -> lift (emit [localGet c, brIf 0])
        l -> lift (throwError ("internal error: a condition of the wrong shape: " ++ show l))
    lift (emit [Loop loopCode])

  Lookup{} -> lift (unsupported "list lookups")
  ForEach{} -> lift (unsupported "loops over lists")
  DrawCommand{} -> lift (unsupported "drawing")
  TableAllocate{} -> lift (unsupported "tables")
  TableWrite{} -> lift (unsupported "tables")
  TableWriteSlice{} -> lift (unsupported "tables")
  TableCopySlice{} -> lift (unsupported "tables")
  TableResize{} -> lift (unsupported "tables")
  where
    captured :: ReaderT r Gen a -> ReaderT r Gen (a, [Instr])
    captured m = ReaderT (capture . runReaderT m)

------------------------------------------------------------------------
-- The kernel

-- | How many bytes of the arguments buffer a value of the given type
-- takes: a truth value or integer (as an i32), a real number, or a color
-- (three bytes) take 8; a complex number 16; a pair its parts'. Nothing
-- for the types the wasm backend can't compile yet.
argSize :: TypeProxy t -> Maybe Int
argSize = \case
  BooleanType -> Just 8
  IntegerType -> Just 8
  RealType -> Just 8
  ColorType -> Just 8
  ComplexType -> Just 16
  PairType a b -> (+) <$> argSize a <*> argSize b
  _ -> Nothing

-- | Fresh locals for a variable of the given type.
locFor :: TypeProxy t -> Gen Loc
locFor t = case t of
  BooleanType -> LBool <$> newLocal I32
  IntegerType -> LInt <$> newLocal I32
  RealType -> LReal <$> newLocal F64
  ComplexType -> LComplex <$> newLocal F64 <*> newLocal F64
  ColorType -> LColor <$> newLocal I32 <*> newLocal I32 <*> newLocal I32
  PairType a b -> LPair <$> locFor a <*> locFor b
  _ -> unsupported ("variables of type " ++ showType t)

-- | Load a variable from the arguments buffer (at param 'pArgs') into its
-- locals, from the given offset; gives the offset after it.
loadArg :: Int -> Loc -> Gen Int
loadArg off = \case
  LBool l -> one (i32Load off) l >> pure (off + 8)
  LInt l -> one (i32Load off) l >> pure (off + 8)
  LReal l -> one (f64Load off) l >> pure (off + 8)
  LComplex a b -> one (f64Load off) a >> one (f64Load (off + 8)) b >> pure (off + 16)
  LColor r g b -> do
    one (i32Load8U off) r
    one (i32Load8U (off + 1)) g
    one (i32Load8U (off + 2)) b
    pure (off + 8)
  LPair a b -> loadArg off a >>= (`loadArg` b)
  where one load l = emit [localGet pArgs, load, localSet l]

pOut, pWidth, pHeight, pSubsamples, pArgs, pX0, pY0, pDX, pDY :: Int
pOut = 0; pWidth = 1; pHeight = 2; pSubsamples = 3; pArgs = 4
pX0 = 5; pY0 = 6; pDX = 7; pDY = 8

kernelParams :: [ValType]
kernelParams = [I32, I32, I32, I32, I32, F64, F64, F64, F64]

-- | Compile a viewer's code into a kernel module's bytes, or say why it
-- can't be.
compileViewer :: forall env. (MissingViewerArgs env, KnownEnvironment env)
              => MemoryImport -> Code (ViewerEnv env) -> Either String [Word8]
compileViewer memory body = do
  GenResult{..} <- runGen kernelParams $ do
    ctx <- envToContextM @WLoc (envProxy (Proxy @(ViewerEnv env))) (\_ t -> locFor t)
    case (ctx :: Context WLoc (ViewerEnv env)) of
      Bind _ _ xLoc (Bind _ _ yLoc (Bind _ _ dxLoc (Bind _ _ dyLoc (Bind _ _ colorLoc rest)))) -> do
        -- the configuration's values, once
        _ <- foldM loadArg 0 (fromContext (\_ _ l -> l) rest)
        (xVar, yVar, dxVar, dyVar) <- case (xLoc, yLoc, dxLoc, dyLoc) of
          (LReal a, LReal b, LReal c, LReal d) -> pure (a, b, c, d)
          _ -> throwError "internal error: the viewer's coordinates aren't real numbers"
        (cr, cg, cb) <- case colorLoc of
          LColor r g b -> pure (r, g, b)
          _ -> throwError "internal error: the viewer's color isn't a color"
        emit [localGet pDX, localSet dxVar, localGet pDY, localSet dyVar]

        (_, bodyCode) <- capture (runReaderT (code body) ctx)
        index <- newLocal I32
        i <- newLocal I32
        j <- newLocal I32
        k <- newLocal I32
        accR <- newLocal I32
        accG <- newLocal I32
        accB <- newLocal I32
        x <- newLocal F64
        y <- newLocal F64
        let (greyR, greyG, greyB) = colorToRGB grey
            accumulate acc c = [localGet acc, localGet c, i32Add, localSet acc]
            output off acc =
              [ localGet pOut, localGet index, i32Add
              , localGet acc, localGet pSubsamples, i32DivU, i32Store8 off ]
            subsample =
              [ localGet x, localSet xVar, localGet y, localSet yVar
              , i32Const (fromIntegral greyR), localSet cr
              , i32Const (fromIntegral greyG), localSet cg
              , i32Const (fromIntegral greyB), localSet cb ]
              ++ bodyCode
              ++ accumulate accR cr ++ accumulate accG cg ++ accumulate accB cb
              ++ [ localGet k, i32Const 1, i32Add, localTee k
                 , localGet pSubsamples, i32LtU, brIf 0 ]
            pixel =
              [ i32Const 0, localSet accR, i32Const 0, localSet accG, i32Const 0, localSet accB
              , i32Const 0, localSet k
              , Loop subsample ]
              ++ output 0 accR ++ output 1 accG ++ output 2 accB
              ++ [ localGet index, i32Const 3, i32Add, localSet index
                 , localGet x, localGet pDX, f64Add, localSet x
                 , localGet j, i32Const 1, i32Add, localTee j
                 , localGet pWidth, i32LtU, brIf 0 ]
            row =
              [ localGet pX0, localSet x, i32Const 0, localSet j
              , Loop pixel
              , localGet y, localGet pDY, f64Sub, localSet y
              , localGet i, i32Const 1, i32Add, localTee i
              , localGet pHeight, i32LtU, brIf 0 ]
        emit [ i32Const 0, localSet index, localGet pY0, localSet y, i32Const 0, localSet i
             , Loop row
             -- (no lists or tables yet, so nothing to report)
             , i32Const 0 ]
  pure $ encodeModule WasmModule
    { wmMemory = memory
    , wmImports = [ Import "math" f (replicate n F64) [F64] | (f, n) <- mathImports ]
    , wmName = "kernel"
    , wmParams = kernelParams
    , wmResults = [I32]
    , wmLocals = grLocals
    , wmBody = grBody
    }
