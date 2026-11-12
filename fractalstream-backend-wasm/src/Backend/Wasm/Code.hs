-- | Compiling a viewer's code into a WebAssembly kernel module.
--
-- The kernel is @kernel(out, width, height, subsamples, args, arena,
-- arenaSize, snapshot, misses, x0, y0, dx, dy, px, py, offsetX, offsetY)
-- -> issues@ (@px@, @py@: the pixel size the code sees, as its
-- @pixel_size@, which needn't be the step: see 'Actor.Viewer.vaPixel';
-- @x0@, @y0@ the image's first pixel's point, and @offsetX@, @offsetY@
-- where the block's first pixel is in the image: a pixel's point is the
-- image's origin plus its index times the step, see
-- 'Actor.Viewer.vaOffset') (@snapshot@: a
-- copy of the autocolor groups, or 0; @misses@: where to note the points
-- it would add, for the engine to add: see "Backend.Wasm"): it renders a block of pixels into
-- @out@ (RGB bytes), exactly as the LLVM backend's kernel does (the same
-- loops, stepping x by dx and y by -dy, each pixel the average of its
-- subsamples, and lists made in the arena, afresh for each subsample),
-- and gives the 'Actor.Viewer.ViewerIssues' bits. A subsample that ran
-- out of arena, or indexed a list out of range, is magenta. The
-- configuration's values are read from @args@, laid out as 'argSize'
-- says, one slot per variable in the environment's order (a list's slot
-- holds its address: see "Backend.Wasm.List").
--
-- A tool's event handler compiles to @tool(args, arena, arenaSize,
-- result, snapshot, misses) -> status@ instead ('compileTool'), with an
-- autocolor snapshot and misses list as a kernel has: it runs its code once, with
-- its variables read from @args@ as a viewer's are, and written back
-- there when it's done (so the engine can see what it changed). Its draw
-- commands go into the arena too, from the top down (lists are made from
-- the bottom up), each a 'drawRecordSize'-byte record (see 'drawRecord');
-- it writes where the lists ended and where the draw commands began at
-- @result@ and @result + 4@, and gives the issues as a kernel does (bit 0:
-- it ran out of arena; bit 1: a bad index).
module Backend.Wasm.Code
  ( compileViewer
  , compileTool
  , argSize
  , drawRecordSize
  ) where

import FractalStream.Prelude

import Backend.Wasm.Encode hiding (Block)
import Backend.Wasm.Gen
import Backend.Wasm.Value
import Backend.Wasm.List
import Backend.Wasm.Table (freezeTable, tableImports, tableAllocate, tableWrite, tableWritePoint, tableWriteSlice, tableCopySlice, tableResize)

import Actor.Viewer.Types (ViewerEnv, MissingViewerArgs)
import Language.Code
import Language.Draw (Draw_(..))
import Data.Indexed.Functor
import Data.Color (colorToRGB, grey)
import Language.Table (elementSize)

data CtxCode :: Environment -> Exp Type
type instance Eval (CtxCode env) = ReaderT (Context WLoc env) Gen ()

code :: Code env -> ReaderT (Context WLoc env) Gen ()
code = indexedFold @CtxCode $ \case
  Block body -> sequence_ body

  NoOp -> pure ()

  Set pf _ e -> do
    x <- value e
    ctx <- ask
    -- (a table another variable now holds too is shared: see "Backend.Wasm.Table")
    lift (freezeTable x)
    lift (copyInto x (withKnownType (typeOfValue e) (getBinding ctx pf)))

  Let pf name val body -> do
    x <- value val
    lift (freezeTable x)
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

  ForEach pfList _ (ListType itemTy) itemName pfNoItem _ _ body -> recallIsAbsent pfNoItem $ do
    ctx <- ask
    case withKnownType (ListType itemTy) (getBinding ctx pfList) of
      LList h -> lift $ walkList h $ \curr _ -> do
        e <- loadElem itemTy curr 8
        runReaderT body (Bind itemName itemTy e ctx)
      l -> lift (throwError ("internal error: a loop over a non-list: " ++ show l))

  -- the first element for which the predicate holds: the action with it,
  -- or the fallback (if any) when there's none
  Lookup pfList _ (ListType itemTy) itemName pfNoItem _ _ predicate action fallback ->
    recallIsAbsent pfNoItem $ do
    ctx <- ask
    case withKnownType (ListType itemTy) (getBinding ctx pfList) of
      LList h -> do
        found <- lift (newLocal I32)
        item <- lift (zeroOf itemTy)
        lift (emit [i32Const 0, localSet found])
        lift $ walkList h $ \curr stop -> do
          e <- loadElem itemTy curr 8
          runReaderT (value predicate) (Bind itemName itemTy e ctx) >>= \case
            LBool b -> do
              (_, take') <- capture (copyInto e item)
              emit [ localGet b
                   , IfElse (take' ++ [i32Const 1, localSet found, i32Const 1, localSet stop]) [] ]
            l -> throwError ("internal error: a test that isn't true or false: " ++ show l)
        (_, yes) <- lift (capture (runReaderT action (Bind itemName itemTy item ctx)))
        (_, no) <- lift (capture (maybe (pure ()) (`runReaderT` ctx) fallback))
        lift (emit [localGet found, IfElse yes no])
      l -> lift (throwError ("internal error: a lookup in a non-list: " ++ show l))
  DrawCommand d -> do
    lift drawingAllowed
    case d of
      Clear _ -> lift (drawRecord 0 [])
      SetStroke _ c -> value c >>= lift . colorRecord 1
      SetFill _ c -> value c >>= lift . colorRecord 2
      DrawPoint _ p -> do
        (x, y) <- point p
        lift (drawRecord 3 [f64At 8 x, f64At 16 y])
      DrawLine _ a b -> do
        (x1, y1) <- point a
        (x2, y2) <- point b
        lift (drawRecord 4 [f64At 8 x1, f64At 16 y1, f64At 24 x2, f64At 32 y2])
      DrawCircle _ filled r p -> do
        rad <- value r >>= \case { LReal l -> pure l; l -> lift (throwError ("internal error: a radius of the wrong shape: " ++ show l)) }
        (x, y) <- point p
        lift (drawRecord 5 [flagAt filled, f64At 8 rad, f64At 16 x, f64At 24 y])
      DrawRect _ filled a b -> do
        (x1, y1) <- point a
        (x2, y2) <- point b
        lift (drawRecord 6 [flagAt filled, f64At 8 x1, f64At 16 y1, f64At 24 x2, f64At 32 y2])
      Write _ t p -> do
        txt <- value t >>= \case { LText l -> pure l; l -> lift (throwError ("internal error: a text of the wrong shape: " ++ show l)) }
        (x, y) <- point p
        lift (drawRecord 7 [\rec -> [localGet rec, localGet txt, i32Store 4], f64At 8 x, f64At 16 y])
  -- (only a tool's handler can change a table)
  TableAllocate pf _ n ty msizes mz1 mz2 mfill -> do
    slot <- withKnownType ty (tableSlot (TableType n ty) pf)
    sizes <- mapM (\v -> value v >>= lift . intLoc) msizes
    z1 <- value mz1 >>= lift . complexLoc
    z2 <- value mz2 >>= lift . complexLoc
    fill <- traverse value mfill
    lift (tableAllocate ty slot sizes z1 z2 fill)
  TableWrite pf _ n ty cell mv -> do
    slot <- withKnownType ty (tableSlot (TableType n ty) pf)
    v <- value mv
    case cell of
      CellAt mixs -> do
        ixs <- mapM (\i -> value i >>= lift . intLoc) mixs
        lift (tableWrite ty slot ixs v)
      CellNear mz mrest -> do
        z <- value mz >>= lift . complexLoc
        rest <- mapM (\i -> value i >>= lift . intLoc) mrest
        lift (tableWritePoint ty slot z rest v)
  TableWriteSlice pf _ n ty at mxs -> do
    slot <- withKnownType ty (tableSlot (TableType n ty) pf)
    (mz, ixs) <- sliceAt at
    xs <- value mxs
    lift (tableWriteSlice ty slot mz ixs xs)
  TableCopySlice pf _ n ty at _ msrc srcAt -> do
    slot <- withKnownType ty (tableSlot (TableType n ty) pf)
    dst <- sliceAt at
    src <- value msrc >>= \case { LTable t -> pure t; l -> lift (throwError ("internal error: copying from a non-table: " ++ show l)) }
    from <- sliceAt srcAt
    lift (tableCopySlice ty slot dst src from)
  TableResize pf _ n ty msizes mfill -> do
    slot <- withKnownType ty (tableSlot (TableType n ty) pf)
    sizes <- mapM (\v -> value v >>= lift . intLoc) msizes
    fill <- traverse value mfill
    lift (tableResize ty slot sizes fill)
  where
    captured :: ReaderT r Gen a -> ReaderT r Gen (a, [Instr])
    captured m = ReaderT (capture . runReaderT m)
    -- the local holding a table variable's descriptor
    tableSlot :: forall env' name t. KnownSymbol name => TypeProxy t -> NameIsPresent name t env' -> ReaderT (Context WLoc env') Gen Int
    tableSlot t pf = do
      ctx <- ask
      case withKnownType t (getBinding ctx pf) of
        LTable l -> pure l
        l -> lift (throwError ("internal error: a table variable of the wrong shape: " ++ show l))
    sliceAt :: forall env'. TableSliceAt env' -> ReaderT (Context WLoc env') Gen (Maybe (Int, Int), [Maybe Int])
    sliceAt (SliceAt mz mixs) = do
      z <- traverse (\v -> value v >>= lift . complexLoc) mz
      ixs <- mapM (traverse (\i -> value i >>= lift . intLoc)) mixs
      pure (z, ixs)
    intLoc = \case { LInt i -> pure i; l -> throwError ("internal error: an index of the wrong shape: " ++ show l) }
    complexLoc = \case { LComplex x y -> pure (x, y); l -> throwError ("internal error: a point of the wrong shape: " ++ show l) }
    point :: forall env'. Value '(env', 'Pair 'RealT 'RealT) -> ReaderT (Context WLoc env') Gen (Int, Int)
    point p = value p >>= \case
      LPair (LReal x) (LReal y) -> pure (x, y)
      l -> lift (throwError ("internal error: a point of the wrong shape: " ++ show l))
    colorRecord tag = \case
      LColor r g b -> drawRecord tag [byteAt 4 r, byteAt 5 g, byteAt 6 b]
      l -> throwError ("internal error: a color of the wrong shape: " ++ show l)
    f64At off l rec = [localGet rec, localGet l, f64Store off]
    byteAt off l rec = [localGet rec, localGet l, i32Store8 off]
    flagAt filled rec = [localGet rec, i32Const (if filled then 1 else 0), i32Store 4]

------------------------------------------------------------------------
-- Draw commands

-- | How many bytes a draw command takes in the arena.
drawRecordSize :: Int
drawRecordSize = 40

-- | Put a draw command into the arena, below the ones before it: its tag
-- (an i32 at 0: 0 erase, 1 the pen's color, 2 the fill's, 3 a point, 4 a
-- line, 5 a circle, 6 a rectangle, 7 text), then its fields, written by the given
-- instructions (given the record's address): a color's bytes at 4, 5 and
-- 6; whether a shape's filled, an i32 at 4; its numbers, f64s from 8 on
-- (a point's x and y; a line's or rectangle's two points; a circle's
-- radius, then its center; text's place), and text's address, an i32 at
-- 4 (see "Backend.Wasm.Text"). If there's no room, the overflow flag is set
-- instead.
drawRecord :: Int -> [Int -> [Instr]] -> Gen ()
drawRecord tag fields = do
  Arena{..} <- getArena
  rec <- newLocal I32
  emit [ localGet arEnd, i32Const (fromIntegral drawRecordSize), i32Sub, localTee rec
       , localGet arBump, i32LtU
       , IfElse [ i32Const 1, localSet arOverflow ]
                ([ localGet rec, localSet arEnd
                 , localGet rec, i32Const (fromIntegral tag), i32Store 0 ]
                 ++ concatMap ($ rec) fields) ]

------------------------------------------------------------------------
-- The kernel

-- | How many bytes of the arguments buffer a value of the given type
-- takes: a truth value or integer (as an i32), a real number, or a color
-- (three bytes), a list, or a text (an address) take 8; a complex number
-- 16; a pair its parts'. Nothing
-- for the types the wasm backend can't compile yet.
argSize :: TypeProxy t -> Maybe Int
argSize = \case
  BooleanType -> Just 8
  IntegerType -> Just 8
  RealType -> Just 8
  ColorType -> Just 8
  ComplexType -> Just 16
  PairType a b -> (+) <$> argSize a <*> argSize b
  ListType t -> elemSize t >> Just 8
  TextType -> Just 8
  TableType _ t -> elementSize t >> Just 8
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
  ListType i | Just _ <- elemSize i -> LList <$> newLocal I32
  TextType -> LText <$> newLocal I32
  TableType _ i | Just _ <- elementSize i -> LTable <$> newLocal I32
  _ -> unsupported ("variables of type " ++ showType t)

-- | Load a variable from the arguments buffer (at the given param) into
-- its locals, from the given offset; gives the offset after it.
loadArg :: Int -> Int -> Loc -> Gen Int
loadArg argsParam off = \case
  LBool l -> one (i32Load off) l >> pure (off + 8)
  LInt l -> one (i32Load off) l >> pure (off + 8)
  LReal l -> one (f64Load off) l >> pure (off + 8)
  LComplex a b -> one (f64Load off) a >> one (f64Load (off + 8)) b >> pure (off + 16)
  LColor r g b -> do
    one (i32Load8U off) r
    one (i32Load8U (off + 1)) g
    one (i32Load8U (off + 2)) b
    pure (off + 8)
  LPair a b -> loadArg argsParam off a >>= \off' -> loadArg argsParam off' b
  LList l -> one (i32Load off) l >> pure (off + 8)
  LText l -> one (i32Load off) l >> pure (off + 8)
  LTable l -> one (i32Load off) l >> pure (off + 8)
  where one load l = emit [localGet argsParam, load, localSet l]

-- | Store a variable's locals into the arguments buffer (at the given
-- param), as 'loadArg' reads them; gives the offset after it.
storeArg :: Int -> Int -> Loc -> Gen Int
storeArg argsParam off = \case
  LBool l -> one (i32Store off) l >> pure (off + 8)
  LInt l -> one (i32Store off) l >> pure (off + 8)
  LReal l -> one (f64Store off) l >> pure (off + 8)
  LComplex a b -> one (f64Store off) a >> one (f64Store (off + 8)) b >> pure (off + 16)
  LColor r g b -> do
    one (i32Store8 off) r
    one (i32Store8 (off + 1)) g
    one (i32Store8 (off + 2)) b
    pure (off + 8)
  LPair a b -> storeArg argsParam off a >>= \off' -> storeArg argsParam off' b
  LList l -> one (i32Store off) l >> pure (off + 8)
  LText l -> one (i32Store off) l >> pure (off + 8)
  LTable l -> one (i32Store off) l >> pure (off + 8)
  where one store l = emit [localGet argsParam, localGet l, store]

pOut, pWidth, pHeight, pSubsamples, pArgs, pArena, pArenaSize, pSnapshot, pMisses,
  pX0, pY0, pDX, pDY, pPX, pPY, pOffX, pOffY :: Int
pOut = 0; pWidth = 1; pHeight = 2; pSubsamples = 3; pArgs = 4
pArena = 5; pArenaSize = 6; pSnapshot = 7; pMisses = 8
pX0 = 9; pY0 = 10; pDX = 11; pDY = 12; pPX = 13; pPY = 14; pOffX = 15; pOffY = 16

kernelParams :: [ValType]
kernelParams = [I32, I32, I32, I32, I32, I32, I32, I32, I32, F64, F64, F64, F64, F64, F64, I32, I32]

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
        _ <- foldM (loadArg pArgs) 0 (fromContext (\_ _ l -> l) rest)
        (xVar, yVar, dxVar, dyVar) <- case (xLoc, yLoc, dxLoc, dyLoc) of
          (LReal a, LReal b, LReal c, LReal d) -> pure (a, b, c, d)
          _ -> throwError "internal error: the viewer's coordinates aren't real numbers"
        (cr, cg, cb) <- case colorLoc of
          LColor r g b -> pure (r, g, b)
          _ -> throwError "internal error: the viewer's color isn't a color"
        -- (the code's pixel size; the pixels are pDX and pDY apart)
        emit [localGet pPX, localSet dxVar, localGet pPY, localSet dyVar]

        arena@Arena{..} <- Arena <$> newLocal I32 <*> newLocal I32 <*> newLocal I32 <*> newLocal I32
        setArena arena
        setAutocolor (AutocolorParams pSnapshot pMisses)
        anyOverflow <- newLocal I32
        anyBadIndex <- newLocal I32
        emit [i32Const 0, localSet anyOverflow, i32Const 0, localSet anyBadIndex]

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
              , i32Const (fromIntegral greyB), localSet cb
              -- a fresh arena
              , localGet pArena, localSet arBump
              , localGet pArena, localGet pArenaSize, i32Add, localSet arEnd
              , i32Const 0, localSet arOverflow, i32Const 0, localSet arBadIndex ]
              ++ bodyCode
              ++ [ localGet anyOverflow, localGet arOverflow, i32Or, localSet anyOverflow
                 , localGet anyBadIndex, localGet arBadIndex, i32Or, localSet anyBadIndex
                 -- magenta if it ran out of room or indexed out of range
                 , localGet arOverflow, localGet arBadIndex, i32Or
                 , IfElse [ i32Const 255, localSet cr, i32Const 0, localSet cg
                          , i32Const 255, localSet cb ] [] ]
              ++ accumulate accR cr ++ accumulate accG cg ++ accumulate accB cb
              ++ [ localGet k, i32Const 1, i32Add, localTee k
                 , localGet pSubsamples, i32LtU, brIf 0 ]
            pixel =
              [ i32Const 0, localSet accR, i32Const 0, localSet accG, i32Const 0, localSet accB
              , i32Const 0, localSet k
              , Loop subsample ]
              ++ output 0 accR ++ output 1 accG ++ output 2 accB
              ++ [ localGet index, i32Const 3, i32Add, localSet index
                 , localGet j, i32Const 1, i32Add, localTee j
                 , localGet pWidth, i32LtU, brIf 0 ]
            -- (each pixel's point: the image's origin plus its index in
            -- the image times the step, not stepped to)
            pointOf = [ localGet pX0, localGet pOffX, localGet j, i32Add, f64ConvertI32S
                      , localGet pDX, f64Mul, f64Add, localSet x ]
            row =
              [ localGet pY0, localGet pOffY, localGet i, i32Add, f64ConvertI32S
              , localGet pDY, f64Mul, f64Sub, localSet y
              , i32Const 0, localSet j
              , Loop (pointOf ++ pixel)
              , localGet i, i32Const 1, i32Add, localTee i
              , localGet pHeight, i32LtU, brIf 0 ]
        emit [ i32Const 0, localSet index, i32Const 0, localSet i
             , Loop row
             -- the issues: bit 0 for running out of arena, bit 1 for a bad index
             , localGet anyOverflow, localGet anyBadIndex, i32Const 1, i32Shl, i32Or ]
  pure $ encodeModule WasmModule
    { wmMemory = memory
    , wmImports = [ Import "math" f (replicate n F64) [F64] | (f, n) <- mathImports ]
                  ++ [ Import "autocolor" "lookup" [I32, I32, I32, F64, F64, I32] [I32] ]
    , wmName = "kernel"
    , wmParams = kernelParams
    , wmResults = [I32]
    , wmLocals = grLocals
    , wmBody = grBody
    }

------------------------------------------------------------------------
-- Tools

tArgs, tArena, tArenaSize, tResult, tSnapshot, tMisses :: Int
tArgs = 0; tArena = 1; tArenaSize = 2; tResult = 3; tSnapshot = 4; tMisses = 5

-- | Compile a tool's event handler (its code, in its environment) into a
-- module's bytes, exporting @tool@ (see above), or say why it can't be.
compileTool :: EnvironmentProxy env -> Code env -> Either String [Word8]
compileTool envp body = withEnvironment envp $ do
  GenResult{..} <- runGen [I32, I32, I32, I32, I32, I32] $ do
    ctx <- envToContextM @WLoc envp (\_ t -> locFor t)
    let vars = fromContext (\_ _ l -> l) ctx
    _ <- foldM (loadArg tArgs) 0 vars
    arena@Arena{..} <- Arena <$> newLocal I32 <*> newLocal I32 <*> newLocal I32 <*> newLocal I32
    setArena arena
    setAutocolor (AutocolorParams tSnapshot tMisses)
    allowDrawing
    emit [ localGet tArena, localSet arBump
         , localGet tArena, localGet tArenaSize, i32Add, localSet arEnd
         , i32Const 0, localSet arOverflow, i32Const 0, localSet arBadIndex ]
    runReaderT (code body) ctx
    -- (what it changed, for the engine)
    _ <- foldM (storeArg tArgs) 0 vars
    emit [ localGet tResult, localGet arBump, i32Store 0
         , localGet tResult, localGet arEnd, i32Store 4
         , localGet arOverflow, localGet arBadIndex, i32Const 1, i32Shl, i32Or ]
  pure $ encodeModule WasmModule
    { wmMemory = UnsharedMemory
    , wmImports = [ Import "math" f (replicate n F64) [F64] | (f, n) <- mathImports ]
                  ++ [ Import "autocolor" "lookup" [I32, I32, I32, F64, F64, I32] [I32] ]
                  ++ tableImports
    , wmName = "tool"
    , wmParams = [I32, I32, I32, I32, I32, I32]
    , wmResults = [I32]
    , wmLocals = grLocals
    , wmBody = grBody
    }
