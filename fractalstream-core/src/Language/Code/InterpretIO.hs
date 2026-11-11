{-# language AllowAmbiguousTypes #-}
module Language.Code.InterpretIO
  ( interpretToIO
  , interpretToIOWithLastValues
  , eval
  , update
  , IORefTypeOfBinding
  , ScalarIORefM
  ) where

import FractalStream.Prelude

import Language.Value
import Language.Code
import Language.Value.Evaluator
import Language.Draw

import Data.Indexed.Functor

import Data.IORef
import Language.Table
import qualified Data.Map as Map

data ScalarIORefM :: Environment -> Exp Type
type instance Eval (ScalarIORefM env) =
  StateT (Context IORefTypeOfBinding env) IO ()

data ScalarIORefM' :: Environment -> Exp Type
type instance Eval (ScalarIORefM' env) =
  StateT (Map String SomeHaskellType, Context IORefTypeOfBinding env) IO ()

data IORefTypeOfBinding :: Symbol -> FSType -> Exp Type
type instance Eval (IORefTypeOfBinding name t) = IORef (HaskellType t)

-- | Evaluate a value in the current environment.
-- Forces the result eagerly to prevent lazy thunk chains from building up
-- over loop iterations: each iteration's IORefs would otherwise accumulate
-- thunks referencing the full context snapshot from that iteration,
-- retaining O(iterations × context_size) memory.
eval :: forall t env
      . Value '(env, t)
     -> StateT (Context IORefTypeOfBinding env) IO (HaskellType t)
eval v = do
  ctxRef <- get
  ctx <- mapContextM (\_ _ -> lift . readIORef) ctxRef
  let result = evaluate v ctx
  lift $ case typeOfValue v of
    ComplexType -> case result of
      re :+ im -> re `seq` im `seq` return result
    _ -> result `seq` return result


-- | Update a variable in the current environment
update :: forall name t env
        . KnownSymbol name
       => NameIsPresent name t env
       -> Proxy name
       -> TypeProxy t
       -> HaskellType t
       -> StateT (Context IORefTypeOfBinding env) IO ()
update pf _name ty v = withKnownType ty $ do
  ctx <- get
  let valueRef = getBinding ctx pf
  lift (writeIORef valueRef v)

zoom :: Monad m => StateT q m t -> StateT (p, q) m t
zoom (StateT f) = StateT (\(p, x) -> f x >>= \(t, q) -> pure (t, (p, q)))

interpretToIO :: forall env
               . DrawHandler ScalarIORefM
              -> Code env
              -> StateT (Context IORefTypeOfBinding env) IO ()
interpretToIO draw code =
  let StateT f = interpretToIOWithLastValues draw code
  in StateT $ \ctx -> second snd <$> f (Map.empty, ctx)

interpretToIOWithLastValues :: forall env
   . DrawHandler ScalarIORefM
  -> Code env
  -> StateT (Map String SomeHaskellType, Context IORefTypeOfBinding env) IO ()
interpretToIOWithLastValues draw = indexedFold @ScalarIORefM' phi
  where
    phi :: forall env'
         . CodeF ScalarIORefM' env'
        -> StateT (Map String SomeHaskellType, Context IORefTypeOfBinding env') IO ()

    phi = \case
      Let pf name ve body -> recallIsAbsent (absentInTail pf) $ do
        (m, ctxRef) <- get
        value <- zoom $ eval ve
        lift (freezeIfTable (typeOfValue ve) value)
        valueRef <- lift (newIORef value)
        let ctxRef' = Bind name (typeOfValue ve) valueRef ctxRef
        let m' = Map.insert (symbolVal name) (SomeHaskellType (typeOfValue ve) value) m
        m'' <- fst <$> lift (execStateT body (m', ctxRef'))
        modify (first $ const m'')

      Set pf name ve -> do
        value <- zoom $ eval ve
        lift (freezeIfTable (typeOfValue ve) value)
        modify (first $ Map.insert (symbolVal name) (SomeHaskellType (typeOfValue ve) value))
        zoom $ update pf name (typeOfValue ve) value

      Block stmts -> sequence_ stmts

      TableAllocate pf name d ty szs p q mfill -> withKnownType ty $ withKnownNat d $ do
        sizes <- mapM (zoom . eval) szs
        p' <- zoom (eval p)
        q' <- zoom (eval q)
        fill <- traverse (zoom . eval) mfill
        when (any (< 0) sizes) $
          error ("A table's size can't be negative, but `" ++ symbolVal name ++ "` was given "
                 ++ show sizes ++ ".")
        lift (allocateTable ty (map fromIntegral sizes) p' q') >>= \case
          Nothing -> error "internal error: not a table element type"
          Just t -> do
            lift (forM_ fill (fillTable ty t))
            zoom (update pf name (TableType d ty) t)

      TableWrite pf name d ty cell ve -> withKnownType ty $ withKnownNat d $ do
        v <- zoom (eval ve)
        t0 <- zoom (eval (Var name (TableType d ty) pf))
        ixs <- case cell of
          CellAt ixs -> map fromIntegral <$> mapM (zoom . eval) ixs
          CellNear z rest -> do
            z' <- zoom (eval z)
            rest' <- map fromIntegral <$> mapM (zoom . eval) rest
            pure $ fromMaybe (error ("The point " ++ show z' ++ " is off the table `" ++ symbolVal name ++ "`."))
                             (pointCell t0 z') ++ rest'
        t <- lift (thawForWrite ty t0)
        ok <- lift (writeCell ty t ixs v)
        unless ok $ error ("Table index " ++ show ixs ++ " is out of range for `" ++ symbolVal name ++ "`.")
        when (tableBuffer t /= tableBuffer t0) $ zoom (update pf name (TableType d ty) t)

      TableWriteSlice pf name d ty sl ve -> withKnownType ty $ withKnownNat d $ do
        vs <- zoom (eval ve)
        t0 <- zoom (eval (Var name (TableType d ty) pf))
        ixs <- sliceIndicesOf t0 sl
        t <- lift (thawForWrite ty t0)
        ok <- lift (writeSlice ty t ixs vs)
        unless ok $ error ("The slice " ++ showSlice ixs ++ " of `" ++ symbolVal name ++ "` is out of range, \
                           \or its length isn't the list's, " ++ show (length vs) ++ ".")
        when (tableBuffer t /= tableBuffer t0) $ zoom (update pf name (TableType d ty) t)

      TableCopySlice pf name d ty sl _ src srcSl -> withKnownType ty $ withKnownNat d $ do
        s0 <- zoom (eval src)
        srcIxs <- sliceIndicesOf s0 srcSl
        t0 <- zoom (eval (Var name (TableType d ty) pf))
        ixs <- sliceIndicesOf t0 sl
        t <- lift (thawForWrite ty t0)
        ok <- lift (copySliceBytes (fromMaybe 0 (elementSize ty)) t ixs s0 srcIxs)
        unless ok $ error ("The slice " ++ showSlice srcIxs ++ " can't be copied onto the slice "
                           ++ showSlice ixs ++ " of `" ++ symbolVal name ++ "`: one is out of range, \
                           \or their sizes are different.")
        when (tableBuffer t /= tableBuffer t0) $ zoom (update pf name (TableType d ty) t)

      TableResize pf name d ty szs mfill -> withKnownType ty $ withKnownNat d $ do
        sizes <- mapM (zoom . eval) szs
        fill <- traverse (zoom . eval) mfill
        t0 <- zoom (eval (Var name (TableType d ty) pf))
        lift (resizeTable ty t0 (map fromIntegral sizes) fill) >>= \case
          Nothing -> error ("A table's size can't be negative, but `" ++ symbolVal name ++ "` was given "
                            ++ show sizes ++ ".")
          Just t -> zoom (update pf name (TableType d ty) t)

      NoOp -> pure ()

      DoWhile cond body -> loop
        where loop = do
                body
                tf <- zoom $ eval cond
                if tf then loop else pure ()

      IfThenElse test yes no -> do
        tf <- zoom $ eval test
        if tf then yes else no

      DrawCommand d -> zoom $ runDrawHandler draw d

      Lookup pfList listName listTy@(ListType itemTy) itemName pfNoItem _ env predicate action fallback ->
        recallIsAbsent pfNoItem $ do
        ctxRef <- gets snd
        let go = \case
              [] -> fromMaybe (pure ()) fallback
              (item : items) -> do
                itemRef <- lift (newIORef item)
                let ctxRef' = Bind itemName itemTy itemRef ctxRef
                matches <- lift (evalStateT (eval predicate) ctxRef')
                m' <- gets fst
                case matches of
                  True -> lift (evalStateT action (m', ctxRef'))
                  False -> go items
        go =<< zoom (eval (withEnvironment env $ Var listName listTy pfList))

      ForEach pfList listName listTy@(ListType itemTy) itemName pfNoItem env _ body ->
        recallIsAbsent pfNoItem $ do
        ctxRef <- gets snd
        let go = \case
              [] -> pure ()
              (item : items) -> do
                itemRef <- lift (newIORef item)
                let ctxRef' = Bind itemName itemTy itemRef ctxRef
                m' <- gets fst
                lift (evalStateT body (m', ctxRef'))
                go items
        go =<< zoom (eval (withEnvironment env $ Var listName listTy pfList))

-- | Freeze a table that's about to be shared (see 'tableFrozen').
freezeIfTable :: TypeProxy t -> HaskellType t -> IO ()
freezeIfTable ty v = case ty of
  TableType{} -> freezeTable v
  _ -> pure ()

-- | A slice's indices in a table, one per dimension.
sliceIndicesOf :: Table -> TableSliceAt env
               -> StateT (Map String SomeHaskellType, Context IORefTypeOfBinding env) IO [SliceIndex]
sliceIndicesOf t (SliceAt mp ixs) = do
  p <- traverse (zoom . eval) mp
  ixs' <- mapM (traverse (fmap fromIntegral . zoom . eval)) ixs
  pure (sliceIndices t p ixs')

showSlice :: [SliceIndex] -> String
showSlice ixs = "(" ++ intercalate ", " (map (\case { Fixed k -> show k; Every -> "*" }) ixs) ++ ")"
