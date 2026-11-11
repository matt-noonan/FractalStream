{-# language AllowAmbiguousTypes #-}
-- | Tables (@Table 2 of R@ and so on): their type, declaring them in
-- @\@config@, and reading their elements with @T \@ (j, k)@.
module Language.TableSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Value.Evaluator
import Language.Table
import Actor.Ensemble (buildEnsembleFromScript)
import qualified Language.Value.Parser as P
import qualified Control.Exception as E
import Data.List (isInfixOf)
import Data.IORef
import Language.Code.Parser (parseCode, noCommonDefs)
import Language.Code.InterpretIO (interpretToIO)
import Language.Draw (DrawHandler(..))
import Language.Parser (ppFullError)

type TEnv = '[ '("T", 'TableT 2 'RealT) ]

tableType2R :: TypeProxy ('TableT 2 'RealT)
tableType2R = TableType natSing RealType

parseIn :: forall t. TypeProxy t -> String -> Either String (Value '(TEnv, t))
parseIn ty src = withEnvironment env $ withKnownType ty $
  first (`P.ppFullError` src) (P.parseValue src)
  where env = declare @"T" tableType2R endOfDecls

-- | A 3 x 2 table whose cell (j, k) holds 10 k + j.
sample :: IO Table
sample = do
  Just t <- newTable RealType [3, 2] (0, 0) (1, 1)
  forM_ [1 .. 3] $ \j -> forM_ [1 .. 2] $ \k ->
    writeCell RealType t [j, k] (fromIntegral (10 * k + j))
  pure t

evalIn :: Table -> TypeProxy t -> String -> IO (HaskellType t)
evalIn t ty src = case parseIn ty src of
  Left err -> fail err
  Right v -> E.evaluate (evaluate v (Bind (Proxy @"T") tableType2R t EmptyContext))

builds :: String -> IO (Either String ())
builds src = buildEnsembleFromScript src <&> either (Left . snd) (const (Right ()))

spec :: Spec
spec = readSpec >> writeSpec >> layerSpec

readSpec :: Spec
readSpec = describe "tables" $ do

  it "read the element at a column and row" $ do
    t <- sample
    evalIn t RealType "T @ (2, 1)" `shouldReturn` 12
    evalIn t RealType "T @ (3, 2) + 1" `shouldReturn` 24
    evalIn t RealType "T @ (1, 1 + 1)" `shouldReturn` 21

  it "start out filled with zeros" $ do
    Just t <- newTable ComplexType [2, 2] (0, 0) (1, 1)
    readCell ComplexType t [2, 2] `shouldBe` Just 0

  it "lay out the first index fastest" $ do
    t <- sample
    cellOffset t [2, 1] `shouldBe` Just 1
    cellOffset t [1, 2] `shouldBe` Just 3
    cellOffset t [4, 1] `shouldBe` Nothing
    cellOffset t [1, 0] `shouldBe` Nothing

  it "fail to read out of range" $ do
    t <- sample
    evalIn t RealType "T @ (4, 1)" `shouldThrow` anyErrorCall

  it "need one index per dimension, as a tuple" $ do
    parseIn RealType "T @ 1" `shouldSatisfy` isLeft
    parseIn RealType "T @ (1, 2, 3)" `shouldSatisfy` isLeft

  it "can't be indexed cyclically" $
    parseIn RealType "T @@ (1, 1)" `shouldSatisfy` isLeft

  -- (the sample's cell (j, k) is centered on (j - 1) - (k - 1) i)
  it "tell whether a point is on them" $ do
    t <- sample
    evalIn t BooleanType "(1 - i) in T" `shouldReturn` True
    evalIn t BooleanType "(0.4 + 0.4 i) in T" `shouldReturn` True
    evalIn t BooleanType "(5 + 0 i) in T" `shouldReturn` False
    evalIn t BooleanType "(1 + 2 i) in T" `shouldReturn` False

  it "give the cell a point is in, on the table or not" $ do
    t <- sample
    evalIn t (PairType IntegerType IntegerType) "table_cell(T, 2 - i)" `shouldReturn` (3, 2)
    evalIn t (PairType IntegerType IntegerType) "table_cell(T, 5 + 3 i)" `shouldReturn` (6, -2)

  it "give the point at the center of a cell" $ do
    t <- sample
    evalIn t ComplexType "table_point(T, (3, 2))" `shouldReturn` (2 :+ (-1))
    evalIn t ComplexType "table_point(T, (0, 0))" `shouldReturn` ((-1) :+ 1)

  it "look a point up, or give a fallback for a point off the table" $ do
    t <- sample
    evalIn t RealType "T @ point (2 - i) else 0" `shouldReturn` 23
    evalIn t RealType "T @ point (9 + 0 i) else -1" `shouldReturn` (-1)
    -- (binding like `@`: the fallback is a single operand)
    evalIn t RealType "T @ point (9 + 0 i) else 0 + 1" `shouldReturn` 1
    -- (and an if/then/else with a lookup in it is just that)
    evalIn t RealType "if true then T @ point (2 - i) else 0" `shouldReturn` 23

  it "have points only in two dimensions" $ do
    parseIn BooleanType "1 in 2" `shouldSatisfy` isLeft
    parseIn (PairType IntegerType IntegerType) "table_cell(1, 0)" `shouldSatisfy` isLeft
    parseIn ComplexType "table_point(T, 1)" `shouldSatisfy` isLeft

  it "are declared in @config with no default" $ do
    let viewer = "\n@viewer \"V\"\ncolor <- if re(T @ (1, 1)) > 0 then red else blue\n"
    builds ("@config \"P\"\nT : Table 2 of C\n" ++ viewer) `shouldReturn` Right ()
    builds ("@config \"P\"\nT : Table 2 of C := 0\n" ++ viewer)
      >>= (`shouldSatisfy` either ("can't have a default value" `isInfixOf`) (const False))
    builds ("@config \"P\"\nx : C\n" ++ "\n@viewer \"V\"\ncolor <- red\n")
      >>= (`shouldSatisfy` either ("needs a default value" `isInfixOf`) (const False))

  it "hold only numbers, truth values, and colors" $ do
    let viewer = "\n@viewer \"V\"\ncolor <- red\n"
    builds ("@config \"P\"\nT : Table 1 of Color\n" ++ viewer) `shouldReturn` Right ()
    builds ("@config \"P\"\nT : Table 1 of Text\n" ++ viewer) >>= (`shouldSatisfy` isLeft)

-- | Run some code with a 2-dimensional real table `T` and a real `x`,
-- giving back their final values.
run :: Table -> String -> IO (Table, Double)
run t src = do
  tRef <- newIORef t
  xRef <- newIORef 0
  let refs = Bind (Proxy @"T") tableType2R tRef $ Bind (Proxy @"x") RealType xRef EmptyContext
  code <- either (fail . (`ppFullError` src)) pure (parseCode (envProxy Proxy) noCommonDefs src)
  _ <- execStateT (interpretToIO (DrawHandler (const (pure ()))) code) refs
  (,) <$> readIORef tRef <*> readIORef xRef

frozenSample :: IO Table
frozenSample = do { t <- sample; freezeTable t; pure t }

writeSpec :: Spec
writeSpec = describe "tables, in a tool" $ do

  it "are reset to a size and corners, and written cell by cell" $ do
    Just t0 <- newTable RealType [0, 0] (0, 0) (1, 1)
    (t, x) <- run t0 (unlines
      [ "reset T to (3, 2) from -3/2 + i to 3/2 - i"
      , "T @ (2, 1) <- 5"
      , "x <- T @ (2, 1) + T @ (1, 1)" ])
    tableDims t `shouldBe` [3, 2]
    x `shouldBe` 5
    -- cells are 1 wide and 1 high, and (1, 1) is centered at -1 + i/2
    tableOrigin t `shouldBe` (-1, 0.5)
    tableStep t `shouldBe` (1, 1)

  it "can be filled with a value when they're reset" $ do
    Just t0 <- newTable RealType [0, 0] (0, 0) (1, 1)
    (t, x) <- run t0 (unlines
      [ "reset T to (3, 2) from -3/2 + i to 3/2 - i filled with 7"
      , "T @ (2, 1) <- 5"
      , "x <- T @ (2, 1) + T @ (3, 2)" ])
    x `shouldBe` 12
    readCell RealType t [1, 1] `shouldBe` Just 7

  it "are written and read at the cell nearest a point" $ do
    t0 <- frozenSample
    (_, x) <- run t0 "T @ point (2.2 - 0.9i) <- 7\nx <- T @ (3, 2) + T @ point (0.1 - 0.1i)"
    -- (sample's origin is 0 and its cells are 1 wide, rows going down)
    x `shouldBe` 7 + 11
    run t0 "T @ point (9 + 0i) <- 7" `shouldThrow` anyErrorCall

  it "copy a table a viewer may be reading before writing it" $ do
    t0 <- frozenSample
    (t, x) <- run t0 "T @ (1, 1) <- 9\nx <- T @ (1, 1)"
    x `shouldBe` 9
    readCell RealType t0 [1, 1] `shouldBe` Just 11
    tableBuffer t `shouldNotBe` tableBuffer t0

  it "write a table the tool made itself in place" $ do
    t0 <- sample
    (t, _) <- run t0 "T @ (1, 1) <- 9"
    tableBuffer t `shouldBe` tableBuffer t0

  it "copy a table another variable also holds" $ do
    t0 <- sample
    (_, x) <- run t0 "U := T\nT @ (1, 1) <- 9\nx <- U @ (1, 1)"
    x `shouldBe` 11

  it "have a size" $ do
    t0 <- sample
    (_, x) <- run t0 "(w, h) := table_size(T)\nx <- 10 w + h"
    x `shouldBe` 32

  it "can't be written out of range" $ do
    t0 <- sample
    run t0 "T @ (4, 1) <- 1" `shouldThrow` anyErrorCall

  it "can only be written by tools" $ do
    let src = unlines
          [ "@config \"P\"", "T : Table 2 of R", ""
          , "@viewer \"V\"", "T @ (1, 1) <- 1", "color <- red", ""
          , "@tool \"Fill\" for=\"V\"", "on click:"
          , "  reset T to (viewer_width, viewer_height)"
          , "  T @ (1, 1) <- 2" ]
    builds src >>= (`shouldSatisfy` either ("Only a tool" `isInfixOf`) (const False))
    builds (unlines (filter (/= "T @ (1, 1) <- 1") (lines src))) `shouldReturn` Right ()

------------------------------------------------------------------------
-- Tables with more than two dimensions: the first two are in the plane

type LEnv = '[ '("T", 'TableT 3 'RealT), '("U", 'TableT 2 'RealT), '("x", 'RealT), '("xs", 'ListT 'RealT) ]

tableType3R :: TypeProxy ('TableT 3 'RealT)
tableType3R = TableType natSing RealType

-- | A 3 x 2 x 2 table whose cell (j, k, m) holds 100 m + 10 k + j, with
-- cell (j, k) centered on (j - 1) - (k - 1) i.
layered :: IO Table
layered = do
  Just t <- newTable RealType [3, 2, 2] (0, 0) (1, 1)
  forM_ [1 .. 3] $ \j -> forM_ [1 .. 2] $ \k -> forM_ [1 .. 2] $ \m ->
    writeCell RealType t [j, k, m] (fromIntegral (100 * m + 10 * k + j))
  pure t

parseL :: forall t. TypeProxy t -> String -> Either String (Value '(LEnv, t))
parseL ty src = withEnvironment layerEnv $ withKnownType ty $
  first (`P.ppFullError` src) (P.parseValue src)

layerEnv :: EnvironmentProxy LEnv
layerEnv = declare @"T" tableType3R $ declare @"U" tableType2R $ declare @"x" RealType
         $ declare @"xs" (ListType RealType) endOfDecls

evalL :: Table -> TypeProxy t -> String -> IO (HaskellType t)
evalL t ty src = do
  u <- sample
  case parseL ty src of
    Left err -> fail err
    Right v -> E.evaluate (evaluate v (Bind (Proxy @"T") tableType3R t $ Bind (Proxy @"U") tableType2R u
                                       $ Bind (Proxy @"x") RealType 0 $ Bind (Proxy @"xs") (ListType RealType) [] EmptyContext))

-- | Run some code with `T` (the given table), `U` (the 2-dimensional
-- sample), a real `x`, and a list of reals `xs`, giving back `T`, `x`,
-- and `xs`.
runL :: Table -> String -> IO (Table, Double, [Double])
runL t src = do
  u <- sample
  tRef <- newIORef t
  uRef <- newIORef u
  xRef <- newIORef 0
  xsRef <- newIORef []
  let refs = Bind (Proxy @"T") tableType3R tRef $ Bind (Proxy @"U") tableType2R uRef
           $ Bind (Proxy @"x") RealType xRef $ Bind (Proxy @"xs") (ListType RealType) xsRef EmptyContext
  code <- either (fail . (`ppFullError` src)) pure (parseCode (envProxy Proxy) noCommonDefs src)
  _ <- execStateT (interpretToIO (DrawHandler (const (pure ()))) code) refs
  (,,) <$> readIORef tRef <*> readIORef xRef <*> readIORef xsRef

layerSpec :: Spec
layerSpec = describe "tables with more than two dimensions" $ do

  it "look up a point in the first two dimensions, and indices in the rest" $ do
    t <- layered
    evalL t RealType "T @ (point (1 - i), 2)" `shouldReturn` 222
    evalL t RealType "T @ (point (2 + 0i), 1)" `shouldReturn` 113
    evalL t RealType "T @ (point (9 + 0i), 1) else -1" `shouldReturn` (-1)
    evalL t RealType "T @ (point (1 - i), 2) else -1" `shouldReturn` 222
    evalL t RealType "T @ (point (1 - i), 3)" `shouldThrow` anyErrorCall

  it "have points, in their first two dimensions" $ do
    t <- layered
    evalL t BooleanType "(1 - i) in T" `shouldReturn` True
    evalL t BooleanType "(1 + i) in T" `shouldReturn` False
    evalL t (PairType IntegerType IntegerType) "table_cell(T, 2 - i)" `shouldReturn` (3, 2)
    evalL t ComplexType "table_point(T, (3, 2))" `shouldReturn` (2 :+ (-1))

  it "read a slice with one `*` as a list" $ do
    t <- layered
    evalL t (ListType RealType) "T @ (point (1 - i), *)" `shouldReturn` [122, 222]
    evalL t (ListType RealType) "T @ (*, 1, 2)" `shouldReturn` [211, 212, 213]
    evalL t (ListType RealType) "T @ (3, *, 1)" `shouldReturn` [113, 123]
    evalL t IntegerType "length(T @ (*, 2, 1))" `shouldReturn` 3
    evalL t (ListType RealType) "T @ (4, *, 1)" `shouldThrow` anyErrorCall

  it "need the right indices" $ do
    parseL RealType "T @ (point (1 - i))" `shouldSatisfy` isLeft
    parseL RealType "T @ (1, point (1 - i))" `shouldSatisfy` isLeft
    parseL RealType "T @ (1, 2)" `shouldSatisfy` isLeft
    parseL RealType "T @ point (1 - i)" `shouldSatisfy` isLeft
    parseL (ListType RealType) "T @ (*, *, 1)" `shouldSatisfy` isLeft
    parseL RealType "xs @ *" `shouldSatisfy` isLeft
    parseL RealType "point 1" `shouldSatisfy` isLeft

  it "are written at a point and indices" $ do
    t0 <- layered
    (t, x, _) <- runL t0 "T @ (point (1 - i), 2) <- 7\nx <- T @ (2, 2, 2)"
    x `shouldBe` 7
    readCell RealType t [2, 2, 1] `shouldBe` Just 122

  it "write a slice from a list as long as it" $ do
    t0 <- layered
    (t, _, xs) <- runL t0 "T @ (point (1 - i), *) <- [1, 2]\nxs <- T @ (2, 2, *)"
    xs `shouldBe` [1, 2]
    readSlice RealType t [Every, Fixed 1, Fixed 2] `shouldBe` Just [211, 212, 213]
    runL t0 "T @ (point (1 - i), *) <- [1, 2, 3]" `shouldThrow` anyErrorCall
    runL t0 "T @ (*, 1, 1) <- [1, 2]" `shouldThrow` anyErrorCall

  it "copy one slice onto another" $ do
    t0 <- layered
    (t, _, _) <- runL t0 "T @ (*, *, 1) <- T @ (*, *, 2)"
    readSlice RealType t [Every, Every, Fixed 1] `shouldBe` Just [211, 212, 213, 221, 222, 223]
    -- (from another table, and between slices of other dimensions)
    t1 <- layered
    (t', _, _) <- runL t1 "T @ (*, *, 2) <- U @ (*, *)\nT @ (1, *, *) <- T @ (3, *, *)"
    readSlice RealType t' [Every, Every, Fixed 2] `shouldBe` Just [13, 12, 13, 23, 22, 23]
    readSlice RealType t' [Fixed 1, Every, Fixed 1] `shouldBe` Just [113, 123]

  it "copy slices only of the same shape" $ do
    t0 <- layered
    runL t0 "T @ (*, *, 1) <- T @ (1, *, *)" `shouldThrow` anyErrorCall
    first (const ()) (parseCode layerEnv noCommonDefs "T @ (*, *, 1) <- T @ (*, 1, 1)") `shouldSatisfy` isLeft
    first (const ()) (parseCode layerEnv noCommonDefs "T @ (*, *, 1) <- xs") `shouldSatisfy` isLeft

  it "copy a table a viewer may be reading before writing a slice of it" $ do
    t0 <- layered
    freezeTable t0
    (t, _, _) <- runL t0 "T @ (*, *, 1) <- T @ (*, *, 2)\nT @ (1, 1, *) <- [0, 0]"
    readCell RealType t0 [1, 1, 1] `shouldBe` Just 111
    readCell RealType t [2, 1, 1] `shouldBe` Just 212
    readCell RealType t [1, 1, 2] `shouldBe` Just 0

  it "are resized in place, keeping their cells" $ do
    t0 <- layered
    freezeTable t0
    (t, _, _) <- runL t0 "resize T to (3, 2, 3) filled with -1"
    tableDims t `shouldBe` [3, 2, 3]
    tableOrigin t `shouldBe` tableOrigin t0
    tableStep t `shouldBe` tableStep t0
    readSlice RealType t [Fixed 2, Fixed 2, Every] `shouldBe` Just [122, 222, -1]
    tableDims t0 `shouldBe` [3, 2, 2]
    -- (growing and shrinking in the plane, at the right and the bottom)
    (t', _, _) <- runL t0 "resize T to (4, 1, 2)"
    readSlice RealType t' [Every, Fixed 1, Fixed 2] `shouldBe` Just [211, 212, 213, 0]
    -- (a new size needs one integer per dimension, and none negative)
    runL t0 "resize T to (3, 2, -1)" `shouldThrow` anyErrorCall
    first (const ()) (parseCode layerEnv noCommonDefs "resize T to (3, 2)") `shouldSatisfy` isLeft

  it "drop a layer by copying the later ones down and shrinking" $ do
    Just t0 <- newTable RealType [1, 1, 4] (0, 0) (1, 1)
    forM_ [1 .. 4] $ \m -> writeCell RealType t0 [1, 1, m] (fromIntegral m)
    (t, _, _) <- runL t0 (unlines
      [ "(w, h, n) := table_size(T)"
      , "for each m in range(2, n - 1):"
      , "  T @ (*, *, m) <- T @ (*, *, m + 1)"
      , "resize T to (w, h, n - 1)" ])
    readSlice RealType t [Fixed 1, Fixed 1, Every] `shouldBe` Just [1, 3, 4]
