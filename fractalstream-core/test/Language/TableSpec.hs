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
spec = readSpec >> writeSpec

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

  it "are allocated with a size and corners, and written cell by cell" $ do
    Just t0 <- newTable RealType [0, 0] (0, 0) (1, 1)
    (t, x) <- run t0 (unlines
      [ "allocate T of size (3, 2) from -3/2 + i to 3/2 - i"
      , "T @ (2, 1) <- 5"
      , "x <- T @ (2, 1) + T @ (1, 1)" ])
    tableDims t `shouldBe` [3, 2]
    x `shouldBe` 5
    -- cells are 1 wide and 1 high, and (1, 1) is centered at -1 + i/2
    tableOrigin t `shouldBe` (-1, 0.5)
    tableStep t `shouldBe` (1, 1)

  it "can be filled with a value when they're allocated" $ do
    Just t0 <- newTable RealType [0, 0] (0, 0) (1, 1)
    (t, x) <- run t0 (unlines
      [ "allocate T of size (3, 2) from -3/2 + i to 3/2 - i filled with 7"
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
          , "  allocate T of size (viewer_width, viewer_height)"
          , "  T @ (1, 1) <- 2" ]
    builds src >>= (`shouldSatisfy` either ("Only a tool" `isInfixOf`) (const False))
    builds (unlines (filter (/= "T @ (1, 1) <- 1") (lines src))) `shouldReturn` Right ()
