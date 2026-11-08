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
spec = describe "tables" $ do

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
