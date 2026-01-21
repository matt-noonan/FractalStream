{-# language AllowAmbiguousTypes #-}
module Language.Value.ParserSpec (spec) where

import Test.Hspec

import FractalStream.Prelude

import Language.Type
import Language.Value
import Language.Value.Evaluator

import qualified Data.Map as Map

import qualified Language.Value.Parser as P

parseValue :: EnvironmentProxy env
           -> TypeProxy t
           -> String
           -> Either String (Value '(env, t))
parseValue env ty i = withEnvironment env $ withKnownType ty $
  first (`P.ppFullError` i) (P.parseValue Map.empty i)

spec :: Spec
spec = do

  describe "when parsing values" $ do

    let eval = fmap (`evaluate` EmptyContext)
        parseI = eval . parseValue endOfDecls IntegerType
        parseF = eval . parseValue endOfDecls RealType
        parseB = eval . parseValue endOfDecls BooleanType

    it "can parse simple arithmetic expressions" $ do
      let parses1 = parseI "(1 + 2) *3+ 4"
          parses2 = parseF "(1.0 + 2) *3.25 + 3.75"
          parses3 = parseI "5 - 3"
          parses4 = parseI "-17"
          parses5 = parseI "-2 * -21"
          parses6 = parseI "6 // 2"
      parses1 `shouldBe` Right 13
      parses2 `shouldBe` Right 13.5
      parses3 `shouldBe` Right 2
      parses4 `shouldBe` Right (-17)
      parses5 `shouldBe` Right 42
      parses6 `shouldBe` Right 3

    it "can parse tuples" $ do
      let ty = PairType IntegerType IntegerType
          parses = parseValue EmptyEnvProxy ty "(1 + 2, 3 * 4)"
      eval parses `shouldBe` Right (3, 12)

    it "parses with expected precedence" $ do
      let parses1 = parseI "1 + 2 * 3 + 4"
          parses2 = parseI "1 + 12 // 2 * 3"
          parses3 = parseI "1 - 2 - 3"
          parses4 = parseI "1 - 4 // 2 - 3"
          parses5 = parseI "5 - 3 + 4 - 2 + 1"
          parses6 = parseI "--10 + ---6"
      parses1 `shouldBe` Right 11
      parses2 `shouldBe` Right 19
      parses3 `shouldBe` Right (-4)
      parses4 `shouldBe` Right (-4)
      parses5 `shouldBe` Right 5
      parses6 `shouldBe` Right 4

    it "parses exponential towers with the correct associativity" $ do
      parseI "3 ^ 2 ^ 3" `shouldBe` Right 6561
      parseI "(3 ^ 2) ^ 3" `shouldBe` Right 729

    it "parses superscripts as exponents" $ do
      parseI "3² + 1" `shouldBe` Right 10
      parseI "2¹⁰" `shouldBe` Right 1024
      parseF "2.0⁻¹" `shouldBe` Right 0.5
      parseF "2⁻¹" `shouldBe` Right 0

    it "parses boolean expressions with expected precedence" $ do
      let parses1 = parseB "true or false and false"
          parses2 = parseB "(true or false) and false"
          parses3 = parseB "not true or false and not false"
          parses4 = parseB "not (true or false) and not false"
      parses1 `shouldBe` Right True
      parses2 `shouldBe` Right False
      parses3 `shouldBe` Right False
      parses4 `shouldBe` Right False

    it "parses if/then/else expressions with expected precedence" $ do
      let parses1 = parseI "if true and false then 1 + 2 else 4 * 5"
          parses2 = parseI "if false then 1 else if true then 2 else 3"
      parses1 `shouldBe` Right 20
      parses2 `shouldBe` Right 2

    it "parses function applications" $ do
      let parses1 = parseF "exp (log 17)"
          parses2 = parseF "exp log(17)"
          parses3 = parseF "exp(log(17)) - cos pi"
      parses1 `shouldBe` Right 17
      parses2 `shouldBe` Right 17
      parses3 `shouldBe` Right 18

    it "parses absolute value bars" $ do
      let parses0 = parseF "|3|"
          parses1 = parseF "|-3| + | 5 - 6|"
          parses2 = parseF "||-1||"
          parses3 = parseF "log |-e|"
      parses0 `shouldBe` Right 3
      parses1 `shouldBe` Right 4
      parses2 `shouldBe` Right 1
      parses3 `shouldBe` Right 1

  describe "when using common notational quirks" $ do

    let eval = fmap (`evaluate` EmptyContext)
        parseI = eval . parseValue EmptyEnvProxy IntegerType
        parseF = eval . parseValue EmptyEnvProxy RealType

    it "parses concatenation as function application" $ do
      let parses1 = parseF "cos pi"
          parses2 = parseF "exp exp 0"
      parses1 `shouldBe` Right (-1)
      parses2 `shouldBe` Right (exp 1)

    it "also parses concatenation as multiplication" $ do
      let parses1 = parseI "(1 + 2) 3 4"
          parses2 = parseF "2 cos pi"
          parses3 = parseF "cos 2 pi"
          parses4 = parseF "cos(2) * pi  - cos (2 pi)"
          parses5 = parseF "3 cos 2 sin 1"
          parses6 = parseF "3 2^2"
          parses7 = parseF "2-3" -- this should become subtraction, not 2 * -3!
      parses1 `shouldBe` Right 36
      parses2 `shouldBe` Right (-2)
      parses3 `shouldBe` Left (unlines
          [ "  cos 2 pi"
          , "  ^^^^^"
          , ""
          , "To avoid ambiguity when using implicit multiplication, functions must go to the left of other values"])
      parses4 `shouldBe` Right (cos 2 * pi - 1)
      parses5 `shouldBe` Right (3 * cos 2 * sin 1)
      parses6 `shouldBe` Right 12
      parses7 `shouldBe` Right (-1)

  describe "when parsing parameterized values" $ do

    let env = declare @"x" IntegerType $ endOfDecls
        ctx x = Bind (Proxy @"x") IntegerType x EmptyContext
        parseI1 s x = fmap (`evaluate` (ctx x)) (parseValue env IntegerType s)
        envC = declare @"x" RealType
             $ declare @"y" RealType
             $ endOfDecls
        ctxC x y = Bind (Proxy @"x") RealType x
                 $ Bind (Proxy @"y") RealType y
                 $ EmptyContext
        envC' = declare @"z" ComplexType
              $ declare @"r" RealType
              $ endOfDecls
        ctxC' z r = Bind (Proxy @"z") ComplexType z
                  $ Bind (Proxy @"r") RealType r
                  $ EmptyContext
        parseCR s x y = fmap (`evaluate` (ctxC x y)) (parseValue envC ComplexType s)
        parseBC s z r = fmap (`evaluate` (ctxC' z r)) (parseValue envC' BooleanType s)

    it "parses expressions with variables in the environment" $ do
      let parses1 = parseI1 "(1 + x) *3 + 4"
          parses2 = parseI1 "x x + 1"
          parses3 = parseBC "re(z) re(z) + im(z) im(z) < r^2"
          parses4 = parseI1 "x² + 1"
          parses5 = parseI1 "-2 x + 5"
          parses6 = parseCR "x^2 y^2"
      parses1 0    `shouldBe` Right 7
      parses1 (-1) `shouldBe` Right 4
      parses2 2    `shouldBe` Right 5
      parses3 (1 :+ 2) 2 `shouldBe` Right False
      parses3 (1 :+ 2) 3 `shouldBe` Right True
      parses4 3 `shouldBe` Right 10
      parses5 2 `shouldBe` Right 1
      parses6 2 3 `shouldBe` Right 36

    it "will not parse an unbound variable" $ do
      let parses1 = parseI1 "(1 + y) *3 + 4"
      parses1 0 `shouldBe` Left (unlines
        [ "  (1 + y) *3 + 4"
        , "       ^"
        , ""
        , "No variable named y is defined here." ])

    it "will not parse a variable at the wrong type" $ do
      let parses1 = parseI1 "if x and false then 1 else 2"
      parses1 0 `shouldBe` Left (unlines
        [ "  if x and false then 1 else 2"
        , "     ^"
        , ""
        , "I expected a truth value here, but x is an integer."])

    it "can coerce values of compatible types" $ do
      let parses1 = parseI1 "if (pi ≡ x) then 1 else 0"
          parses2 = parseI1 "if (x ≡ pi) then 1 else 0"
          parses3 = parseCR "x + y i"
          parses4 = pprint <$> parseValue endOfDecls RealType "1 + 2"
      parses1 0 `shouldBe` Right 0
      parses2 0 `shouldBe` Right 0
      parses3 1 2 `shouldBe` Right (1 :+ 2)
      parses4 `shouldBe` Right "(1 + 2):R"

  describe "when parsing boolean-valued operations" $ do
    let env = declare @"x" IntegerType
            $ declare @"y" RealType
            $ declare @"z" ComplexType
            $ endOfDecls
        ctx x y z = Bind (Proxy @"x") IntegerType x
                  $ Bind (Proxy @"y") RealType y
                  $ Bind (Proxy @"z") ComplexType z
                  $ EmptyContext
        parseB1 s x y z = fmap (`evaluate` (ctx x y z))
          (parseValue env BooleanType s)

    it "can parse equalities" $ do
      let parses1 = parseB1 "exp log 2 ≡ log exp 2"
          parses2 = parseB1 "1 + 2 + 3 = 2 * 3"
      parses1 1 2 3 `shouldBe` Right True
      parses2 0 0 0 `shouldBe` Right True

    it "can parse inequalities" $ do
      let parses1 = parseB1 "3 x < 5"
      let parses2 = parseB1 "3 x > 5"
      let parses3 = parseB1 "3 x <= 6"
      let parses4 = parseB1 "3 x >= 6"

      parses1 1 0 0 `shouldBe` Right True
      parses1 2 0 0 `shouldBe` Right False
      parses2 1 0 0 `shouldBe` Right False
      parses2 2 0 0 `shouldBe` Right True
      parses3 1 0 0 `shouldBe` Right True
      parses3 2 0 0 `shouldBe` Right True
      parses3 3 0 0 `shouldBe` Right False
      parses4 1 0 0 `shouldBe` Right False
      parses4 2 0 0 `shouldBe` Right True
      parses4 3 0 0 `shouldBe` Right True

  describe "when using keywords that require splices" $ do

    let ctx :: Complex Double -> Double -> Double -> Double
            -> Context HaskellValue VanishEscapeEnv
        ctx z x big tiny
            = Bind (Proxy @"z") ComplexType z
            $ Bind (Proxy @"x") RealType    x
            $ Bind (Proxy @"R") RealType    big
            $ Bind (Proxy @"epsilon") RealType tiny
            $ EmptyContext
        splices = Map.fromList
            [ ("[internal] escape radius",
               either (error . show) id (P.parseParsedValue Map.empty "R"))
            , ("[internal] vanishing radius",
               either (error . show) id (P.parseParsedValue Map.empty "epsilon"))
            ]
        test z x big tiny input =
            let c = ctx z x big tiny
            in (evaluateInContext @_ @'BooleanT $ ctx z x big tiny)
               <$> first (`P.ppFullError` input) (withEnvironment (contextToEnv c)
                                                  $ P.parseValue splices input)

    it "Can splice in the escape radius and vanishing radius comparisons" $ do
      test 1 0    2 0.1 "z escapes" `shouldBe` Right False
      test 3 0    2 0.1 "z escapes" `shouldBe` Right True
      test 1 1.01 2 0.1 "|z| - x vanishes" `shouldBe` Right True
      test 1 1.01 2 0.1 "z + x escapes" `shouldBe` Right True

    it "Can splice in vanishing radius during comparisons" $ do
      test 1 1 2 0.1 "x = x + 0.01" `shouldBe` Right True
      test 1 1 2 0.1 "x ≡ x + 0.01" `shouldBe` Right False


type VanishEscapeEnv = '("z", 'ComplexT) ':
                       '("x", 'RealT) ':
                       '("R", 'RealT) ':
                       '("epsilon", 'RealT) ': '[]
