module Language.Value.ParserSpec (spec) where

import Test.Hspec
import Language.Type
import Language.Value
import Language.Value.Parser
import Language.Value.Evaluator

spec :: Spec
spec = do

  describe "when parsing values" $ do

    let eval = fmap (evaluate EmptyContext)
        parseI = eval . parseValue EmptyEnvProxy IntegerProxy
        parseF = eval . parseValue EmptyEnvProxy RealProxy
        parseB = eval . parseValue EmptyEnvProxy BooleanProxy

    it "can parse simple arithmetic expressions" $ do
      let parses1 = parseI "(1 + 2) *3+ 4"
          parses2 = parseF "(1.0 + 2) *3.25 + 3.75"
      parses1 `shouldBe` Right 13
      parses2 `shouldBe` Right 13.5

    it "can parse tuples" $ do
      let ty = PairProxy IntegerProxy IntegerProxy
          parses = parseValue EmptyEnvProxy ty "(1 + 2, 3 * 4)"
      eval parses `shouldBe` Right (3, 12)

    it "parses with expected precedence" $ do
      let parses1 = parseI "1 + 2 * 3 + 4"
          parses2 = parseI "1 + 12 / 2 * 3"
          parses3 = parseI "1 - 2 - 3"
          parses4 = parseI "1 - 4 / 2 - 3"
      parses1 `shouldBe` Right 11
      parses2 `shouldBe` Right 3
      parses3 `shouldBe` Right (-4)
      parses4 `shouldBe` Right (-4)

    it "parses exponential towers with the correct associativity" $ do
      parseI "3 ^ 2 ^ 3" `shouldBe` Right 6561
      parseI "(3 ^ 2) ^ 3" `shouldBe` Right 729

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
      let parses1 = parseF "|-3| + | 5 - 6|"
          parses2 = parseF "||-1||"
          parses3 = parseF "log |-e|"
      parses1 `shouldBe` Right 4
      parses2 `shouldBe` Right 1
      parses3 `shouldBe` Right 1

  describe "when using common notational quirks" $ do

    let eval = fmap (evaluate EmptyContext)
        parseI = eval . parseValue EmptyEnvProxy IntegerProxy
        parseF = eval . parseValue EmptyEnvProxy RealProxy

    it "parses concatenation as function application" $ do
      let parses1 = parseF "cos pi"
          parses2 = parseF "exp exp 0"
      parses1 `shouldBe` Right (-1)
      parses2 `shouldBe` Right (exp 1)

    it "also parses concatenation as multiplication" $ do
      let parses1 = parseI "(1 + 2) 3 4"
          parses2 = parseF "2 cos pi"
          parses3 = parseF "cos 2pi"
      parses1 `shouldBe` Right 36
      parses2 `shouldBe` Right (-2)
      parses3 `shouldBe` Left (1, AmbiguousParse)

  describe "when parsing parameterized values" $ do

    let env = BindingProxy (Proxy @"x") IntegerProxy (Proxy @'[])
        ctx x = Bind (Proxy @"x") IntegerProxy x EmptyContext
        parseI1 s x = fmap (evaluate (ctx x)) (parseValue env IntegerProxy s)

    it "parses expressions with variables in the environment" $ do
      let parses1 = parseI1 "(1 + x) *3 + 4"
          parses2 = parseI1 "x x + 1"
      parses1 0    `shouldBe` Right 7
      parses1 (-1) `shouldBe` Right 4
      parses2 2    `shouldBe` Right 5

    it "will not parse an unbound variable" $ do
      let parses1 = parseI1 "(1 + y) *3 + 4"
      parses1 0    `shouldBe` Left (4, UnboundVariable "y")

    it "will not parse a variable at the wrong type" $ do
      let parses1 = parseI1 "if x and false then 1 else 2"
      parses1 0 `shouldBe` Left (2, MismatchedType "x")
