module Language.Value.ParserSpec (spec) where

import Test.Hspec
import Language.Type
import Language.Value
import Language.Value.Parser hiding (parseValue)
import Language.Value.Evaluator
import Language.Untyped.Shape
import Language.Untyped.Infer
import Language.Untyped.Constraints hiding (UnboundVariable)
import Helper.UnionFind
import Data.STRef
import Control.Monad.State
import qualified Data.Map as Map
import GHC.TypeLits
import Control.Monad.Except
import Data.Functor ((<&>))

parseValue :: EnvironmentProxy env
           -> ScalarProxy t
           -> String
           -> Either (Int, BadParse) (Value '(env, t))
parseValue env rt input = do
  -- Parse the untyped value
  uv <- parseUntypedValue input
  -- Infer the value's shape
  (v, ctx') <- runST $ do
    ctx <- fmap Map.fromList $ fromEnvironmentM env $ \name ty -> do
      let go :: forall ty s. ScalarProxy ty -> ST s (UF s (STShape s))
          go = \case
            BooleanProxy -> fresh (STShape BoolShape)
            IntegerProxy -> fresh (STShape NumShape)
            RealProxy    -> fresh (STShape NumShape)
            ComplexProxy -> fresh (STShape NumShape)
            ColorProxy   -> fresh (STShape ColorShape)
            PairProxy x y -> do
              ps <- PairShape <$> go x <*> go y
              fresh (STShape ps)
            _ -> error "missing case"
      t <- go ty
      pure (symbolVal name, t)

    s <- inferShape ctx uv

    nextTV <- newSTRef 0
    ctx' <- forM ctx (shapeToTS nextTV [])
    evalStateT (toTypeShape nextTV s) ctx' <&> \case
      Left  e -> Left . (-1,) $ case e of
        Unbound n -> UnboundVariable n
        _         -> Other (show e)
      Right v -> pure (v, ctx')

  --traceM ("v = " ++ show v)
  -- Infer the value's type
  case evalStateT (infer env ctx' rt v) initialTCState of
    Right typedValue -> pure typedValue
    Left e -> throwError . (-1,) $ case e of
      _ -> Other (show e)

spec :: Spec
spec = do

  describe "when parsing values" $ do

    let eval = fmap (`evaluate` EmptyContext)
        parseI = eval . parseValue EmptyEnvProxy IntegerProxy
        parseF = eval . parseValue EmptyEnvProxy RealProxy
        parseB = eval . parseValue EmptyEnvProxy BooleanProxy

    it "can parse simple arithmetic expressions" $ do
      let parses1 = parseI "(1 + 2) *3+ 4"
          parses2 = parseF "(1.0 + 2) *3.25 + 3.75"
          parses3 = parseI "5 - 3"
          parses4 = parseI "-17"
          parses5 = parseI "-2 * -21"
      parses1 `shouldBe` Right 13
      parses2 `shouldBe` Right 13.5
      parses3 `shouldBe` Right 2
      parses4 `shouldBe` Right (-17)
      parses5 `shouldBe` Right 42

    it "can parse tuples" $ do
      let ty = PairProxy IntegerProxy IntegerProxy
          parses = parseValue EmptyEnvProxy ty "(1 + 2, 3 * 4)"
      eval parses `shouldBe` Right (3, 12)

    it "parses with expected precedence" $ do
      let parses1 = parseI "1 + 2 * 3 + 4"
          parses2 = parseI "1 + 12 / 2 * 3"
          parses3 = parseI "1 - 2 - 3"
          parses4 = parseI "1 - 4 / 2 - 3"
          parses5 = parseI "5 - 3 + 4 - 2 + 1"
          parses6 = parseI "--10 + ---6"
      parses1 `shouldBe` Right 11
      parses2 `shouldBe` Right 3
      parses3 `shouldBe` Right (-4)
      parses4 `shouldBe` Right (-4)
      parses5 `shouldBe` Right 5
      parses6 `shouldBe` Right 4

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
          parses4 = parseF "cos(2) pi  - cos (2 pi)"
      parses1 `shouldBe` Right 36
      parses2 `shouldBe` Right (-2)
      parses3 `shouldBe` Left (3, AmbiguousParse)
      parses4 `shouldBe` Right (cos 2 * pi - 1)

  describe "when parsing parameterized values" $ do

    let env = BindingProxy (Proxy @"x") IntegerProxy EmptyEnvProxy
        ctx x = Bind (Proxy @"x") IntegerProxy x EmptyContext
        parseI1 s x = fmap (`evaluate` (ctx x)) (parseValue env IntegerProxy s)
        envC = BindingProxy (Proxy @"x") RealProxy
             $ BindingProxy (Proxy @"y") RealProxy
             $ EmptyEnvProxy
        ctxC x y = Bind (Proxy @"x") RealProxy x
                 $ Bind (Proxy @"y") RealProxy y
                 $ EmptyContext
        envC' = BindingProxy (Proxy @"z") ComplexProxy
              $ BindingProxy (Proxy @"r") RealProxy
              $ EmptyEnvProxy
        ctxC' z r = Bind (Proxy @"z") ComplexProxy z
                  $ Bind (Proxy @"r") RealProxy r
                  $ EmptyContext
        parseCR s x y = fmap (`evaluate` (ctxC x y)) (parseValue envC ComplexProxy s)
        parseBC s z r = fmap (`evaluate` (ctxC' z r)) (parseValue envC' BooleanProxy s)

    it "parses expressions with variables in the environment" $ do
      let parses1 = parseI1 "(1 + x) *3 + 4"
          parses2 = parseI1 "x x + 1"
          parses3 = parseBC "re(z) re(z) + im(z) im(z) < r^2"
      parses1 0    `shouldBe` Right 7
      parses1 (-1) `shouldBe` Right 4
      parses2 2    `shouldBe` Right 5
      parses3 (1 :+ 2) 2 `shouldBe` Right False
      parses3 (1 :+ 2) 3 `shouldBe` Right True

    it "will not parse an unbound variable" $ do
      let parses1 = parseI1 "(1 + y) *3 + 4"
      parses1 0    `shouldBe` Left (-1, UnboundVariable "y") -- FIXME: location should be 4

    it "will not parse a variable at the wrong type" $ do
      let parses1 = parseI1 "if x and false then 1 else 2"
      parses1 0 `shouldBe` Left (-1, Other "ShapeErr \"Inconsistent\"") -- FIXME, location: (2, MismatchedType "x")

    it "can coerce values of compatible types" $ do
      let parses1 = parseI1 "if (pi = x) then 1 else 0"
          parses2 = parseI1 "if (x = pi) then 1 else 0"
          parses3 = parseCR "x + y i"
      parses1 0 `shouldBe` Right 0
      parses2 0 `shouldBe` Right 0
      parses3 1 2 `shouldBe` Right (1 :+ 2)

  describe "when parsing boolean-valued operations" $ do
    let env = BindingProxy (Proxy @"x") IntegerProxy
            $ BindingProxy (Proxy @"y") RealProxy
            $ BindingProxy (Proxy @"z") ComplexProxy
            $ EmptyEnvProxy
        ctx x y z = Bind (Proxy @"x") IntegerProxy x
                  $ Bind (Proxy @"y") RealProxy y
                  $ Bind (Proxy @"z") ComplexProxy z
                  $ EmptyContext
        parseB1 s x y z = fmap (`evaluate` (ctx x y z))
          (parseValue env BooleanProxy s)

    it "can parse equalities" $ do
      let parses1 = parseB1 "exp log 2 = log exp 2"
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
