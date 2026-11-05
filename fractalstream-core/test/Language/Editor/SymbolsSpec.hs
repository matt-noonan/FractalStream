module Language.Editor.SymbolsSpec (spec) where

import Test.Hspec

import Control.Monad (forM_)
import Data.List (nub)

import Language.Editor.Symbols
import Language.Parser.Tokenizer (SRToken(..), Token(..), tokenize)

spec :: Spec
spec = describe "symbol names" $ do

  it "are letters, each named once" $ do
    forM_ symbolNames $ \(n, _) -> n `shouldSatisfy` all isSymbolNameChar
    let names = map fst symbolNames
    nub names `shouldBe` names

  it "name Greek letters that are identifiers" $
    forM_ ["alpha", "theta", "vartheta", "epsilon", "varphi", "lambda", "omega", "Gamma", "Omega"] $ \n -> do
      let sym = named n
      map baseToken (tokenize sym) `shouldBe` [Identifier sym]

  it "name the language's own symbols" $ do
    let means n spelled = do
          let sym = named n
          map baseToken (tokenize sym) `shouldBe` map baseToken (tokenize spelled)
    "to" `means` "->"
    "gets" `means` "<-"
    "mapsto" `means` "->"
    "le" `means` "<="
    "geq" `means` ">="
    "ne" `means` "!="
    "equiv" `means` "==="
    "coloneqq" `means` ":="
    "times" `means` "*"
    "prime" `means` "'"
    "pi" `means` "pi"

  it "aren't partial names" $ do
    symbolNamed "thet" `shouldBe` Nothing
    symbolNamed "theta" `shouldBe` Just "θ"
    symbolNamed "Theta" `shouldBe` Just "Θ"

  describe "previews" $ do

    it "show nothing before a name is started" $
      symbolPreview "" `shouldBe` Nothing

    it "show the symbol a name is, marked, and longer names" $
      symbolPreview "in" `shouldBe` Just ("∈ \\in    ∫ \\int    ∞ \\infty", Just (0, 5))

    it "show what a partial name could become, unmarked" $
      symbolPreview "the" `shouldBe` Just ("θ \\theta", Nothing)

    it "say when no name starts that way" $
      symbolPreview "thx" `shouldBe` Just ("no symbol named \\thx", Nothing)

    it "show only the first several names" $ do
      let line = maybe "" fst (symbolPreview "c")
      line `shouldSatisfy` (\l -> drop (length l - 1) l == "…")

  describe "Tab" $ do

    it "finishes a name" $
      completeSymbolName "in" `shouldBe` Just (Finished "∈")

    it "finishes the only name that starts that way" $
      completeSymbolName "th" `shouldBe` Just (Finished "θ")

    it "goes on as far as the names that start that way agree" $ do
      completeSymbolName "va" `shouldBe` Just (Longer "var")
      completeSymbolName "subs" `shouldBe` Just (Longer "subset")
      completeSymbolName "subset" `shouldBe` Just (Finished "⊂")

    it "does nothing when it can't" $ do
      completeSymbolName "s" `shouldBe` Nothing
      completeSymbolName "thx" `shouldBe` Nothing

  describe "in pasted text" $ do

    it "become their symbols" $
      replaceSymbolNames "let \\theta = 2 \\times \\pi # \\alpha\\beta, \\le"
        `shouldBe` "let θ = 2 × π # αβ, ≤"

    it "are left as they are if they aren't names, or after two backslashes" $
      replaceSymbolNames "\\th \\thetax \\\\theta \\ \\"
        `shouldBe` "\\th \\thetax \\\\theta \\ \\"

-- | The symbol a name is for (or an empty string, which no test expects).
named :: String -> String
named = maybe "" id . symbolNamed
