module Language.Editor.SymbolsSpec (spec) where

import Test.Hspec

import Control.Monad (forM_)
import Data.List (nub)

import Language.Editor.Symbols
import Language.Parser.Tokenizer (SRToken(..), Token(..), tokenize, identifierName)

spec :: Spec
spec = describe "symbol names" $ do

  it "are letters, each named once" $ do
    forM_ symbolNames $ \(n, _) -> n `shouldSatisfy` all isSymbolNameChar
    forM_ accentNames $ \(n, _) -> n `shouldSatisfy` all isSymbolNameChar
    let names = map fst symbolNames ++ map fst accentNames
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

  it "put accents that make identifiers" $ do
    let accented = [ new | (n, _) <- accentNames, Complete new _ <- [escapeAt True ("\\" ++ n ++ " z")] ]
    length accented `shouldBe` length accentNames
    forM_ accented $ \x ->
      map baseToken (tokenize (x ++ "₁ + 1")) `shouldBe` [Identifier (x ++ "₁"), Plus, NumberI 1]

  it "put accents that make the same identifier, however it's typed" $
    map baseToken (tokenize "a\x302 = \x00E2") `shouldBe` [Identifier "\x00E2", Equal, Identifier "\x00E2"]

  it "aren't partial names" $ do
    symbolNamed "thet" `shouldBe` Nothing
    symbolNamed "theta" `shouldBe` Just "θ"
    symbolNamed "Theta" `shouldBe` Just "Θ"

  describe "previews" $ do

    it "show nothing before a name is started" $
      escapePreview "\\" `shouldBe` Nothing

    it "show the symbol a name is, marked, and longer names" $
      escapePreview "\\in" `shouldBe` Just ("∈ \\in    ∫ \\int    ∞ \\infty", Just (0, 5))

    it "show what a partial name could become, unmarked" $
      escapePreview "\\the" `shouldBe` Just ("θ \\theta", Nothing)

    it "say when no name starts that way" $
      escapePreview "\\thx" `shouldBe` Just ("no symbol named \\thx", Nothing)

    it "show only the first several names" $ do
      let line = maybe "" fst (escapePreview "\\c")
      line `shouldSatisfy` (\l -> drop (length l - 1) l == "…")

    it "show an accent as it waits for what it goes on" $ do
      escapePreview "\\hat" `shouldBe` Just ("◌̂ \\hat", Just (0, 7))
      escapePreview "\\hat " `shouldBe` Just ("◌̂ \\hat, on what comes next", Nothing)
      escapePreview "\\hat{x" `shouldBe` Just ("◌̂ \\hat, on what comes next", Nothing)
      escapePreview "\\hat{\\the" `shouldBe` Just ("θ \\theta", Nothing)

  describe "escapes" $ do

    it "finish a symbol's name with what comes after it" $ do
      escapeAt False "\\theta" `shouldBe` Incomplete
      escapeAt False "\\theta " `shouldBe` Complete "θ" 6
      escapeAt False "\\alpha\\" `shouldBe` Complete "α" 6
      escapeAt True "\\theta" `shouldBe` Complete "θ" 6

    it "put an accent on what comes next" $ do
      escapeAt False "\\hat" `shouldBe` Incomplete
      escapeAt False "\\hat " `shouldBe` Incomplete
      escapeAt False "\\hat z" `shouldBe` Complete (nfc "z\x302") 6
      escapeAt False "\\hat{z" `shouldBe` Incomplete
      escapeAt False "\\hat{z}" `shouldBe` Complete (nfc "z\x302") 7
      escapeAt False "\\hat\\theta" `shouldBe` Incomplete
      escapeAt False "\\hat\\theta " `shouldBe` Complete (nfc "θ\x302") 10
      escapeAt False "\\hatθ" `shouldBe` Complete (nfc "θ\x302") 5
      escapeAt False "\\vec{\\dot x}" `shouldBe` Complete (nfc "x\x307\x20D7") 12

    it "put accents in composed form, where there is one" $
      escapeAt False "\\hat a" `shouldBe` Complete "â" 6

    it "leave alone what isn't a name, or can't have an accent" $ do
      escapeAt False "\\\\" `shouldBe` Literal
      escapeAt False "\\foo " `shouldBe` NotAName
      escapeAt False "\\hat{xy}" `shouldBe` NotAName
      escapeAt False "\\hat}" `shouldBe` NotAName
      escapeAt True "\\hat" `shouldBe` NotAName

  describe "Tab" $ do

    it "finishes a name" $
      completeSymbolName "in" `shouldBe` Just (Finished "∈")

    it "finishes the only name that starts that way" $
      completeSymbolName "th" `shouldBe` Just (Finished "θ")

    it "goes on as far as the names that start that way agree" $ do
      completeSymbolName "va" `shouldBe` Just (Longer "var")
      completeSymbolName "subs" `shouldBe` Just (Longer "subset")
      completeSymbolName "subset" `shouldBe` Just (Finished "⊂")

    it "finishes an accent's name, to wait for what it goes on" $ do
      completeSymbolName "ha" `shouldBe` Just (Finished "\\hat ")
      completeSymbolName "ddo" `shouldBe` Just (Finished "\\ddot ")

    it "does nothing when it can't" $ do
      completeSymbolName "s" `shouldBe` Nothing
      completeSymbolName "thx" `shouldBe` Nothing

  describe "in pasted text" $ do

    it "become their symbols" $
      replaceSymbolNames "let \\theta = 2 \\times \\pi # \\alpha\\beta, \\le"
        `shouldBe` "let θ = 2 × π # αβ, ≤"

    it "put accents on what they go on" $
      replaceSymbolNames "\\hat x + \\hat{y} + \\bar\\theta + \\hat{\\dot z}"
        `shouldBe` nfc "x\x302 + y\x302 + θ\x304 + z\x307\x302"

    it "are left as they are if they aren't names, or after two backslashes" $
      replaceSymbolNames "\\th \\thetax \\\\theta \\ \\"
        `shouldBe` "\\th \\thetax \\\\theta \\ \\"

-- | The symbol a name is for (or an empty string, which no test expects).
named :: String -> String
named = maybe "" id . symbolNamed

-- | In composed form, as names are.
nfc :: String -> String
nfc = identifierName
