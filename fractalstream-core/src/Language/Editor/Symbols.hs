-- | LaTeX-style names for the mathematical symbols a code editor lets
-- you type: @\\theta@ for θ, @\\le@ for ≤, @\\R@ for ℝ, and so on.
-- An editor turns @\\@ and a name into its symbol once the name is
-- finished (by typing something that isn't a letter), showing what the
-- name so far would become ('symbolPreview') while it's typed.
--
-- Names are as LaTeX has them, where LaTeX has one (so @\\epsilon@ is
-- ϵ and @\\varepsilon@ is ε, as in LaTeX), except that @\\to@ and
-- @\\gets@ are the arrows the language writes with, ⟼ and ⟵. Some
-- symbols (∞, ∂, ∑, ...) mean nothing in a script; they're for
-- comments and text.
module Language.Editor.Symbols
  ( symbolNames
  , symbolNamed
  , symbolsStartingWith
  , isSymbolNameChar
  , symbolPreview
  , Completion(..)
  , completeSymbolName
  , replaceSymbolNames
  ) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map as Map

-- | Every symbol name (without its backslash), and its symbol.
symbolNames :: [(String, String)]
symbolNames = greek ++ language ++ other
  where
    greek =
      [ ("alpha", "α"), ("beta", "β"), ("gamma", "γ"), ("delta", "δ")
      , ("epsilon", "ϵ"), ("varepsilon", "ε"), ("zeta", "ζ"), ("eta", "η")
      , ("theta", "θ"), ("vartheta", "ϑ"), ("iota", "ι"), ("kappa", "κ")
      , ("varkappa", "ϰ"), ("lambda", "λ"), ("mu", "μ"), ("nu", "ν")
      , ("xi", "ξ"), ("omicron", "ο"), ("pi", "π"), ("varpi", "ϖ")
      , ("rho", "ρ"), ("varrho", "ϱ"), ("sigma", "σ"), ("varsigma", "ς")
      , ("tau", "τ"), ("upsilon", "υ"), ("phi", "ϕ"), ("varphi", "φ")
      , ("chi", "χ"), ("psi", "ψ"), ("omega", "ω")
      -- (the capitals that look different from Latin ones)
      , ("Gamma", "Γ"), ("Delta", "Δ"), ("Theta", "Θ"), ("Lambda", "Λ")
      , ("Xi", "Ξ"), ("Pi", "Π"), ("Sigma", "Σ"), ("Upsilon", "Υ")
      , ("Phi", "Φ"), ("Psi", "Ψ"), ("Omega", "Ω") ]
    -- symbols that are part of the language
    language =
      [ ("to", "⟼"), ("gets", "⟵"), ("mapsto", "↦"), ("longmapsto", "⟼")
      , ("rightarrow", "→"), ("leftarrow", "←")
      , ("longrightarrow", "⟶"), ("longleftarrow", "⟵")
      , ("le", "≤"), ("leq", "≤"), ("ge", "≥"), ("geq", "≥")
      , ("ne", "≠"), ("neq", "≠"), ("equiv", "≡")
      , ("coloneqq", "≔"), ("coloneq", "≔"), ("times", "×"), ("prime", "′")
      , ("R", "ℝ"), ("C", "ℂ"), ("Z", "ℤ"), ("ldots", "…") ]
    other =
      [ ("N", "ℕ"), ("Q", "ℚ"), ("infty", "∞"), ("partial", "∂")
      , ("nabla", "∇"), ("sum", "∑"), ("prod", "∏"), ("int", "∫")
      , ("sqrt", "√"), ("pm", "±"), ("mp", "∓"), ("cdot", "·")
      , ("circ", "∘"), ("div", "÷"), ("approx", "≈"), ("sim", "∼")
      , ("cong", "≅"), ("propto", "∝"), ("in", "∈"), ("notin", "∉")
      , ("subset", "⊂"), ("subseteq", "⊆"), ("cup", "∪"), ("cap", "∩")
      , ("setminus", "∖"), ("emptyset", "∅"), ("forall", "∀"), ("exists", "∃")
      , ("neg", "¬"), ("land", "∧"), ("lor", "∨"), ("implies", "⟹")
      , ("iff", "⟺"), ("ell", "ℓ"), ("hbar", "ℏ"), ("Re", "ℜ"), ("Im", "ℑ")
      , ("cdots", "⋯"), ("degree", "°") ]

symbolTable :: Map.Map String String
symbolTable = Map.fromList symbolNames

-- | The symbol a name is for, if it's a name.
symbolNamed :: String -> Maybe String
symbolNamed n = Map.lookup n symbolTable

-- | The names that start with a prefix, and their symbols: the name
-- that's the prefix itself first, then shorter names before longer.
symbolsStartingWith :: String -> [(String, String)]
symbolsStartingWith prefix =
  sortOn (\(n, _) -> (length n, n)) [ ns | ns@(n, _) <- symbolNames, prefix `isPrefixOf` n ]

-- | What a name is made of: ASCII letters.
isSymbolNameChar :: Char -> Bool
isSymbolNameChar c = isAsciiLower c || isAsciiUpper c

-- | What to show while a name is being typed (after its backslash):
-- what it would become, with any longer names it could still turn
-- into, as a line like @⟼ \\to    τ \\tau    θ \\theta …@ (for @t@); and which part of
-- that line (character offsets, from and up to) is the symbol the name
-- is now, if it's a name. Nothing, before any letters.
symbolPreview :: String -> Maybe (String, Maybe (Int, Int))
symbolPreview "" = Nothing
symbolPreview prefix = Just $ case symbolsStartingWith prefix of
  [] -> ("no symbol named \\" ++ prefix, Nothing)
  matches ->
    let shown = take maxShown matches
        entries = [ sym ++ " \\" ++ n | (n, sym) <- shown ]
        line = joinWith "    " entries ++ (if length matches > maxShown then "    …" else "")
        exact = case zip shown entries of
          ((n, _), entry) : _ | n == prefix -> Just (0, length entry)
          _ -> Nothing
    in (line, exact)
  where
    maxShown = 8
    joinWith sep = foldr1 (\a b -> a ++ sep ++ b)

-- | What Tab does with a name being typed ('completeSymbolName').
data Completion
  = Finished String
    -- ^ turn it into this symbol
  | Longer String
    -- ^ go on with the name this far (as far as every name it could
    -- become goes alike)
  deriving (Eq, Show)

-- | Complete a name, if it can be: a name becomes its symbol, and so
-- does a name that only one name starts with (@\\th@ is θ). Otherwise,
-- the name goes on as far as the names it could be agree (@\\va@ is
-- @\\var@), even if that's a name with longer ones (@\\subs@ is
-- @\\subset@, which another Tab makes ⊂, for @\\subseteq@'s sake).
completeSymbolName :: String -> Maybe Completion
completeSymbolName prefix = case symbolsStartingWith prefix of
  [] -> Nothing
  matches@((n, sym) : _)
    | n == prefix -> Just (Finished sym)
    | [(_, only)] <- matches -> Just (Finished only)
    | otherwise ->
        let common = foldr1 commonPrefix (map fst matches)
        in if length common > length prefix then Just (Longer common) else Nothing
  where
    commonPrefix (a : as) (b : bs) | a == b = a : commonPrefix as bs
    commonPrefix _ _ = []

-- | Turn every name in some text into its symbol, as if it had been
-- typed (for text that's pasted): a backslash and the letters after it,
-- if they're a name. A backslash right after a backslash keeps the
-- name after it as it is (@\\\\theta@).
replaceSymbolNames :: String -> String
replaceSymbolNames = \case
  '\\' : '\\' : rest -> '\\' : '\\' : replaceSymbolNames rest
  '\\' : rest ->
    let (name, after) = span isSymbolNameChar rest
    in maybe ('\\' : name) id (symbolNamed name) ++ replaceSymbolNames after
  c : rest -> c : replaceSymbolNames rest
  [] -> []
