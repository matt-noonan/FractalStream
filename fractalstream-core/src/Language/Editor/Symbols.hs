-- | LaTeX-style names for the mathematical symbols a code editor lets
-- you type: @\\theta@ for θ, @\\le@ for ≤, @\\R@ for ℝ, and so on;
-- and for accents, combining marks that go on the character after them,
-- as in LaTeX: @\\hat x@, @\\hat{x}@ and @\\hat\\theta@ are x̂ and θ̂.
--
-- An editor turns @\\@ and a name into its symbol once the name is
-- finished (by typing something that isn't a letter), and an accent once
-- what it goes on is there ('escapeAt'), showing what's coming while
-- it's typed ('escapePreview'). Pasted text has its names turned into
-- symbols all at once ('replaceSymbolNames').
--
-- Names are as LaTeX has them, where LaTeX has one (so @\\epsilon@ is
-- ϵ and @\\varepsilon@ is ε, as in LaTeX), except that @\\to@ and
-- @\\gets@ are the arrows the language writes with, ⟼ and ⟵. Some
-- symbols (∞, ∂, ∑, ...) mean nothing in a script; they're for
-- comments and text.
module Language.Editor.Symbols
  ( symbolNames
  , accentNames
  , symbolNamed
  , accentNamed
  , symbolsStartingWith
  , isSymbolNameChar
  , Escape(..)
  , escapeAt
  , escapePreview
  , typedName
  , Completion(..)
  , completeSymbolName
  , replaceSymbolNames
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isMark)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map as Map

import Language.Parser.Tokenizer (identifierName)

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

-- | Every accent's name, and its combining mark.
accentNames :: [(String, Char)]
accentNames =
  [ ("hat", '\x302'), ("tilde", '\x303'), ("bar", '\x304'), ("overline", '\x305')
  , ("dot", '\x307'), ("ddot", '\x308'), ("dddot", '\x20DB'), ("vec", '\x20D7')
  , ("check", '\x30C'), ("breve", '\x306'), ("acute", '\x301'), ("grave", '\x300')
  , ("mathring", '\x30A') ]

-- | What a name is for.
data Named = Symbol String | Accent Char

nameTable :: Map.Map String Named
nameTable = Map.fromList ([ (n, Symbol s) | (n, s) <- symbolNames ] ++ [ (n, Accent m) | (n, m) <- accentNames ])

-- | The symbol a name is for, if it's a symbol's name.
symbolNamed :: String -> Maybe String
symbolNamed n = Map.lookup n nameTable >>= \case
  Symbol s -> Just s
  Accent _ -> Nothing

-- | The combining mark an accent's name is for, if it's an accent's.
accentNamed :: String -> Maybe Char
accentNamed n = Map.lookup n nameTable >>= \case
  Accent m -> Just m
  Symbol _ -> Nothing

-- | How a symbol or accent looks, to show it: an accent on a dotted
-- circle (◌̂).
shown :: Named -> String
shown = \case
  Symbol s -> s
  Accent m -> ['◌', m]

-- | The names that start with a prefix, and how what they're for looks
-- ('shown'): the name that's the prefix itself first, then shorter names
-- before longer.
symbolsStartingWith :: String -> [(String, String)]
symbolsStartingWith prefix = sortOn (\(n, _) -> (length n, n))
  [ (n, shown x) | (n, x) <- Map.toList nameTable, prefix `isPrefixOf` n ]

-- | What a name is made of: ASCII letters.
isSymbolNameChar :: Char -> Bool
isSymbolNameChar c = isAsciiLower c || isAsciiUpper c

-- | What the text at a backslash is ('escapeAt').
data Escape
  = Complete String Int
    -- ^ the first so many characters are finished, and become this
  | Incomplete
    -- ^ not finished yet: there's more to type
  | Literal
    -- ^ two backslashes, which stay as they are
  | NotAName
    -- ^ something that isn't a name (or an accent on nothing it can go
    -- on), which stays as it is
  deriving (Eq, Show)

-- | What the text starting with a backslash is: a symbol's name, which
-- is finished by whatever comes after it (@\\theta@ in @\\theta + 1@);
-- an accent's name and what it goes on, which is the character after it
-- (after any spaces), a name after it, or one character (with any
-- accents) in braces (@\\hat x@, @\\hat\\theta@, @\\hat{\\dot x}@); or
-- neither. With @atEnd@, the text ends there (so @\\theta@ at the end
-- is finished); otherwise more may be typed after it.
escapeAt :: Bool -> String -> Escape
escapeAt atEnd = \case
  '\\' : '\\' : _ -> Literal
  '\\' : rest ->
    let (name, after) = span isSymbolNameChar rest
        nameLen = 1 + length name
    in if null after && not atEnd then Incomplete else case Map.lookup name nameTable of
      Nothing -> NotAName
      Just (Symbol s) -> Complete s nameLen
      Just (Accent m) -> case argument after of
        Right (Just (base, len)) -> Complete (identifierName (base ++ [m])) (nameLen + len)
        Right Nothing -> Incomplete
        Left () -> NotAName
  _ -> NotAName
  where
    -- what an accent goes on, and how many characters that is; or
    -- nothing yet, or nothing it can go on
    argument s =
      let (spaces, s') = span (== ' ') s
          afterSpaces = fmap (fmap (\(b, len) -> (b, length spaces + len)))
      in afterSpaces $ case s' of
        [] -> if atEnd then Left () else Right Nothing
        '{' : inside -> case closingBrace (0 :: Int) inside of
          Nothing -> if atEnd then Left () else Right Nothing
          Just n ->
            let base = replaceSymbolNames (take n inside)
            in if oneCharacter base then Right (Just (base, n + 2)) else Left ()
        '\\' : _ -> case escapeAt atEnd s' of
          Complete base n | oneCharacter base -> Right (Just (base, n))
          Incomplete -> Right Nothing
          _ -> Left ()
        c : more
          | c == '}' || c < ' ' || c == ' ' -> Left ()
          | otherwise -> let marks = takeWhile isMark more
                         in Right (Just (c : marks, 1 + length marks))
    -- where the brace that closes a group is
    closingBrace depth = go 0 depth
      where
        go _ _ [] = Nothing
        go i d (c : cs) = case c of
          '}' | d == 0 -> Just i
              | otherwise -> go (i + 1) (d - 1) cs
          '{' -> go (i + 1) (d + 1) cs
          _ -> go (i + 1) d cs
    oneCharacter = \case
      c : marks -> not (isMark c) && all isMark marks
      [] -> False

-- | What to show while an escape is being typed (the text from its
-- backslash to the cursor): while a name is typed, what it would become,
-- with any longer names it could still turn into, as a line like
-- @⟼ \\to    τ \\tau    θ \\theta …@ (for @t@), and which part of that
-- line (character offsets, from and up to) is the name as it is now, if
-- it's a name; or, while an accent waits for what it goes on, the
-- accent. Nothing, before any letters.
escapePreview :: String -> Maybe (String, Maybe (Int, Int))
escapePreview text = case typedName text of
  Just "" -> waiting
  Just prefix -> Just (names prefix)
  Nothing -> waiting
  where
    -- an accent, waiting
    waiting = case span isSymbolNameChar (drop 1 text) of
      (n, _ : _) | Just m <- accentNamed n -> Just (['◌', m] ++ " \\" ++ n ++ ", on what comes next", Nothing)
      _ -> Nothing
    names prefix = case symbolsStartingWith prefix of
      [] -> ("no symbol named \\" ++ prefix, Nothing)
      matches ->
        let shownMatches = take maxShown matches
            entries = [ sym ++ " \\" ++ n | (n, sym) <- shownMatches ]
            line = joinWith "    " entries ++ (if length matches > maxShown then "    …" else "")
            exact = case zip shownMatches entries of
              ((n, _), entry) : _ | n == prefix -> Just (0, length entry)
              _ -> Nothing
        in (line, exact)
    maxShown = 8
    joinWith sep = foldr1 (\a b -> a ++ sep ++ b)

-- | The name being typed at the end of some text: the letters after its
-- last backslash, if that's where it ends.
typedName :: String -> Maybe String
typedName text = case break (== '\\') (reverse text) of
  (backwards, _ : _) | all isSymbolNameChar backwards -> Just (reverse backwards)
  _ -> Nothing

-- | What Tab does with a name being typed ('completeSymbolName').
data Completion
  = Finished String
    -- ^ put this in place of the backslash and the name: its symbol,
    -- or for an accent, its whole name and a space (so that what comes
    -- next is what it goes on)
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
  matches@((n, _) : _)
    | n == prefix -> finished n
    | [(only, _)] <- matches -> finished only
    | otherwise ->
        let common = foldr1 commonPrefix (map fst matches)
        in if length common > length prefix then Just (Longer common) else Nothing
  where
    finished n = Finished <$> case Map.lookup n nameTable of
      Just (Symbol s) -> Just s
      Just (Accent _) -> Just ("\\" ++ n ++ " ")
      Nothing -> Nothing
    commonPrefix (a : as) (b : bs) | a == b = a : commonPrefix as bs
    commonPrefix _ _ = []

-- | Turn every name in some text into its symbol, and every accent into
-- its mark on what it goes on, as if it had been typed (for text that's
-- pasted). A backslash right after a backslash keeps the name after it
-- as it is (@\\\\theta@).
replaceSymbolNames :: String -> String
replaceSymbolNames = \case
  s@('\\' : rest) -> case escapeAt True s of
    Complete new n -> new ++ replaceSymbolNames (drop n s)
    Literal -> '\\' : '\\' : replaceSymbolNames (drop 1 rest)
    _ -> '\\' : replaceSymbolNames rest
  c : rest -> c : replaceSymbolNames rest
  [] -> []
