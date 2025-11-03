module Language.Parser.Tokenizer
  ( Token(..)
  , SRToken(..)
  , tokenize
  , tokenizeWithIndentation
  , commentRanges
  ) where

import FractalStream.Prelude

import qualified Data.Map as Map
import Data.Char
import Data.Ord

import Language.Parser.SourceRange

------------------------------------------------------
-- Token types
------------------------------------------------------

data Token
  = NumberI Integer
  | NumberF Double
  | Plus
  | Minus
  | Times
  | Divide
  | IntegerDivide
  | Caret
  | Bar
  | Identifier String
  | Junk Char
  | OpenParen
  | CloseParen
  | OpenBracket
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma
  | Colon
  | Semicolon
  | Equal
  | ExactlyEqual
  | NotEqual
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual
  | Indent
  | Dedent
  | Newline
  | If
  | Then
  | Else
  | Euler
  | Pi
  | I
  | True_
  | False_
  | Or_
  | And_
  | Not_
  | LeftArrow
  | RightArrow
  | Escapes
  | Vanishes
  | Iterations
  | TimesKeyword
  | Stuck
  | Quoted String
  | TextKeyword
  | AppendKeyword
  | PrependKeyword
  | JoinKeyword
  | RemoveKeyword
  | FindKeyword
  | TransformKeyword
  | RangeKeyword
  | At
  | AtAt
  | LengthKeyword
  | OpenSplice
  | CloseSplice
  deriving (Eq, Ord, Show)

instance IsString Token where fromString = Identifier

data SRToken = SRToken
  { baseToken :: Token
  , tokenSourceRange :: SourceRange
  }
  deriving (Show)

-- | CAUTION! Sketchy Eq instance ahead!
instance Eq SRToken where
  t1 == t2 = baseToken t1 == baseToken t2


------------------------------------------------------
-- Tokenization
------------------------------------------------------

tokenize :: String -> [SRToken]
tokenize = tokenize' .
  zipWith (\i c -> let p = Pos 0 i in (c, SourceRange p p)) [0..]

commentRanges :: String -> [(Int, Int)]
commentRanges = comments . zip [0..]
  where
    comments :: [(Int, Char)] -> [(Int, Int)]
    comments = \case
      ((i, '#') : etc) ->
        let comment = takeWhile ((/= '\n') . snd) etc
        in (i, i + length comment) : comments (dropWhile ((/= '\n') . snd) etc)
      (_ : etc) -> comments etc
      [] -> []

tokenizeWithIndentation :: String -> [SRToken]
tokenizeWithIndentation
         = ([SRToken Indent NoSourceRange] ++)
         . (++ [SRToken Dedent NoSourceRange])
         . concatMap toTok
         . block
         . map observeSpaces
         . joinLineContinuations
         . filter (not . all (isSpace . fst))
         . map dropComments
         . map (\(r, xs) -> map (\(c, x) -> let p = Pos r c
                                            in (x, SourceRange p p)) xs)
         . zip [0..]
         . map (zip [0..])
         . lines
  where
    dropComments :: SRString -> SRString
    dropComments = \case
      []      -> []
      (q0@('"',_):etc) -> let (quoted, cs) = skipString etc
                       in (q0 : quoted) ++ dropComments cs
      (('#',_):_) -> []
      (c:cs)  -> c : dropComments cs

    observeSpaces :: SRString -> (Int, SRString)
    observeSpaces s = (length (takeWhile (isSpace . fst) s)
                      , dropWhile (isSpace . fst) s)

    -- Dedent all lines by n spaces
    extract :: Int -> [(Int, SRString)] -> ([TokenGroup], [(Int, SRString)])
    extract n ss =
      let ss'  = takeWhile ((>= n) . fst) ss
          ss'' = dropWhile ((>= n) . fst) ss
      in (block [(m - n, x) | (m,x) <- ss'], ss'')

    block :: [(Int, SRString)] -> [TokenGroup]
    block = \case
      [] -> []
      ((n,s) : ss)
        | n == 0 -> Single (tokenize' s) : block ss
        | n > 0 ->
            let (is, ss') = extract n ss
            in Group (tokenize' s) is : block ss'
      _ -> error "bad indent"

skipString :: SRString -> (SRString, SRString)
skipString = go []
  where go acc = \case
          (c1@('\\',_) : c2 : etc) -> go (c2 : c1 : acc) etc
          (q@('"',_) : etc) -> (reverse (q : acc), etc)
          (c : etc) -> go (c : acc) etc
          [] -> (reverse acc, [])


------------------------------------------------------
-- Internals
------------------------------------------------------

identChar :: Char -> Bool
identChar c = (isAlphaNum c && not (c `elem` superscripts) && (c /= '⁻'))
              || c == '_'

opTokens :: [(String, Token)]
opTokens = sortOn (\x -> (Down (length (fst x)), x)) $
  [ ("+", Plus), ("-", Minus), ("*", Times)
  , ("//", IntegerDivide), ("/", Divide)
  , ("^", Caret), ("|", Bar), ("(", OpenParen), (")", CloseParen)
  , ("[", OpenBracket), ("]", CloseBracket)
  , ("{", OpenBrace), ("}", CloseBrace)
  , (",", Comma), (":", Colon), (";", Semicolon)
  , (">", GreaterThan), ("<", LessThan)
  , ("===", ExactlyEqual), ("≡", ExactlyEqual)
  , ("==", Equal), ("=", Equal)
  , (">=", GreaterThanOrEqual), ("<=", LessThanOrEqual)
  , ("≥", GreaterThanOrEqual), ("≤", LessThanOrEqual)
  , ("!=", NotEqual), ("=/=", NotEqual), ("≠", NotEqual)
  , ("->", RightArrow), ("<-", LeftArrow)
  , ("⭢", RightArrow), ("⭠", LeftArrow)
  , ("→", RightArrow), ("←", LeftArrow)
  , ("⟼", RightArrow), ("↦", RightArrow)
  , ("@", At), ("@@", AtAt)
  , ("<<", OpenSplice), (">>", CloseSplice)
  ]

longestMatchingOperator :: SRString -> Maybe (SRToken, SRString)
longestMatchingOperator cs =
  listToMaybe [ ( SRToken t (mconcat $ map snd $ take (length s) cs)
                , drop (length s) cs)
              | (s, t) <- opTokens, s `isPrefixOf` (map fst cs) ]

wordlikeTokens :: Map String Token
wordlikeTokens = Map.fromList
  [ ("if", If), ("then", Then), ("else", Else)
  , ("e", Euler), ("𝑒", Euler)
  , ("pi", Pi), ("π", Pi)
  , ("i", I), ("𝑖", I)
  , ("true", True_), ("false", False_)
  , ("or", Or_), ("and", And_), ("not", Not_)
  , ("escapes", Escapes), ("escaped", Escapes)
  , ("vanishes", Vanishes), ("vanished", Vanishes)
  , ("iterations", Iterations), ("times", TimesKeyword)
  , ("stuck", Stuck)
  , ("text", TextKeyword)
  , ("append", AppendKeyword), ("prepend", PrependKeyword)
  , ("join", JoinKeyword), ("remove", RemoveKeyword)
  , ("find", FindKeyword), ("transform", TransformKeyword)
  , ("range", RangeKeyword), ("length", LengthKeyword)
  ]

data TokenGroup
  = Group [SRToken] [TokenGroup]
  | Single [SRToken]
  deriving (Eq, Show)

toTok :: TokenGroup -> [SRToken]
toTok (Single s) = s ++ [SRToken Newline NoSourceRange]
toTok (Group s xs) = [SRToken Indent NoSourceRange] ++ s ++
                     [SRToken Newline NoSourceRange] ++
                     concatMap toTok xs ++ [SRToken Dedent NoSourceRange]

tokenize' :: SRString -> [SRToken]
tokenize' = \case
  -- Done!
  [] -> []

  -- Skip whitespace
  (c:cs) | isSpace (fst c) -> tokenize' cs

  -- Skip comments
  (('#',_):_) -> []

  -- Tokenize quotes
  (('"', srq) : etc) ->
    let (body, etc') = skipString etc
        txt = map fst $ take (length body - 1) body
        sr = mconcat (srq : map snd body)
    in SRToken (Quoted txt) sr : tokenize' etc'

  -- Tokenize positive numbers
  cs@(d:_)
    | isDigit (fst d) ->
        let ds = takeWhile (isDigit . fst) cs
        in case dropWhile (isDigit . fst) cs of
          (('.',sr) : cs'@(d' : _))
            | isDigit (fst d') ->
                let ds' = ds <> [('.', sr)] <> takeWhile (isDigit . fst) cs'
                in SRToken (NumberF (read $ map fst ds')) (mconcat $ map snd ds')
                   : tokenize' (dropWhile (isDigit . fst) cs')
          cs' -> SRToken (NumberI (read $ map fst ds)) (mconcat $ map snd ds)
                 : tokenize' cs'

  -- Tokenize the ⁻, ⁰, ¹, ², ³, ⁴, ⁵, ⁶, ⁷, ⁸, ⁹ superscripts
  (('⁻',sr):cs@(d:_))
    | fst d `elem` superscripts ->
        let ds  = takeWhile ((`elem` superscripts) . fst) cs
            cs' = dropWhile ((`elem` superscripts) . fst) cs
         in SRToken Caret NoSourceRange
            : SRToken (NumberI (negate $ superscriptNumber (map fst ds)))
                    (sr <> mconcat (map snd ds))
            : tokenize' cs'
  cs@(d:_)
    | fst d `elem` superscripts ->
        let ds  = takeWhile ((`elem` superscripts) . fst) cs
            cs' = dropWhile ((`elem` superscripts) . fst) cs
         in SRToken Caret NoSourceRange
            : SRToken (NumberI (superscriptNumber (map fst ds)))
                    (mconcat (map snd ds))
            : tokenize' cs'

  -- Tokenize special operators
  cs | Just (tok, cs') <- longestMatchingOperator cs
       -> tok : tokenize' cs'

  -- Tokenize identifiers
  cs@(c:_) | isAlpha (fst c) && not (fst c `elem` superscripts) && (fst c /= '⁻') ->
               let ds = takeWhile (identChar . fst) cs
                   cs' = dropWhile (identChar . fst) cs
                   tok = case Map.lookup (map fst ds) wordlikeTokens of
                     Just t  -> t
                     Nothing -> Identifier (map fst ds)
               in SRToken tok (mconcat (map snd ds)) : tokenize' cs'

  -- Otherwise, grab a junk character
  ((c, sr):cs) -> SRToken (Junk c) sr : tokenize' cs


superscripts :: String
superscripts = "⁰¹²³⁴⁵⁶⁷⁸⁹"

superscriptNumber :: String -> Integer
superscriptNumber = go 0
  where
    go acc = let next i = go (10 * acc + i)
             in \case
      [] -> acc
      ('⁰':xs) -> next 0 xs
      ('¹':xs) -> next 1 xs
      ('²':xs) -> next 2 xs
      ('³':xs) -> next 3 xs
      ('⁴':xs) -> next 4 xs
      ('⁵':xs) -> next 5 xs
      ('⁶':xs) -> next 6 xs
      ('⁷':xs) -> next 7 xs
      ('⁸':xs) -> next 8 xs
      ('⁹':xs) -> next 9 xs
      _ -> error "should be impossible"

type SRString = [(Char, SourceRange)]

-- Handle ... / … line continuation markers
joinLineContinuations :: [SRString] -> [SRString]
joinLineContinuations = \case
  [] -> []
  (x:xs) -> case hasLineContinuation x of
    Nothing -> x : joinLineContinuations xs
    Just x' -> case joinLineContinuations xs of
      (y:ys) -> (x' ++ y) : ys
      []     -> [x] -- leave the hanging ellipsis so we get a parse error later

hasLineContinuation :: SRString -> Maybe SRString
hasLineContinuation line = case dropWhile (isSpace . fst) (reverse line) of
  ('.',_):('.',_):('.',_):line' -> Just (reverse line')
  ('…',_):line' -> Just (reverse line')
  _ -> Nothing
