module Language.Parser.SourceRange
  ( SourceRange(..)
  , Pos(..)
  , SourceSpan(..)
  , spanOfSourceRange
  , HasErrorLocation(..)
  , indicateError
  , Indicated(..)
  ) where

import FractalStream.Prelude

------------------------------------------------------
-- Source ranges
------------------------------------------------------

data SourceRange
  = SourceRange Pos Pos
  | NoSourceRange
  deriving (Eq, Show)

data Pos = Pos { posRow :: Int, posCol :: Int }
  deriving (Eq, Ord, Show)

instance Semigroup SourceRange where
  NoSourceRange <> r = r
  r <> NoSourceRange = r
  SourceRange s e <> SourceRange s' e' =
    SourceRange (min s s') (max e e')

instance Monoid SourceRange where
  mempty = NoSourceRange

data SourceSpan
  = InLine Int Int Int
  | InRows Int Int
  deriving (Eq, Show)

spanOfSourceRange :: SourceRange -> Maybe SourceSpan
spanOfSourceRange = \case
  NoSourceRange -> Nothing
  SourceRange (Pos r c) (Pos r' c')
    | r == r'   -> Just (InLine r c c')
    | otherwise -> Just (InRows r r')

class HasErrorLocation a where
  errorLocation :: a -> SourceRange

instance (HasErrorLocation a, HasErrorLocation b) => HasErrorLocation (Either a b) where
  errorLocation = either errorLocation errorLocation

instance HasErrorLocation a => HasErrorLocation (Maybe a) where
  errorLocation = maybe NoSourceRange errorLocation

indicateError :: SourceRange -> String -> [String]
indicateError sr input =
  case spanOfSourceRange sr of
    Nothing -> []
    Just (InLine i s e) -> case drop i inputLines of
      [] -> []
      (line:_) -> map ("  " ++) [line, replicate s ' ' ++ replicate (e + 1 - s) '^']
    Just (InRows s e) -> map ("| " ++) . take (e + 1 - s) . drop s $ inputLines
  where
    inputLines = lines input

data Indicated = Indicated SourceRange String

instance PrettyPrint Indicated where
  pp (Indicated sr i) = indicateError sr i
