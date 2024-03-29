{-# language AllowAmbiguousTypes, UndecidableInstances #-}
module UI
  ( type UI
  , type UIMonad
  , ToUI(..)
  , UIActor(..)
  ) where

import Event
import Language.Environment
import Language.Value.Evaluator

import Control.Applicative ((<|>))
import Data.Kind

-- | 'UI' is meant to be used as an open kind, so that
-- type-level tags for UIs are given by types of kind 'UI'.
--
-- > data WX :: UI
-- > data Gloss :: UI
-- > data DearImgui :: UI
--
-- The silliness about 'UL_' and @UL_ -> Type@ is a hack to
-- deal with the fact that kinds for data type declarations have
-- to end in Type; you aren't allowed to have a non-Type return
-- type for a data type declaration.
data UI_
type UI = UI_ -> Type

-- | The monad in which UI execution occurs. This may be
-- as simple as
--
-- > type instance UIMonad MyUI = IO
--
-- or
--
-- > type instance UIMonad MyUI = StateT MyState IO
type family UIMonad (ui :: UI) :: Type -> Type

class ToUI (ui :: UI) component where
  type CreationContext ui component :: Type
  type UIObject ui component :: Type
  toUI :: component
       -> CreationContext ui component
       -> UIMonad ui (UIObject ui component)

data UIActor (ui :: UI) =
  UIActor (forall args
            . Event args
           -> Maybe (Context HaskellTypeOfBinding args
                    -> UIMonad ui ()))

instance Monad (UIMonad ui) => Semigroup (UIActor ui) where
  UIActor f <> UIActor g = UIActor $ \event -> f event <|> g event

instance Monad (UIMonad ui) => Monoid (UIActor ui) where
  mempty = UIActor (const Nothing)
