{-# language AllowAmbiguousTypes #-}
module UI
  ( type UI
  , type UIMonad
  , ToUI(..)
  ) where

import Actor

-- | 'UI' is meant to be used as an open kind, so that
-- type-level tags for UIs are given by types of kind 'UI'.
--
-- > data WX :: UI
-- > data Gloss :: UI
-- > data DearImgui :: UI
--
-- The silliness about 'UL_' and @UL_ -> *@ is a hack to
-- deal with the fact that kinds for data type declarations have
-- to end in *; you aren't allowed to have a non-* return
-- type for a data type declaration.
data UI_
type UI = UI_ -> *

-- | The monad in which UI execution occurs. This may be
-- as simple as
--
-- > type instance UIMonad MyUI = IO
--
-- or
--
-- > type instance UIMonad MyUI = StateT MyState IO
type family UIMonad (ui :: UI) :: * -> *

class Actor actor => ToUI (ui :: UI) actor where
  type CreationContext ui actor :: *
  type UIObject ui actor :: *
  toUI :: actor
       -> CreationContext ui actor
       -> UIMonad ui (UIObject ui actor)
