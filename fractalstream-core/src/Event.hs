module Event
  ( Event(..)
  , type ResizeEnv
  , type PosEnv
  , type PinchEnv
  , type DragEnv
  , type ButtonEnv
  , resizeEnv
  , posEnv
  , pinchEnv
  , dragEnv
  , buttonEnv
  ) where

import Language.Environment
import Language.Type
import Data.Proxy

data Event (args :: Environment) where
  Refresh   :: Event '[]
  Resize    :: Event ResizeEnv
  Timer     :: Event '[]
  Click     :: Event PosEnv
  MouseDown :: Event PosEnv
  MouseUp   :: Event PosEnv
  Motion    :: Event PosEnv
  Pinch     :: Event PinchEnv
  Drag      :: Event DragEnv
  Button    :: Event ButtonEnv
  Update    :: Event '[]

deriving instance Eq   (Event args)
deriving instance Ord  (Event args)
deriving instance Show (Event args)

type ResizeEnv = '[ '("viewWidth", 'IntegerT)
                  , '("viewHeight", 'IntegerT) ]
type PosEnv = '[ '("posX", 'RealT)
               , '("posY", 'RealT) ]
type PinchEnv = '[ '("posX", 'RealT)
                 , '("posY", 'RealT)
                 , '("posZ", 'RealT) ]
type DragEnv = '[ '("posX", 'RealT)
                , '("posY", 'RealT)
                , '("oldX", 'RealT)
                , '("oldY", 'RealT) ]
type ButtonEnv = '[ '("button", 'IntegerT) ]

resizeEnv :: EnvironmentProxy ResizeEnv
resizeEnv = BindingProxy (Proxy @"viewWidth")  IntegerType
          $ BindingProxy (Proxy @"viewHeight") IntegerType
          $ EmptyEnvProxy

posEnv :: EnvironmentProxy PosEnv
posEnv = BindingProxy (Proxy @"posX") RealType
       $ BindingProxy (Proxy @"posY") RealType
       $ EmptyEnvProxy

pinchEnv :: EnvironmentProxy PinchEnv
pinchEnv = BindingProxy (Proxy @"posX") RealType
         $ BindingProxy (Proxy @"posY") RealType
         $ BindingProxy (Proxy @"posZ") RealType
         $ EmptyEnvProxy

dragEnv :: EnvironmentProxy DragEnv
dragEnv = BindingProxy (Proxy @"posX") RealType
        $ BindingProxy (Proxy @"posY") RealType
        $ BindingProxy (Proxy @"oldX") RealType
        $ BindingProxy (Proxy @"oldY") RealType
        $ EmptyEnvProxy

buttonEnv :: EnvironmentProxy ButtonEnv
buttonEnv = BindingProxy (Proxy @"button") IntegerType
          $ EmptyEnvProxy
