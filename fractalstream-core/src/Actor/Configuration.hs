{-# language OverloadedStrings #-}
module Actor.Configuration
  ( Configuration(..)
  , withConfigurationEnv
  , getConfigurationSplices
  ) where

import Actor.Layout
import Language.Type
import Language.Environment
import Language.Value.Parser
import qualified Data.Map as Map
import Data.Codec

import Data.Aeson

data Configuration f = Configuration
  { coTitle :: String
  , coSize :: Dimensions
  , coContents :: Layout f
  }
  deriving Show

instance CodecWith ctx (Layout f) => CodecWith ctx (Configuration f) where
  codecWith_ ctx = do
    title <-coTitle-< key "title"
    size  <-coSize-< key "size"
    body  <-coContents-< codecWith ctx
    build Configuration title size body

withConfigurationEnv :: forall m t env
                           . MonadFail m
                          => Maybe Configuration
                          -> EnvironmentProxy env
                          -> (forall env'. EnvironmentProxy env' -> m t)
                          -> m t
withConfigurationEnv Nothing env0 k = k env0
withConfigurationEnv (Just Configuration{..}) env0 k
   = go (allBindings coContents) env0
 where
   go :: forall e. [(String, SomeType)] -> EnvironmentProxy e -> m t
   go [] env = k env
   go ((nameStr, SomeType ty) : etc) env =
     bindInEnv nameStr ty env (go etc)

getConfigurationSplices :: forall m. MonadFail m
                        => Configuration
                        -> m Splices
getConfigurationSplices Configuration{..}
    = (>>= traverse theOnly)
    . fmap (Map.fromListWith (<>) . map (\(k,v) -> (k, [v])))
    . mapM getSplice
    $ allBindingVars coContents
  where
    theOnly :: [a] -> m a
    theOnly = \case
      [x] -> pure x
      _   -> fail ("duplicate splices")

    getSplice :: ConfigVar -> m (String, ParsedValue)
    getSplice (ConfigVar valStr (SomeType _ty) _envMap name) = do
      case parseParsedValue Map.empty valStr of
        Left e -> fail (ppFullError e valStr)
        Right v -> pure (name, v)
