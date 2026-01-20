{-# language OverloadedStrings #-}
module Actor.Configuration
  ( Configuration(..)
  --, withConfigurationEnv
 --  , getConfigurationSplices
  ) where

import Actor.Layout
import Language.Value.Parser
import Data.Codec
import Data.DynamicValue

data Configuration = Configuration
  { coTitle    :: Parsed String
  , coSize     :: Variable Dimensions
  , coContents :: Layout
  }

instance CodecWith (Dynamic (Either String Splices)) Configuration where
  codecWith_ splices = do
    debugDump "Configuration"
    title <-coTitle-< mapped (key "title") $ \_ -> pure nonEmptyString
    size  <-coSize-< key "size"
    body  <-coContents-< codecWith splices
    build Configuration title size body

{-
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
-}
