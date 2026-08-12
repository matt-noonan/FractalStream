{-# language LambdaCase #-}
import Distribution.MacOSX
import Distribution.AppImage
import qualified Distribution.MacOSX as Mac
import qualified Distribution.AppImage as Linux
import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags(..))
import Distribution.PackageDescription hiding (updatePackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Program
import Distribution.Simple.Setup hiding (Flag)
import qualified System.Info as System
import Data.Either (partitionEithers)
import Data.List (isSuffixOf)

main :: IO ()
main = do
  defaultMainWithHooks $ simpleUserHooks
    { postBuild = \args buildFlags packageDesc localBuildInfo -> do
        appBundleBuildHook [macApp]   args buildFlags packageDesc localBuildInfo
        appImageBuildHook  [linuxApp] args buildFlags packageDesc localBuildInfo
    , confHook = fsConfHook
    }

linuxApp :: AppImage
linuxApp = AppImage
  { Linux.appName = "FractalStream"
  , Linux.appDesktop = "FractalStream.desktop"
  , Linux.appIcons = ["FS_64x64x32.png", "FS_128x128x32.png", "FS_256x256x32.png"]
  , Linux.appResources = []
  , Linux.appDirCustomize = Nothing
  }

macApp :: MacApp
macApp = MacApp "FractalStream"
  -- Icon file
  (Just "FS.icns")

  -- Info.plist
  (Just "../macos/Info.plist")

  -- Other resource files
  []

  -- Other binary files
  []

  -- Starting in Big Sur, MacOS caches certain
  -- system libraries. They act like they are present
  -- when using dlopen but do not actually exist on
  -- disk at the stated locations! This throws off
  -- `ChaseWithDefaults` when it uses otool -L to find
  -- dylib dependencies. We'll work around it by excluding
  -- /usr/lib, where these libraries claim to be installed.
  --
  -- See: https://developer.apple.com/forums/thread/655588
  --
  (ChaseWith $ defaultExclusions ++ ["/usr/lib"])

fsConfHook :: (GenericPackageDescription, HookedBuildInfo)
           -> ConfigFlags
           -> IO LocalBuildInfo
fsConfHook (pkg_descr, hooked_bi) flags = do
  -- Get wx-config --libs output
  wxConfig <- getWxConfig flags
  output0 <- words <$> wxConfig ["--libs"]
  (includes, cxxOpts) <- partitionEithers . map splitCxxOptions . words
                         <$> wxConfig ["--cxxflags"]
  let output = if System.os == "darwin"
               then output0 ++ ["-framework", "AppKit"]
               else output0
      (wxLibs0, wxLdDirsOrOpts) = partitionEithers (concatMap splitLdOptions output)
      (wxLdDirs0, wxLdOpts) = partitionEithers wxLdDirsOrOpts

  (wxLibs, wxLdDirs) <- case lookupFlagAssignment (mkFlagName "use-jemalloc")
                             $ configConfigurationsFlags flags of
    Just True -> do
      let verbosity = fromFlag $ configVerbosity flags
      program <- fst <$> requireProgram verbosity (simpleProgram "jemalloc-config")
                        (configPrograms flags)
      (words <$> getProgramOutput verbosity program ["--libdir"]) >>= \case
        [d] -> pure ("jemalloc" : wxLibs0, d : wxLdDirs0)
        []  -> error "jemalloc-config --libdir returned nothing"
        _   -> error "jemalloc-config --libdir returned multiple directories"

    _ -> pure (wxLibs0, wxLdDirs0)
  let pkg_descr' = updatePackageDescription wxLibs wxLdOpts wxLdDirs includes cxxOpts pkg_descr
  confHook simpleUserHooks (pkg_descr', hooked_bi) flags

splitLdOptions :: String -> [Either String (Either String String)]
splitLdOptions opt = case opt of
  '-' : 'l' : libname -> [Left libname]
  '-' : 'L' : dirname -> [Right (Left dirname)]
  -- This is a hack to work around a bug(?) in wx-config where some frameworks
  -- are given as a full path, which confuses the linker.
  _ | ".framework" `isSuffixOf` opt
                      -> [ Right (Right "-framework")
                         , Right . Right
                         . takeWhile (/= '.') . reverse
                         . takeWhile (/= '/') . reverse
                         $ opt
                         ]
    | otherwise       -> [Right (Right opt)]

splitCxxOptions :: String -> Either String String
splitCxxOptions opt = case opt of
  '-' : 'I' : include -> Left include
  _                   -> Right opt

getWxConfig :: ConfigFlags -> IO ([String] -> IO String)
getWxConfig confFlags = do
  let verbosity = fromFlag $ configVerbosity confFlags
  program <- fst <$> requireProgram verbosity (simpleProgram "wx-config-3.2")
                                   (configPrograms confFlags)
  return $ getProgramOutput verbosity program

updatePackageDescription :: [String]
                         -> [String]
                         -> [String]
                         -> [String]
                         -> [String]
                         -> GenericPackageDescription
                         -> GenericPackageDescription
updatePackageDescription wxLibs wxLdOpts wxLdDirs wxIncludes wxCxxOpts gpd =
    gpd { condExecutables = map updateExecutable (condExecutables gpd) }
  where
    updateExecutable (name, condTree) =
      (name, fmap (\exe -> exe {
                      buildInfo = (buildInfo exe)
                        { ldOptions = ldOptions (buildInfo exe) ++ wxLdOpts
                        , extraLibs = extraLibs (buildInfo exe) ++ wxLibs
                        , extraLibDirs = extraLibDirs (buildInfo exe) ++ wxLdDirs
                        , cxxOptions = cxxOptions (buildInfo exe) ++ wxCxxOpts
                        , ccOptions  = ccOptions (buildInfo exe) ++ wxCxxOpts
                        , includeDirs = includeDirs (buildInfo exe) ++ wxIncludes
                        }
                      }) condTree)
