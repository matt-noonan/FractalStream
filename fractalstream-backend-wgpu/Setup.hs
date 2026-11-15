-- | The usual build, but for where wgpu-native is: dev-tools/wgpu/fetch.sh
-- puts it in third_party/wgpu-native, at the top of the repository. Its
-- library folder must be given to cabal as an absolute path (ghc-pkg
-- won't register a relative one), and as a library folder, not in
-- ld-options (cabal checks that the library links, and GNU ld needs it
-- after this package's code), so it's added here, as --extra-lib-dirs
-- would, when the package is configured (in its own folder).
import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags(..))
import System.Directory (doesDirectoryExist, makeAbsolute)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook = \description flags -> do
      lib <- makeAbsolute "../third_party/wgpu-native/lib"
      found <- doesDirectoryExist lib
      confHook simpleUserHooks description
        flags { configExtraLibDirs = configExtraLibDirs flags ++ [ lib | found ] }
  }
