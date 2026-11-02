-- | Run before every test (hspec-discover finds this module by name).
module SpecHook (hook) where

import FractalStream.Console (transliterateConsole)
import Test.Hspec

-- | Tests print messages that quote names with typographic quotes, which
-- would throw on a console that can't show them (see
-- "FractalStream.Console").
hook :: Spec -> Spec
hook = beforeAll_ transliterateConsole
