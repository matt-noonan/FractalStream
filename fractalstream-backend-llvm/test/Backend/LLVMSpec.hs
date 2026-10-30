module Backend.LLVMSpec (spec) where

import Test.Hspec

import Language.Value
import Language.Draw
import Language.Code.Parser (parseCode, noCommonDefs)
import Actor.Event (EventArgument_, mutableVar, interpretedToolExec, ToolExec)
import Data.DynamicValue
import Data.Color
import Data.IORef
import Data.List (isInfixOf)
import Control.Exception (IOException, ErrorCall(..))
import Actor.Viewer (ViewerFunction(..), ViewerArgs(..), ViewerIssues(..))
import Actor.Viewer.Types (ViewerEnv)
import Foreign (allocaBytes, peekArray, Word8)
import Backend.LLVM

-- Deliberately plain string literals here, not a 'Text.RawString.QQ'
-- quasiquote: quasiquotes require Template Haskell, which forces GHC to
-- load this module's own transitive dependencies -- including
-- fractalstream-backend-llvm itself, and so llvm-hs's statically-linked,
-- every-target LLVM archive -- through its runtime (bytecode) linker at
-- compile time. That linker doesn't fold the duplicate weak symbols
-- LLVM's per-target code generators share, so it fails outright; a
-- normal compiled test executable never hits this since it links via
-- the system linker instead. See CLAUDE_NOTES.md.

spec :: Spec
spec = do

  describe "when compiling programs to LLVM" $ do

    it "generates the expected code for simple programs" $ do
      let env = declare @"x" RealType
              $ declare @"y" RealType
              $ declare @"color" ColorType
              $ declare @"[internal] iteration count" IntegerType
              $ declare @"[internal] iteration limit" IntegerType
              $ declare @"[internal] stuck" BooleanType
              $ declare @"[internal] escape radius" RealType
              $ declare @"[internal] vanishing radius" RealType
              $ endOfDecls
      let prog1 = "\nwhile x < y up to 1000 times:\n  x <- x + 1\ncolor <- rgb(y / x, 0, 0)\n"
      withCompiledCode (Proxy @"color") ColorType env prog1 $ \f -> do
        f1 <- invoke' env ColorType f 1.5 5 black 0 0 False 0 0
        f2 <- invoke' env ColorType f 1.5 2 black 0 0 False 0 0
        f1 `shouldBe` (rgbToColor (round (255 * 5 / 5.5 :: Double), 0, 0))
        f2 `shouldBe` (rgbToColor (round (255 * 2 / 2.5 :: Double), 0, 0))

    it "can compile a basic Mandelbrot set program" $ do
      let env = declare @"x" RealType
              $ declare @"y" RealType
              $ declare @"maxRadius" RealType
              $ declare @"maxIter" IntegerType
              $ declare @"color" ColorType
              $ declare @"[internal] iteration count" IntegerType
              $ declare @"[internal] iteration limit" IntegerType
              $ declare @"[internal] stuck" BooleanType
              $ declare @"[internal] escape radius" RealType
              $ declare @"[internal] vanishing radius" RealType
              $ endOfDecls
      let mandel = "\nC : C <- x + i y\nz : C <- 0\nk : Z <- 0\n\
                   \while |z|^2 < maxRadius^2 and k < maxIter up to 1000 times:\n\
                   \  z <- z^2 + C\n\
                   \  k <- k + 1\n\
                   \if k = maxIter:\n\
                   \  color <- blue\n\
                   \else:\n\
                   \  color <- red"

      withCompiledCode (Proxy @"color") ColorType env mandel $ \f -> do
        v1 <- invoke' env ColorType f 1 1 10 100 black 0 0 False 0 0
        v1 `shouldBe` red
        v2 <- invoke' env ColorType f (-1) 0.1 10 100 black 0 0 False 0 0
        v2 `shouldBe` blue
        v3 <- invoke' env ColorType f (-0.123) 0.745 10 100 black 0 0 False 0 0
        v3 `shouldBe` blue

  describe "when compiling tool handlers that use text" $ do

    let scripts =
          [ ("a literal", "write \"hello\" at 1 + 2i")
          , ("numbers of each type",
             "write n at 0\nwrite x at 0\nwrite z at 0\nwrite 0.1 + x at 0")
          , ("concatenation", "write text(\"z = \", z, \", n = \", n) at 0.5")
          , ("non-ASCII text, set back on the host", "msg <- text(msg, \"★₁\", \"γ\")")
          , ("text built up in a loop",
             "repeat up to 3 times:\n  msg <- text(msg, \" A\")\n  n <- n + 1\nwhile true\nwrite msg at 0")
          , ("if/then/else", "write (if n > 2 then \"big\" else \"small\") at 0")
          , ("the empty string", "msg <- \"\"\nwrite text(msg, msg) at 0")
          , ("a local text variable", "t := text(\"k=\", n)\nwrite t at 0\nmsg <- t")
          ]

    mapM_ (\(name, src) ->
            it ("matches the interpreter: " ++ name) $ withJIT $ \jit -> do
              compiled    <- runTool (compiledToolExec jit) src
              interpreted <- runTool interpretedToolExec src
              compiled `shouldBe` interpreted)
          scripts

  describe "when compiling tool handlers that use lists" $ do

    let scripts =
          [ ("appending to a list", "pts <- append(pts, z)")
          , ("transforming a list", "pts <- transform(pts, p ⟼ p^2 + z)")
          , ("removing everything", "pts <- remove(pts, p ⟼ true)")
          , ("a list of lists, like spider's legs",
             "legs <- append(legs, [z, 2z])\nlegs <- prepend(legs, pts)\n"
             ++ "legs <- transform(legs, leg ⟼ append(leg, 0))")
          , ("drawing from a list of lists",
             "for each leg in legs:\n  for each p in leg:\n    draw point at p")
          , ("a list of text", "words <- append(words, text(\"n=\", n))\n"
             ++ "for each w in words:\n  write w at 0")
          , ("leaving lists alone", "write length(legs) at 0")
            -- 600 appends copy ~4 MB of list nodes, overflowing the first
            -- arenas; draw commands must still happen exactly once
          , ("outgrowing the first arenas",
             "draw point at z\nks := range(1, 600)\nfor each k in ks:\n  pts <- append(pts, z)\n"
             ++ "write length(pts) at 0")
          ]

    mapM_ (\(name, src) ->
            it ("matches the interpreter: " ++ name) $ withJIT $ \jit -> do
              compiled    <- runTool (compiledToolExec jit) src
              interpreted <- runTool interpretedToolExec src
              compiled `shouldBe` interpreted)
          scripts

  describe "when a compiled tool handler fails" $ do

    it "reports a bad list index as such, like the interpreter" $ withJIT $ \jit -> do
      let src = "draw point at z\nx <- re(pts @ 5)"
      runTool (compiledToolExec jit) src
        `shouldThrow` (\e -> "list index was out of range" `isInfixOf` show (e :: IOException))
      runTool interpretedToolExec src
        `shouldThrow` (\(ErrorCall msg) -> msg == "List index 5 is out of range")

    it "retries after an overflow, even if it also caused a bad index" $ withJIT $ \jit -> do
      -- the first arena overflows while building ks, leaving it (and so
      -- big) cut short, so `big @ 50000` is out of range in that run
      let src = "ks := range(1, 100000)\nbig := transform(ks, k ⟼ z)\nx <- re(big @ 50000)"
      compiled    <- runTool (compiledToolExec jit) src
      interpreted <- runTool interpretedToolExec src
      compiled `shouldBe` interpreted

  describe "when rendering a viewer" $ do

    let -- Render a 4x4 block of a script that's fine for n = 0, indexes a
        -- list out of range for n = 1, and runs out of its 1 MB of
        -- scratch memory for n = 2 (a 100000-element list takes 1.6 MB).
        render :: LLVMJit -> Int64 -> IO (ViewerIssues, [Word8])
        render jit n = do
          let env = envProxy (Proxy @(ViewerEnv '[ '("n", 'IntegerT) ]))
              src = "color <- blue\nxs := [1, 2, 3]\n"
                 ++ "if n = 1:\n  k := xs @ 5\n  color <- if k > 0 then red else green\n"
                 ++ "if n = 2:\n  ks := range(1, 100000)\n"
                 ++ "  color <- if length(ks) > 0 then red else green"
          code <- either (fail . show) pure (parseCode env noCommonDefs src)
          withJittedViewer jit Nothing code $ \(ViewerFunction vf) ->
            allocaBytes (3 * 4 * 4) $ \buf -> do
              issues <- vf ViewerArgs
                { vaPoint = (0, 0), vaStep = (0.1, 0.1)
                , vaWidth = 4, vaHeight = 4, vaSubsamples = 1
                , vaBuffer = buf
                , vaArgs = Bind (Proxy @"n") IntegerType n EmptyContext }
              (issues,) <$> peekArray (3 * 4 * 4) buf
        allPixels (r, g, b) = concat (replicate 16 [r, g, b])

    it "reports no issues for a script without problems" $ withJIT $ \jit -> do
      (issues, pixels) <- render jit 0
      issues `shouldBe` mempty
      pixels `shouldBe` allPixels (colorToRGB blue)

    it "reports a list indexed out of range, drawn in magenta" $ withJIT $ \jit -> do
      (issues, pixels) <- render jit 1
      issues `shouldBe` ViewerIssues { viOutOfMemory = False, viBadIndex = True }
      pixels `shouldBe` allPixels (255, 0, 255)

    it "reports running out of scratch memory, drawn in magenta" $ withJIT $ \jit -> do
      (issues, pixels) <- render jit 2
      issues `shouldBe` ViewerIssues { viOutOfMemory = True, viBadIndex = False }
      pixels `shouldBe` allPixels (255, 0, 255)

type ToolTestEnv =
  '[ '("msg", 'TextT), '("n", 'IntegerT), '("x", 'RealT), '("z", 'ComplexT)
   , '("pts", 'ListT 'ComplexT), '("legs", 'ListT ('ListT 'ComplexT))
   , '("words", 'ListT 'TextT)
   , '("[internal] iteration count", 'IntegerT)
   , '("[internal] iteration limit", 'IntegerT)
   , '("[internal] stuck", 'BooleanT) ]

-- | Run a tool handler with the given executor, returning everything it drew
-- and the final values of the variables it can set.
runTool :: (DrawSink -> ToolExec) -> String
        -> IO ( [String]
              , (String, Int64, Double)
              , ([Complex Double], [[Complex Double]], [String]) )
runTool exec src = do
  vMsg <- newVariable "start"
  vN   <- newVariable (2 :: Int64)
  vX   <- newVariable (0.01 :: Double)
  vZ   <- newVariable (1 :+ 0.5 :: Complex Double)
  vPts <- newVariable [1 :+ 1, 0 :+ (-2) :: Complex Double]
  vLegs <- newVariable [[0 :+ 0, 1 :+ 0], [], [0 :+ 1 :: Complex Double]]
  vWords <- newVariable ["one", "★"]
  vI   <- newVariable (0 :: Int64)
  vL   <- newVariable (100 :: Int64)
  vS   <- newVariable False
  let ctx :: Context EventArgument_ ToolTestEnv
      ctx = Bind (Proxy @"msg") TextType (mutableVar vMsg)
          $ Bind (Proxy @"n") IntegerType (mutableVar vN)
          $ Bind (Proxy @"x") RealType (mutableVar vX)
          $ Bind (Proxy @"z") ComplexType (mutableVar vZ)
          $ Bind (Proxy @"pts") (ListType ComplexType) (mutableVar vPts)
          $ Bind (Proxy @"legs") (ListType (ListType ComplexType)) (mutableVar vLegs)
          $ Bind (Proxy @"words") (ListType TextType) (mutableVar vWords)
          $ Bind (Proxy @"[internal] iteration count") IntegerType (mutableVar vI)
          $ Bind (Proxy @"[internal] iteration limit") IntegerType (mutableVar vL)
          $ Bind (Proxy @"[internal] stuck") BooleanType (mutableVar vS)
          $ EmptyContext
      env = contextToEnv ctx
  code <- either (fail . show) pure (parseCode env noCommonDefs src)
  events <- newIORef []
  let record e = modifyIORef events (e :)
      sink = DrawSink
        { dsClear  = record "clear"
        , dsStroke = \c -> record ("stroke " ++ show c)
        , dsFill   = \c -> record ("fill " ++ show c)
        , dsPoint  = \p -> record ("point " ++ show p)
        , dsLine   = \p q -> record ("line " ++ show (p, q))
        , dsCircle = \f r p -> record ("circle " ++ show (f, r, p))
        , dsRect   = \f p q -> record ("rect " ++ show (f, p, q))
        , dsWrite  = \t p -> record ("write " ++ show t ++ " at " ++ show p)
        }
  invoker <- exec sink env code
  invoker ctx
  drawn <- reverse <$> readIORef events
  final <- (,,) <$> getDynamic vMsg <*> getDynamic vN <*> getDynamic vX
  lists <- (,,) <$> getDynamic vPts <*> getDynamic vLegs <*> getDynamic vWords
  pure (drawn, final, lists)
