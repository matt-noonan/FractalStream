-- | The syntax of scripts for the original FractalStream (2008-2009, for
-- Mac OS X 10.5), as described in its guides "Interacting with
-- FractalStream" and "A Guide To FractalStream". Scripts are a sequence
-- of sentence-like commands, each ending with a period:
--
-- > par set z to -c/2.
-- > iterate z^2 + c*z + 1 until z escapes.
-- > if im(z) > 0 then [Checkered | red].
--
-- "Language.Legacy.Parser" parses them, and "Language.Legacy.Translate"
-- turns them into a project for this FractalStream.
module Language.Legacy.Syntax
  ( Script(..)
  , Cmd(..)
  , ProbeFormat(..)
  , Expr(..)
  , BinOp(..)
  , Cond(..)
  , RelOp(..)
  , BoolOp(..)
  ) where

-- | A whole script.
data Script = Script
  { scriptIsReal :: Bool  -- ^ starts with @{real}@
  , scriptBody   :: [Cmd]
  } deriving (Eq, Show)

-- | A command. Names are as written: the old language ignores case, so
-- "Language.Legacy.Translate" decides which spelling to use.
data Cmd
  = Set [String] Expr             -- ^ @set v to e.@, or @set (x, y) to (a, b).@
  | Default String Expr           -- ^ @default v to e.@: an editable parameter
  | If Cond Cmd (Maybe Cmd)       -- ^ @if c then cmd.@, optionally @else cmd.@
  | Block [Cmd]                   -- ^ @block ... end.@ or @: ... end.@
  | DoUntil [Cmd] Cond            -- ^ @do ... until c.@
  | Iterate Expr (Maybe String) Cond  -- ^ @iterate e [on v] until c.@
  | Repeat Expr Cmd               -- ^ @repeat n times cmd@
  | Reduce String Expr            -- ^ @reduce v mod e.@
  | Report Expr                   -- ^ @report e.@
  | Succeed                       -- ^ @succeed.@
  | Fail                          -- ^ @fail.@
  | Flag String (Maybe String)    -- ^ @[name]@ or @[name | color]@
  | Par Cmd                       -- ^ @par cmd@: only in the parameter plane
  | Dyn Cmd                       -- ^ @dyn cmd@: only in the dynamical plane
  | Probe ProbeFormat String Cmd  -- ^ @probe [format] "Name" cmd@
  | Unsupported String String
    -- ^ A command this FractalStream has no counterpart for (@load@ of a
    -- special tool, @using each ... in@ a data source): what it is, and
    -- its source text
  | Stray String String
    -- ^ An @end.@ that ends no block, or an @until c.@ with no @do@
    -- before it: which (@"end."@ or @"until"@), and its source text
  | Comment String                -- ^ @(a comment)@, where a command could start
  deriving (Eq, Show)

data ProbeFormat = ProbeComplex | ProbeReal | ProbeInteger | ProbeRational
  deriving (Eq, Show)

data Expr
  = Num String        -- ^ a real number, as written (@17@, @1.25@)
  | Imag String       -- ^ an imaginary number, as written without the @i@ (@1.2@ for @1.2i@)
  | Var String
  | Pi
  | Neg Expr
  | Bin BinOp Expr Expr
  | Call String Expr  -- ^ @f(x)@
  | Pair Expr Expr    -- ^ @(x, y)@: a 2-vector, which the old language treats as x + y i
  | Paren Expr        -- ^ parentheses the script wrote, kept for readability
  deriving (Eq, Show)

data BinOp = Add | Sub | Mul | Div | Pow
  deriving (Eq, Show)

data Cond
  = Rel RelOp Expr Expr
  | Escapes Expr
  | Vanishes Expr
  | Stops Expr
  | BoolOp BoolOp Cond Cond
  | ParenCond Cond
  deriving (Eq, Show)

data RelOp = Equal | Less | LessEq | Greater | GreaterEq
  deriving (Eq, Show)

data BoolOp = And | Or | Xor
  deriving (Eq, Show)
