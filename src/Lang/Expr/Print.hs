
module Lang.Expr.Print
  ( prettyPrint
  , printTree
  , printATree
  , sexpr
  , sexpr'
  ) where

import Lang.Expr
import Lang.Numbers

import Data.List (intercalate)

instance Show Expr where show = prettyPrint

prettyPrint :: Expr -> String
prettyPrint = foldExpr phi
  where phi expr = case expr of
         Let s x a  -> "(let " ++ s ++ " = " ++ x ++ " in " ++ a ++ ")"
         Var s      -> s
         I          -> "i"
         Apply f x  -> f ++ "(" ++ x ++ ")"
         Lambda s e -> "(" ++ s ++ " |-> " ++ e ++ ")"
         Const (R r) -> show r
         Add xs     -> "(" ++ intercalate " + " xs ++ ")"
         Embed x    -> "complex(" ++ x ++ ")"
         Sub x y    -> "(" ++ x ++ " - " ++ y ++ ")"
         Mul xs     -> intercalate " " xs
         Div x y    -> x ++ " / " ++ y
         Pow x n    -> x ++ "^(" ++ n ++ ")"
         Neg x      -> "-" ++ x
         Conj x     -> "conj(" ++ x ++ ")"
         RealPart x -> "real(" ++ x ++ ")"
         ImagPart x -> "imag(" ++ x ++ ")"
         Norm x     -> "|" ++ x ++ "|"
         Norm2 x    -> "|" ++ x ++ "|^2"
         Diff s x   -> "d/d" ++ s ++ " " ++ x
         Pair x y   -> "(" ++ x ++ ", " ++ y ++ ")"
         Case cs x  -> "[case " ++ x ++ " of " ++ show cs ++ "]"
         Builtin f  -> show f

printTree :: (String -> [String] -> String) -> Expr -> String
printTree f = printATree (const f) . annotate (const ())

printATree :: (t -> String -> [String] -> String) -> AExpr t -> String
printATree tree = foldAExpr printTree'
  where printTree' t expr = case expr of
         Let s x a    -> tree t ("let{" ++ x ++ "}") [x,a]
         Var s        -> "{" ++ s ++ "}"
         I            -> "i"
         Apply f x    -> tree t "apply" [f,x]
         Lambda s e   -> tree t ("lambda{" ++ s ++ "}") [e]
         Const (R r)  -> show r
         Add xs       -> tree t "sum" xs
         Embed x      -> tree t "embed" [x]
         Sub x y      -> tree t "subtract" [x,y]
         Mul xs       -> tree t "product" xs
         Div x y      -> tree t "divide" [x,y]
         Pow x n      -> tree t "power" [x,n]
         Neg x        -> tree t "neg" [x]
         Conj x       -> tree t "bar" [x]
         RealPart x   -> tree t "real" [x]
         ImagPart x   -> tree t "imag" [x]
         Norm x       -> tree t "norm" [x]
         Norm2 x      -> tree t "norm2" [x]
         Diff s x     -> tree t ("d/d" ++ s) [x]
         Pair x y     -> tree t "pair" [x,y]
         Case cs x    -> error "TODO"
         Builtin f    -> show f

sexpr :: String -> [String] -> String
sexpr s xs = "(" ++ intercalate " " (s : xs) ++ ")"

sexpr' :: Show t => t -> String -> [String] -> String
sexpr' a s xs = "(" ++ show a ++ " | " ++ intercalate " " (s : xs) ++ ")"
