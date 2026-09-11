module Ast (Expr (..), Statement (..), stripLocations) where

import Diagnostic (SourceSpan)

-- ----------------------------------------------------------------------------
-- Abstract Syntax Tree (AST)
-- ----------------------------------------------------------------------------

data Expr
  = At SourceSpan Expr
  | Var String -- Variable: x
  | Lam String Expr -- Lambda Abstraction: \x -> e (can be nested by parser)
  | App Expr Expr -- Application: e1 e2
  | Let String Expr Expr -- Let Binding: let x = e1 in e2
  | Num Integer -- Integer Literal: 5
  | Add Expr Expr -- Addition: e1 + e2
  | Sub Expr Expr -- Subtraction: e1 - e2
  | Mul Expr Expr -- Multiplication: e1 * e2
  | Eq Expr Expr -- Equality Check: e1 == e2 (Needed for conditions)
  | List [Expr] -- List Literal: [e1, e2]
  | Cons Expr Expr -- List Constructor: cons e1 e2
  | Head Expr -- List Head: head e
  | Tail Expr -- List Tail: tail e
  | IsEmpty Expr -- Check if list is empty: isEmpty e
  | BoolLit Bool -- Boolean Literal: True, False
  | IfThenElse Expr Expr Expr -- Conditional: if cond then expr1 else expr2
  deriving (Show, Eq)


data Statement = Definition String Expr | Expression Expr deriving (Show, Eq)

-- Locations belong to diagnostics, not the AST display.
stripLocations :: Expr -> Expr
stripLocations expr = case expr of
  At _ e -> stripLocations e
  Lam x e -> Lam x (stripLocations e)
  App f x -> App (stripLocations f) (stripLocations x)
  Let x a b -> Let x (stripLocations a) (stripLocations b)
  Add a b -> Add (stripLocations a) (stripLocations b)
  Sub a b -> Sub (stripLocations a) (stripLocations b)
  Mul a b -> Mul (stripLocations a) (stripLocations b)
  Eq a b -> Eq (stripLocations a) (stripLocations b)
  List es -> List (map stripLocations es)
  Cons a b -> Cons (stripLocations a) (stripLocations b)
  Head e -> Head (stripLocations e)
  Tail e -> Tail (stripLocations e)
  IsEmpty e -> IsEmpty (stripLocations e)
  IfThenElse a b c -> IfThenElse (stripLocations a) (stripLocations b) (stripLocations c)
  _ -> expr
