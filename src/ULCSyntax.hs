module ULCSyntax where

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit

data Lit
  = LInt Int
  | LBool Bool
