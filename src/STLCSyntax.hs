module STLCSyntax where

type Name = String

data Expr
  = Var Name
  | Lit Ground
  | App Expr Expr
  | Lam Name Type Expr
  deriving (Eq, Show)

data Ground
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TArr Type Type
  deriving (Eq, Read, Show)
