module TAType where

data Type
  = TNat
  | TBool
  | TArr Type Type
  deriving (Eq, Show)
