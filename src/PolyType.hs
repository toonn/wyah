module PolyType where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCon String
  | TArr Type Type
  deriving (Show, Eq, Ord)

typeInt, typeBool :: Type
typeInt = TCon "Int"
typeBool = TCon "Bool"

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)
