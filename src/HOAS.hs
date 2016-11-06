{-# LANGUAGE GADTs #-}

data Expr a where
  Lift :: a                       -> Expr a
  Tup  :: Expr a  -> Expr b       -> Expr (a, b)
  Lam  :: (Expr a -> Expr b)      -> Expr (a -> b)
  App  :: Expr (a -> b) -> Expr a -> Expr b
  Fix  :: Expr (a -> a)           -> Expr a

id :: Expr (a -> a)
id = Lam Prelude.id

tr :: Expr (a -> b -> a)
tr = Lam (Lam . const)

fl :: Expr (a -> b -> b)
fl = Lam (const (Lam Prelude.id))

eval :: Expr a -> a
eval (Lift v)    = v
eval (Tup e1 e2) = (eval e1, eval e2)
eval (Lam f)     = eval . f . Lift
eval (App e1 e2) = eval e1 (eval e2)
eval (Fix f)     = eval f (eval (Fix f))

fact :: Expr (Integer -> Integer)
fact =
  Fix (
    Lam (\f ->
      Lam (\y ->
        Lift (
          if eval y == 0
          then 1
          else eval y * eval f (eval y - 1)))))

test :: Integer
test = eval fact 10

main :: IO ()
main = print test
