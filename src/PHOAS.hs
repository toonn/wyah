{-# LANGUAGE RankNTypes #-}

data ExprP a
  = VarP a
  | AppP (ExprP a) (ExprP a)
  | LamP (a -> ExprP a)
  | LitP Integer

newtype Expr = Expr { unExpr :: forall a . ExprP a }

i :: ExprP a
i = LamP VarP

k :: ExprP a
k = LamP (\x -> LamP (\y -> VarP x))

s :: ExprP a
s =
  LamP (\f ->
    LamP (\g ->
      LamP (\x ->
        AppP
          (AppP (VarP f) (VarP x))
          (AppP (VarP g) (VarP x))
        )))

data Value
  = VLit Integer
  | VFun (Value -> Value)

fromVFun :: Value -> (Value -> Value)
fromVFun val = case val of
  VFun f -> f
  _      -> error "not a function"

fromVLit :: Value -> Integer
fromVLit val = case val of
  VLit n -> n
  _      -> error "not an integer"

eval :: Expr -> Value
eval e = ev (unExpr e) where
  ev (LamP f)     = VFun (ev . f)
  ev (VarP v)     = v
  ev (AppP e1 e2) = fromVFun (ev e1) (ev e2)
  ev (LitP n)     = VLit n

skk :: ExprP a
skk = AppP (AppP s k) k

example :: Integer
example = fromVLit $ eval $ Expr (AppP skk (LitP 3))

main = undefined
