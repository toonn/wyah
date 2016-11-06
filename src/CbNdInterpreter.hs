import Data.IORef

data Expr
  = EVar String
  | ELam String Expr
  | EApp Expr Expr
  | EBool Bool
  | EInt Integer
  | EFix Expr
  deriving (Show)

data Value
  = VBool Bool
  | VInt Integer
  | VClosure (Thunk -> IO Value)

instance Show Value where
  show (VBool b) = show b
  show (VInt n) = show n
  show (VClosure _) = "<<closure>>"

type Env = [(String, IORef Thunk)]

type Thunk = () -> IO Value

update :: IORef Thunk -> Value -> IO ()
update ref v = do
  writeIORef ref (\() -> return v)
  return ()

lookupEnv :: Env -> String -> IO (IORef Thunk)
lookupEnv [] y = error $ "Unbound Variable" ++ y
lookupEnv ((x, v) : xs) n =
  if x == n
  then return v
  else lookupEnv xs n

force :: IORef Thunk -> IO Value
force ref = do
  th <- readIORef ref
  v <- th ()
  update ref v
  return v

mkThunk :: Env -> String -> Expr -> (Thunk -> IO Value)
mkThunk env x body a = do
  a' <- newIORef a
  eval ((x, a') : env) body

eval :: Env -> Expr -> IO Value
eval env ex = case ex of
                EVar n -> do
                  th <- lookupEnv env n
                  force th
                ELam x e -> return $ VClosure (mkThunk env x e)
                EApp a b -> do
                  VClosure c <- eval env a
                  c (\() -> eval env b)
                EBool b -> return $ VBool b
                EInt n -> return $ VInt n
                EFix e -> eval env (EApp e (EFix e))

omega :: Expr
omega = EApp (ELam "x" (EApp (EVar "x") (EVar "x")))
             (ELam "x" (EApp (EVar "x") (EVar "x")))

test1 :: IO Value
test1 = eval [] $ EApp (ELam "y" (EInt 42)) omega
                
main = undefined
