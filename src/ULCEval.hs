module ULCEval (
  runEval
) where

import ULCSyntax
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Writer

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr ULCEval.Scope

instance Show Value where
  show (VInt a) = show a
  show (VBool a) = show a
  show VClosure {} = "<<closure>>"

data EvalState = EvalState
  { depth :: Int
  } deriving (Show)

inc :: Eval a -> Eval a
inc m = do
  modify $ \s -> s { depth = depth s + 1 }
  out <- m
  modify $ \s -> s { depth = depth s - 1 }
  return out

red :: Expr -> Eval ()
red x = do
  d <- gets depth
  tell [(d, x)]
  return ()

type Step = (Int, Expr)
type Eval a = WriterT [Step] (State EvalState) a

type Scope = Map.Map String Value

eval :: ULCEval.Scope -> Expr -> Eval Value
eval env expr = case expr of
                  Lit (LInt x) ->
                    return $ VInt (fromIntegral x)
                  Lit (LBool x) ->
                    return $ VBool x
                  Var x -> do
                    red expr
                    return $ env Map.! x
                  Lam x body -> inc $ return (VClosure x body env)
                  App a b -> inc $ do
                    x <- eval env a
                    red a
                    y <- eval env b
                    red b
                    apply x y

extend :: Scope -> String -> Value -> Scope
extend env v t = Map.insert v t env

apply :: Value -> Value -> Eval Value
apply (VClosure n e clo) ex = eval (extend clo n ex) e
apply _ _ = error "Tried to apply non-closure"

emptyScope :: Scope
emptyScope = Map.empty

runEval :: Expr -> (Value, [Step])
runEval x = evalState (runWriterT (eval emptyScope x)) (EvalState 0)
