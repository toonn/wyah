{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PolyInfer where

import PolyType
import PolySyntax

import Control.Monad.State
import Control.Monad.Except

import Data.Monoid
import Data.List (nub)
import Data.Foldable (foldr)
import qualified Data.Map as Map
import qualified Data.Set as Set


newtype TypeEnv = TypeEnv (Map.Map Name Scheme)
  deriving Monoid

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String

data Unique = Unique { count :: Int }

type Infer a = ExceptT TypeError (State Unique) a

type Subst = Map.Map TVar Type

initUnique :: Unique
initUnique = Unique { count = 0 }

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv Map.empty

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon a)       = TCon a
  apply s t@(TVar a)     = Map.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TVar a)   = [a]
    fv (TArr a b) = fv a ++ fv b
    fv (TCon _)   = []

    normtype (TArr a b) = TArr (normtype a) (normtype b)
    normtype (TCon a)   = TCon a
    normtype (TVar a)   =
      case lookup a ord of
        Just x -> TVar x
        Nothing -> error "type variable not in signature"

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftv env

closeOver :: (Map.Map TVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where sc = generalize emptyTyenv (apply sub ty)

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err -> Left err
  Right res -> Right $ closeOver res

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ TVar $ TV (letters !! count s)

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

nullSubst :: Subst
nullSubst = Map.empty

bind ::  TVar -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ Map.singleton a t

unify :: Type -> Type -> Infer Subst
unify (l `TArr` r) (l' `TArr` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b) | a == b = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

lookupEnv :: TypeEnv -> Name -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
    case Map.lookup x env of
      Nothing -> throwError $ UnboundVariable (show x)
      Just s -> do
        t <- instantiate s
        return (nullSubst, t)

ops :: Map.Map Binop Type
ops = Map.insert Add (typeInt `TArr` (typeInt `TArr` typeInt)) $
        Map.insert Mul (typeInt `TArr` (typeInt `TArr` typeInt)) $
          Map.insert Sub (typeInt `TArr` (typeInt `TArr` typeInt)) $
            Map.insert Eql (typeInt `TArr` (typeInt `TArr` typeBool))
              Map.empty

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env ex = case ex of
  Var x -> lookupEnv env x
  Lam x e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TArr` t1)
  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (TArr t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t'   = generalize env t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s1 `compose` s2, t2)
  If cond tr fl -> do
    (s1, t1) <- infer env cond
    (s2, t2) <- infer env tr
    (s3, t3) <- infer env fl
    s4 <- unify t1 typeBool
    s5 <- unify t2 t3
    return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)
  Fix e1 -> do
    (s1, t) <- infer env e1
    tv <- fresh
    s2 <- unify (tv `TArr` tv) t
    return (s2, apply s1 tv)
  Op op e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2
    tv <- fresh
    s3 <- unify (t1 `TArr` (t2 `TArr` tv)) (ops Map.! op)
    return (s1 `compose` s2 `compose` s3, apply s3 tv)
  Lit (LInt _) -> return (nullSubst, typeInt)
  Lit (LBool _) -> return (nullSubst, typeBool)

typeof :: TypeEnv -> Name -> Maybe Scheme
typeof (TypeEnv env) name = Map.lookup name env

inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr = runInfer ... infer
  where (...) = (.) . (.)

inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
                                 Left err -> Left err
                                 Right ty -> inferTop (extend env (name, ty)) xs
