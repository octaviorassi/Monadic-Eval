module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip                   = return Skip
stepComm (Let v e)              = do  ev <- evalExp e
                                      update v ev
                                      return Skip
stepComm (Seq Skip c2)          = return c2
stepComm (Seq c1 c2)            = do  c1' <- stepComm c1
                                      return (Seq c1' c2)
stepComm (IfThenElse b c1 c2)   = do  bv <- evalExp b
                                      if bv then return c1
                                            else return c2
stepComm r@(Repeat b c)         = return $ Seq c (IfThenElse b r Skip)

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const      n)  = return  n
evalExp (Var        v)  = lookfor v
evalExp (UMinus     n)  = evalUnary  negate n
evalExp (Plus   e0 e1)  = evalBin (+)   e0 e1
evalExp (Minus  e0 e1)  = evalBin (-)   e0 e1
evalExp (Times  e0 e1)  = evalBin (*)   e0 e1
evalExp (Div    e0 e1)  = evalBin div   e0 e1

evalExp BTrue           = return True
evalExp BFalse          = return False 

evalExp (Not        e)  = evalUnary     not e
evalExp (Lt     e0 e1)  = evalBin (<)   e0 e1
evalExp (Gt     e0 e1)  = evalBin (>)   e0 e1
evalExp (And    e0 e1)  = evalBin (&&)  e0 e1
evalExp (Or     e0 e1)  = evalBin (&&)  e0 e1
evalExp (Eq     e0 e1)  = evalBin (==)  e0 e1
evalExp (NEq    e0 e1)  = evalBin (/=)  e0 e1

evalExp (VarInc     s)  = varUpdate s succ
evalExp (VarDec     s)  = varUpdate s pred

evalBin :: MonadState m => (a -> b -> c) -> Exp a -> Exp b -> m c
evalBin op e0 e1 = do v0 <- evalExp e0
                      v1 <- evalExp e1
                      return $ op v0 v1

evalUnary :: MonadState m => (a -> b) -> Exp a -> m b
evalUnary op e = do v <- evalExp e 
                    return $ op v

varUpdate :: MonadState m => String -> (a -> b) -> m b
varUpdate s f = do  v <- lookfor s
                    update s (f v)
                    return $ f v

{-

  Lt ::Exp Int -> Exp Int -> Exp Bool
  Gt ::Exp Int -> Exp Int -> Exp Bool
  And ::Exp Bool -> Exp Bool -> Exp Bool
  Or ::Exp Bool -> Exp Bool -> Exp Bool
  Not ::Exp Bool -> Exp Bool
  Eq ::Exp Int -> Exp Int -> Exp Bool
  NEq ::Exp Int -> Exp Int -> Exp Bool
  VarInc :: Variable -> Exp Int
  VarDec :: Variable -> Exp Int

-}
