module Eval2
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple             as P
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x  = StateError (\env -> Right (x :!: env))
  m >>=  f  = StateError (
              \env -> runStateError m env >>= \(a :!: env') -> 
                      runStateError (f a) env'   
              )

{-
m :: StateError a 
runStateError m :: Env -> Either Error (Pair a Env)

f :: a -> StateError b
f a :: StateError b
runStateError (f a) :: Env -> Either Error (Pair b Env)

Yo quiero dado un Env, construir un Either Error (Pair b Env)
-} 

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
-- COMPLETAR

-- data Error = DivByZero | UndefVar deriving (Eq, Show)

instance MonadError StateError where
  -- throw :: Error -> StateError a
  throw DivByZero = StateError (\_ -> Left DivByZero)
  throw UndefVar  = StateError (\_ -> Left UndefVar)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
-- COMPLETAR
instance MonadState StateError where
  -- lookfor :: Variable -> StateError Int
  lookfor v = StateError $ \env -> case M.lookup v env of
                                  Nothing -> Left UndefVar
                                  Just v  -> Right (v :!: env)

  -- update :: Variable -> Int -> StateError ()
  update v i = StateError $ \env -> Right (() :!: M.insert v i env)

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p =  runStateError (stepCommStar p) initEnv >>= \p ->
          return $ P.snd p


-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const      n)  = return  n
evalExp (Var        v)  = lookfor v
evalExp (UMinus     n)  = evalUnary  negate n
evalExp (Plus   e0 e1)  = evalBin (+)   e0 e1
evalExp (Minus  e0 e1)  = evalBin (-)   e0 e1
evalExp (Times  e0 e1)  = evalBin (*)   e0 e1

evalExp (Div    e0 e1)  = do v0 <- evalExp e0
                             v1 <- evalExp e1
                             if v1 == 0 then throw DivByZero
                                        else return (div v0 v1)

evalExp BTrue           = return True
evalExp BFalse          = return False 

evalExp (Not        e)  = evalUnary     not e
evalExp (Lt     e0 e1)  = evalBin (<)   e0 e1
evalExp (Gt     e0 e1)  = evalBin (>)   e0 e1
evalExp (And    e0 e1)  = evalBin (&&)  e0 e1
evalExp (Or     e0 e1)  = evalBin (||)  e0 e1
evalExp (Eq     e0 e1)  = evalBin (==)  e0 e1
evalExp (NEq    e0 e1)  = evalBin (/=)  e0 e1

evalExp (VarInc     s)  = varUpdate s succ
evalExp (VarDec     s)  = varUpdate s pred

evalBin :: (MonadState m, MonadError m) => (a -> b -> c) -> Exp a -> Exp b -> m c
evalBin op e0 e1 = do v0 <- evalExp e0
                      v1 <- evalExp e1
                      return $ op v0 v1

evalUnary :: (MonadState m, MonadError m) => (a -> b) -> Exp a -> m b
evalUnary op e = do v <- evalExp e 
                    return $ op v

varUpdate :: (MonadState m, MonadError m) => String -> (Int -> Int) -> m Int
varUpdate s f = do  v <- lookfor s
                    let fv = f v
                    update s fv
                    return fv

