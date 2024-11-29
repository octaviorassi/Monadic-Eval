module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int
type Trace = String

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
-- COMPLETAR

-- Definamos los transformers ErrorT, StateT y TraceT
newtype ErrorT  e m a  = ErrorT  { runErrorT   :: m (Either e a    )}
newtype WriterT w m a  = WriterT { runWriterT  :: m (Pair a w      )}
newtype StateT  s m a  = StateT  { runStateT   :: m (s -> Pair a s )} -- Esto es medio inutil en realidad, la m deberia aplicar al pair


-- Env -> Pair (Either Error (Pair a Trace)) Env
type StateErrorTrace a = WriterT Trace (ErrorT Error (StateT Env Identity)) a

instance Monad m => Monad (ErrorT e m) where
  return a  = ErrorT $  return (Right a)
  m >>=  f  = ErrorT $  runErrorT m >>= \eith ->
                        case eith of
                          Left  e ->  return $ Left  e
                          Right a ->  runErrorT (f a) >>= \eith' ->
                                      case eith' of
                                        Left  e -> return $ Left  e
                                        Right b -> return $ Right b 

instance (Monad m, Monoid w) => Monad (WriterT e w) where
  return a  = WriterT $ return (a :!: mempty)
  m >>=  f  = WriterT $ runWriterT m      >>= \(a :!: w)  ->
                        runWriterT (f a)  >>= \(b :!: w') ->
                        return (b :!: w mappend w')

instance Monad m => Monad (StateT s m) where
  return a = StateT $ return (\s -> (a :!: s))
  m >>=  f = StateT $ (\s ->  runStateT m    >>= \h ->
                              let (a :!: s') = h s
                              in  runStateT (f a) >>= \g ->
                                  g s')   

instance MonadTrans (StateT s) where
  -- Monad m => m a -> StateT s m a
  lift ma = ma >>= \a ->
            return a


instance MonadTrans (ErrorT e) where
  -- Monad m => m a -> ErrorT e m a
  lift ma = ma >>= \a ->
            return a

instance MonadTrans (WriterT s) where
  -- Monad m => m a -> WriterT w m a
  lift ma = ma >>= \a ->
            return a


-- Recuerde agregar las siguientes instancias para calmar al GHC:
-- instance Functor StateErrorTrace where
--   fmap = liftM

-- instance Applicative StateErrorTrace where
--   pure  = return
--   (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs


-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
-- COMPLETAR

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
-- COMPLETAR

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
-- COMPLETAR

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Env, Trace)
eval = undefined

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
-- stepCommStar :: [dar el tipo segun corresponda]
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
-- stepComm :: [dar el tipo segun corresponda]
stepComm = undefined

-- Evalua una expresion 
-- evalIntExp :: [dar el tipo segun corresponda]
evalExp = undefined
