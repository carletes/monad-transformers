-- Examples from http://www.grabmueller.de/martin/www/pub/Transformers.pdf

module Transformers where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.Reader (MonadReader(..), ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
           deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
             deriving (Show)

type Env = Map.Map Name Value

-- Reference implementation
eval0                  :: Env -> Exp -> Value
eval0 env (Lit i)      = IntVal i
eval0 env (Var n)      = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Abs n e)    = FunVal env n e
eval0 env (App e1 e2)  = let val1 = eval0 env e1
                             val2 = eval0 env e2
                         in case val1 of
                              FunVal env' n body -> eval0 (Map.insert n val2 env') body

-- Sample expression
exampleExp :: Exp
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

-- Implementation in monadic style
type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1                   :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)      = return $ IntVal i
eval1 env (Var n)      = maybe (fail ("Undefined variable: " ++ n)) return $ Map.lookup n env
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e)    = return $ FunVal env n e
eval1 env (App e1 e2)  = do val1 <- eval1 env e1
                            val2 <- eval1 env e2
                            case val1 of
                              FunVal env' n body -> eval1 (Map.insert n val2 env') body

-- Implementation with error handling
type Eval2 a = ErrorT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runErrorT

eval2                  :: Env -> Exp -> Eval2 Value
eval2 env (Lit i)      = return $ IntVal i
eval2 env (Var n)      = case Map.lookup n env of
                           Nothing  -> throwError ("Unbound variable: " ++ n)
                           Just val -> return val
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                              _                      -> throwError "Type error in addition"
eval2 env (Abs n e)    = return $ FunVal env n e
eval2 env (App e1 e2)  = do val1 <- eval2 env e1
                            val2 <- eval2 env e2
                            case val1 of
                              FunVal env' n body -> eval2 (Map.insert n val2 env') body
                              _                  -> throwError "type error in application"

-- Implementation with error handling and hidden environment
type Eval3 a = ReaderT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity (runErrorT (runReaderT ev env))

eval3              :: Exp -> Eval3 Value
eval3 (Lit i)      = return $ IntVal i
eval3 (Var n)      = do env <- ask
                        case Map.lookup n env of
                          Nothing  -> throwError $ "Unbound variable: " ++ n
                          Just val -> return val
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _                      -> throwError "Type error in addition"
eval3 (Abs n e)    = do env <- ask
                        return $ FunVal env n e
eval3 (App e1 e2)  = do env <-ask
                        val1 <- eval3 e1
                        val2 <- eval3 e2
                        case val1 of
                          FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
                          _                  -> throwError "Type error in application"

-- Implementation with error handling, hidden environment and state
type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

runEval4           :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval4              :: Exp -> Eval4 Value
eval4 (Lit i)      = do tick
                        return $ IntVal i
eval4 (Var n)      = do tick
                        env <- ask
                        case Map.lookup n env of
                          Nothing  -> throwError $ "Unbound variable: " ++ n
                          Just val -> return val
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _                      -> throwError "Type error in addition"
eval4 (Abs n e)    = do tick
                        env <- ask
                        return $ FunVal env n e
eval4 (App e1 e2)  = do tick
                        env <-ask
                        val1 <- eval4 e1
                        val2 <- eval4 e2
                        case val1 of
                          FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body)
                          _                  -> throwError "Type error in application"

-- Implementation with error handling, hidden environment, state and logging
type Eval5 a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer Identity))) a

runEval5           :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity (runStateT (runWriterT (runErrorT (runReaderT ev env))) st)

eval5 :: Exp -> Eval5 Value
eval5 (Lit i)      = do tick
                        tell ["Lit " ++ show i]
                        return $ IntVal i
eval5 (Var n)      = do tick
                        tell ["Var " ++ n]
                        env <- ask
                        case Map.lookup n env of
                          Nothing  -> throwError $ "Unbound variable: " ++ n
                          Just val -> return val
eval5 (Plus e1 e2) = do tick
                        tell ["Plus " ++ show e1 ++ " " ++ show e2]
                        e1' <- eval5 e1
                        e2' <- eval5 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _                      -> throwError "Type error in addition"
eval5 (Abs n e)    = do tick
                        tell ["Abs " ++ show n ++ " " ++ show e]
                        env <- ask
                        return $ FunVal env n e
eval5 (App e1 e2)  = do tick
                        tell ["App " ++ show e1 ++ " " ++ show e2]
                        env <-ask
                        val1 <- eval5 e1
                        val2 <- eval5 e2
                        case val1 of
                          FunVal env' n body -> local (const (Map.insert n val2 env')) (eval5 body)
                          _                  -> throwError "Type error in application"
