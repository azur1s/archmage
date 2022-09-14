module Env where

import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Types
import Data.Map as M

data Bindings = Var   (IORef (M.Map String Value))
              | Const (M.Map String Value)

type Env = [Bindings]

emptyEnv :: IO Env
emptyEnv = (: []) . Var <$> newIORef M.empty

bind :: [Value] -> [Value] -> M.Map String Value -> Maybe (M.Map String Value)
bind (Sym key : keys) (value : values) map = bind keys values (M.insert key value map)
bind [] [] map                             = Just map
bind _ _ _                                 = Nothing

-- Insert a value into the environment
envSet :: Env -> String -> Value -> IO ()
envSet (Var ref : _) key value = modifyIORef ref (M.insert key value)
envSet _ _ _                   = error "unreachable"

-- Get a variable from the environment
envGet :: Env -> String -> IO (Maybe Value)
envGet env key = f env where
    f [] = return Nothing
    f (Const map : outer) = case M.lookup key map of
        Nothing    -> f outer
        value      -> return value
    f (Var ref : outer) = do
        map <- readIORef ref
        case M.lookup key map of
            Nothing    -> f outer
            value      -> return value

-- Returns a new environment with a new scope on top of the outer environment
envApply :: Env -> [Value] -> [Value] -> Maybe Env
envApply outer keys values = (: outer) . Const <$> bind keys values M.empty


