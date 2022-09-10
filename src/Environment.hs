module Environment where

import Types (toSeq)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Types           as T

data Bindings = Var   (IORef (Map.Map String T.Value))
              | Const (Map.Map String T.Value)

type Env = [Bindings]

emptyEnv :: IO Env
emptyEnv = (: []) . Var <$> newIORef Map.empty

bind :: [T.Value] -> [T.Value] -> Map.Map String T.Value -> Maybe (Map.Map String T.Value)
bind [T.Sym "&", T.Sym key] values m         = Just $ Map.insert key (toSeq values) m
bind (T.Sym key : keys)   (value : values) m = bind keys values $ Map.insert key value m
bind [] [] m                                 = Just m
bind _ _ _                                   = Nothing

-- Insert a variable into the environment
envSet :: Env -> String -> T.Value -> IO ()
envSet (Var ref : _) key value = modifyIORef ref $ Map.insert key value
envSet _             _   _     = error "envSet: internal error"

-- Get a variable from the environment
envGet :: Env -> String -> IO (Maybe T.Value)
envGet env key = loop env where
    loop [] = return Nothing
    loop (Const m : outer) = case Map.lookup key m of
        Nothing -> loop outer
        value   -> return value
    loop (Var ref : outer) = do
        m <- readIORef ref
        case Map.lookup key m of
            Nothing -> loop outer
            value   -> return value

-- Returns a new environment with a new scope on top of the outer environment
envApply :: Env -> [T.Value] -> [T.Value] -> Maybe Env
envApply outer keys values = (: outer) . Const <$> bind keys values Map.empty
