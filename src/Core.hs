module Core where

import Control.Monad.Trans (liftIO)
import Types

invalidArgAmount :: String -> Int -> Except Value
invalidArgAmount fn amount = throwStr
    $ fn ++ ": Invalid number of arguments (expected " ++ show amount ++ ")"

invalidArgTypes :: String -> [Value] -> Except Value
invalidArgTypes fn args = throwStr
    $ fn ++ ": Invalid argument types: " ++ show args

core :: [(String, Fn)]
core = [
    -- Arithmetic functions
      ("+", \case
        [Int a, Int b]     -> return $ Int (a + b)
        [Float a, Float b] -> return $ Float (a + b)
        [a, b]             -> invalidArgTypes "+" [a, b]
        _                  -> invalidArgAmount "+" 2)
    , ("-", \case
        [Int a, Int b]     -> return $ Int (a - b)
        [Float a, Float b] -> return $ Float (a - b)
        [a, b]             -> invalidArgTypes "-" [a, b]
        _                  -> invalidArgAmount "-" 2)
    , ("*", \case
        [Int a, Int b]     -> return $ Int (a * b)
        [Float a, Float b] -> return $ Float (a * b)
        [a, b]             -> invalidArgTypes "*" [a, b]
        _                  -> invalidArgAmount "*" 2)
    , ("/", \case
        [Int a, Int b]     -> return $ Int (a `div` b)
        [Float a, Float b] -> return $ Float (a / b)
        [a, b]             -> invalidArgTypes "/" [a, b]
        _                  -> invalidArgAmount "/" 2)
    , ("%", \case
        [Int a, Int b]     -> return $ Int (a `mod` b)
        [a, b]             -> invalidArgTypes "%" [a, b]
        _                  -> invalidArgAmount "%" 2)
    -- IO Functions
    , ("print", \case
        [a] -> liftIO $ putStr (fmtValue a) >> return nil
        _   -> invalidArgAmount "print" 1)
    , ("println", \case
        [a] -> liftIO $ putStr (fmtValueLn a) >> return nil
        _   -> invalidArgAmount "println" 1)
    , ("str", return . Str . concatMap fmtValue)
    ]

