module Comp where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Types (Value(..))

type Except = ExceptT Value IO

throwErr :: String -> Except a
throwErr = throwE . Str

data Js = JsBool  Bool
            | JsInt   Int
            | JsFloat Float
            | JsStr   String
            | JsVar   String
            | JsCall  Js [Js]
            | JsLam   [String] Js
            | JsBin   String Js Js
            | JsRet   Js
            | JsConst String Js
            | JsBlock [Js]
            deriving (Eq, Show, Read)

printArgs :: [String] -> String
printArgs [] = ""
printArgs [x] = x
printArgs (x : xs) = x ++ ", " ++ printArgs xs

printJS :: Js -> String
-- Atom
printJS (JsBool b)  = show b
printJS (JsInt i)   = show i
printJS (JsFloat f) = show f
printJS (JsStr s)   = show s
printJS (JsVar v)   = v
-- Expression
printJS (JsCall f args) = case f of
    JsLam _ _ -> "(" ++ printJS f ++ ")(" ++ printArgs (map printJS args) ++ ")"
    _         -> printJS f        ++ "("  ++ printArgs (map printJS args) ++ ")"
printJS (JsLam args body) = unlines
    [ "(" ++ printArgs args ++ ") => {"
    , printJS body
    , "}"
    ]
printJS (JsBin op a b) = printJS a ++ " " ++ op ++ " " ++ printJS b
printJS (JsRet e) = "return " ++ printJS e ++ ";"
printJS (JsConst name e) = "const " ++ name ++ " = " ++ printJS e ++ ";"
printJS (JsBlock es) = unwords $
    map (\e -> if last e == ';' then e else e ++ ";") $
    map printJS es

compile :: Value -> Except Js
compile (Bool b)  = return $ JsBool b
compile (Int i)   = return $ JsInt i
compile (Float f) = return $ JsFloat f
compile (Str s)   = return $ JsStr s
compile (Sym s)   = return $ JsVar s
compile (List xs) = compileList xs
compile a = throwErr $ "Todo: " ++ show a

compileList :: [Value] -> Except Js
compileList [] = return $ JsVar "null"
compileList (Sym "do" : body) = do
    body' <- mapM compile body
    case body' of
        []  -> throwErr "Empty do block"
        [x] -> return $ JsCall (JsLam [] (JsRet x)) []
        _   -> do
            let (body'', last') = (init body', last body')
            return $ JsCall (JsLam [] (JsBlock (body'' ++ [JsRet last']))) []
compileList (Sym call : args) = case lookup call builtins of
    Just f  -> f args
    Nothing -> do
        args' <- mapM compile args
        return $ JsCall (JsVar ("_defined_" ++ call)) args'
compileList (x : xs) = throwErr $ "Invalid expression: " ++ show x ++ " " ++ show xs

type Builtin = [Value] -> Except Js

compileBin :: String -> [Value] -> Except Js
compileBin op [] = throwErr $ "Not enough arguments to " ++ op
compileBin _ [a]   = compile a
compileBin op as   = foldl1 (JsBin op) <$> mapM compile as

builtins :: [(String, Builtin)]
builtins =
    [ ("+", compileBin "+")
    , ("-", compileBin "-")
    , ("*", compileBin "*")
    , ("/", compileBin "/")

    , ("def", \case
        [Sym name, body] -> compile body >>= \b -> return $ JsConst name b
        _ -> throwErr "Invalid arguments for builtin `def`")

    , ("print" , \case
        [] -> throwErr "Invalid arguments for builtin `print`"
        as -> mapM compile as >>= \as' -> return $ JsCall (JsVar "console.log") as')
    ]

