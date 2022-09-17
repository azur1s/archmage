module Comp where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Types (Value(..))

type Except = ExceptT Value IO

throwErr :: String -> Except a
throwErr = throwE . Str

data JsExpr = JsBool  Bool
            | JsInt   Int
            | JsFloat Float
            | JsStr   String
            | JsVar   String
            | JsCall  JsExpr [JsExpr]
            | JsLam   [String] JsExpr
            | JsBin   String JsExpr JsExpr
            | JsRet   JsExpr
            | JsExprs [JsExpr]
            deriving (Eq, Show, Read)

printArgs :: [String] -> String
printArgs [] = ""
printArgs [x] = x
printArgs (x : xs) = x ++ ", " ++ printArgs xs

printJS :: JsExpr -> String
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
printJS (JsExprs es) = unwords $ map (++ ";") $ map printJS es

compile :: Value -> Except JsExpr
compile (Bool b)  = return $ JsBool b
compile (Int i)   = return $ JsInt i
compile (Float f) = return $ JsFloat f
compile (Str s)   = return $ JsStr s
compile (Sym s)   = return $ JsVar s
compile (List xs) = compileList xs
compile a = throwErr $ "Todo: " ++ show a

compileList :: [Value] -> Except JsExpr
compileList [] = return $ JsVar "null"
compileList (Sym "do" : body) = do
    body' <- mapM compile body
    case body' of
        []  -> throwErr "Empty do block"
        [x] -> return $ JsCall (JsLam [] (JsRet x)) []
        _   -> do
            let (body'', last') = (init body', last body')
            return $ JsCall (JsLam [] (JsExprs (body'' ++ [JsRet last']))) []
compileList (Sym call : args) = do
    args' <- mapM compile args
    return $ JsCall (JsVar call) args'
compileList (x : xs) = throwErr $ "Invalid expression: " ++ show x ++ " " ++ show xs

