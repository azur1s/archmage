module Model where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans as T

-- | Concrete Syntax Tree
-- Only used for parsing, because it will be lowered
-- to a normal abstract syntax tree
data Cst
    = CstBool Bool
    | CstInt Int
    | CstFloat Float
    | CstStr T.Text
    | CstSym T.Text
    | CstList [Cst]
    deriving (Show)

-- | Abstract Syntax Tree
-- This is the data type that will be used for evaluation
data Ast
    = Nil
    | Bool Bool
    | Int Integer
    | Float Float
    | Str T.Text
    | Sym T.Text
    | App Ast [Ast]
    -- Primitives
    | Quote Ast
    | Unquote Ast
    | UnquoteSplicing Ast
    | Quasiquote Ast
    | Lam [T.Text] Ast
    | Let T.Text Ast
    | If Ast Ast Ast
    | Do [Ast]
    deriving (Show, Eq)

-- | Format Abstract Syntax Tree for printing
printAst :: Ast -> T.Text
printAst Nil = "nil"
printAst (Bool b) = if b then "true" else "false"
printAst (Int i) = T.pack $ show i
printAst (Float f) = T.pack $ show f
printAst (Str s) = s
printAst (Sym s) = s
printAst (App f []) = "(" <> printAst f <> ")"
printAst (App f args) = "(" <> printAst f <> " " <> T.intercalate " " (map printAst args) <> ")"
printAst (Quote a) = "'" <> printAst a
printAst (Unquote a) = "~" <> printAst a
printAst (UnquoteSplicing a) = "~@" <> printAst a
printAst (Quasiquote a) = "`" <> printAst a
printAst (Lam args body) = "(lambda (" <> T.intercalate " " args <> ") " <> printAst body <> ")"
printAst (Let name body) = "(let " <> name <> " " <> printAst body <> ")"
printAst (If cond then' else') = "(if " <> printAst cond <> " " <> printAst then' <> " " <> printAst else' <> ")"
printAst (Do exprs) = "(do " <> T.intercalate " " (map printAst exprs) <> ")"

-- | Check if all the element in list are symbol
isSymList :: [Ast] -> Bool
isSymList [] = True
isSymList (Sym _ : xs) = isSymList xs
isSymList _ = False

-- | Lower a concrete syntax tree to an abstract syntax tree
lowerCst :: Cst -> Ast
lowerCst (CstBool x) = Bool x
lowerCst (CstInt x) = Int $ toInteger x
lowerCst (CstFloat x) = Float x
lowerCst (CstStr x) = Str x
lowerCst (CstSym x) = Sym x
lowerCst (CstList asts) = if null asts then Nil
    else case lowerCst (head asts) of

        Sym "comment" -> Nil

        Sym "quote" -> if length asts == 2
            then Quote (lowerCst (asts !! 1))
            else error "quote: arity mismatch (required 1)"

        Sym "unquote" -> if length asts == 2
            then Unquote (lowerCst (asts !! 1))
            else error "unquote: arity mismatch (required 1)"

        Sym "unquote-splicing" -> if length asts == 2
            then UnquoteSplicing (lowerCst (asts !! 1))
            else error "unquote-splicing: arity mismatch (required 1)"

        Sym "quasiquote" -> if length asts == 2
            then Quasiquote (lowerCst (asts !! 1))
            else error "quasiquote: arity mismatch (required 1)"

        Sym "lambda" -> if length (tail asts) /= 2 then
            error "lambda: arity mismatch (required 2)"
            else case lowerCst (asts !! 1) of -- Get argument(s)
                App x xs -> if isSymList xs then
                    Lam (map (\(Sym x) -> x) (x:xs)) (lowerCst (asts !! 2))
                    else error "Invalid lambda, symbol is expected in argument list"
                _ -> error "Invalid lambda, argument should be a list"

        Sym "let" -> if length (tail asts) /= 2 then
            error "let: arity mismatch (required 2)"
            else case lowerCst (asts !! 1) of -- Get name
                Sym n -> Let n (lowerCst (asts !! 2))
                -- Convert (let (f a b c) ..) to (let f (lambda (a b c) ..))
                App (Sym n) xs -> if isSymList xs then
                    Let n (Lam (map (\(Sym x) -> x) xs) (lowerCst (asts !! 2)))
                    else error "Invalid let, symbol is expected in argument list"
                _ -> error "Invalid let, symbol or list is expected in argument list"

        Sym "if" -> if length (tail asts) /= 3 then
            error "if: arity mismatch (required 3)"
            else If (lowerCst (asts !! 1)) (lowerCst (asts !! 2)) (lowerCst (asts !! 3))

        Sym "do" -> if null (tail asts) then
            error "do: arity mismatch (required at least 1)"
            else Do (map lowerCst (tail asts))

        x -> App x (map lowerCst (tail asts))

-- | Environment & Evaluator Monad data type
data EvalData = EvalData
    { env :: M.Map T.Text Ast
    , qq :: Bool
    } deriving (Show)
type Eval a = E.ExceptT String (S.StateT EvalData IO) a

initEvalData :: EvalData
initEvalData = EvalData M.empty False

-- | Helper function for checking arity
require :: T.Text -> Int -> [a] -> Eval ()
require name n xs = if length xs == n then return () else
    E.throwE
    $ T.unpack name ++ ": arity mismatch, expected " ++ show n
    ++ " but got " ++ show (length xs)

-- | Environment manipulation
get :: T.Text -> Eval Ast
get x = T.lift S.get >>= \e -> case M.lookup x (env e) of
    Just a -> return a
    Nothing -> E.throwE $ "Unbound variable: " ++ T.unpack x

set :: T.Text -> Ast -> Eval ()
set x v = T.lift S.get >>= \e -> T.lift $ S.put e { env = M.insert x v (env e) }