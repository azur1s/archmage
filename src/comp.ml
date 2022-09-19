open Types

type compile_result = (ir list, string) result
let ( >>= ) = Result.bind

let rec compile (v : value) : compile_result = match v with
    | Bool b  -> Ok [Push (IRBool b)]
    | Int  i  -> Ok [Push (IRInt  i)]
    | Str  s  -> Ok [Push (IRStr  s)]
    | Sym  s  -> Ok [Load s]
    | Key  k  -> Error ("Key not allowed in this context: " ^ k)
    | List [] -> Ok [Push IRNil]
    | List l  -> match l with
        | (f :: args) -> compile_list f args
        | _           -> failwith "Empty list"

and compile_list (f : value) (args : value list) : compile_result = match f with
    | Sym "def" -> (match args with
        | [Sym s; Key _; v] -> compile v >>= fun ir -> Ok (ir @ [Store s])
        | a                 -> invalid_args "def" a)
    | Sym "if" -> (match args with
        | [c; t; e] -> compile c >>= fun c' ->
                       compile t >>= fun t' ->
                       compile e >>= fun e' ->
                       Ok (c' @ [JumpF (List.length t' + 1)]
                         @ t' @ [Jump (List.length e')]
                         @ e')
        | a -> invalid_args "if" a)
    | Sym "+" -> (match args with
        | [a; b] -> compile_binop Add a b
        | a      -> invalid_args "+" a)
    | Sym "-" -> (match args with
        | [a; b] -> compile_binop Sub a b
        | a      -> invalid_args "-" a)
    | Sym "*" -> (match args with
        | [a; b] -> compile_binop Mul a b
        | a      -> invalid_args "*" a)
    | Sym "/" -> (match args with
        | [a; b] -> compile_binop Div a b
        | a      -> invalid_args "/" a)
    | Sym "%" -> (match args with
        | [a; b] -> compile_binop Mod a b
        | a      -> invalid_args "%" a)
    | Sym "=" -> (match args with
        | [a; b] -> compile_binop Equal a b
        | a      -> invalid_args "=" a)
    | Sym "<" -> (match args with
        | [a; b] -> compile_binop Less a b
        | a      -> invalid_args "<" a)
    | Sym ">" -> (match args with
        | [a; b] -> compile_binop Great a b
        | a      -> invalid_args ">" a)
    | Sym "and" -> (match args with
        | [a; b] -> compile_binop And a b
        | a      -> invalid_args "and" a)
    | Sym "or" -> (match args with
        | [a; b] -> compile_binop Or a b
        | a      -> invalid_args "or" a)
    | Sym "not" -> (match args with
        | [a] -> compile a >>= fun a' -> Ok (a' @ [Not])
        | a   -> invalid_args "not" a)
    | Sym "print" -> (match args with
        | [a] -> compile a >>= fun a' -> Ok (a' @ [Call "print"])
        | a   -> invalid_args "print" a)
    | Sym "join" -> (match args with
        | [a; b] -> compile a >>= fun a' ->
                    compile b >>= fun b' ->
                    Ok (a' @ b' @ [Call "join"])
        | a      -> invalid_args "join" a)
    | Sym s -> compiles args >>= fun args' -> Ok (args' @ [Call s])
    | _ -> Error "Invalid function"

and invalid_args (op : string) (args : value list) : compile_result =
    Error (Printf.sprintf "Invalid arguments to %s: %s" op (fmt_value (List args)))

and compile_binop (op : ir) (a : value) (b : value) : compile_result =
    compile a >>= fun a' ->
    compile b >>= fun b' ->
    Ok (a' @ b' @ [op])

and compiles (vs : value list) : compile_result = match vs with
    | [] -> Ok []
    | v :: vs -> compile v   >>= fun v' ->
                 compiles vs >>= fun vs' ->
                 Ok (v' @ vs')

