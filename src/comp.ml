open Types

type compile_result = (ir list, string) result
let ( >>= ) = Result.bind

let rec compile (v : value) : compile_result = match v with
    | Bool b  -> Ok [Push (IRBool b)]
    | Int  i  -> Ok [Push (IRInt  i)]
    | Str  s  -> Ok [Push (IRStr  s)]
    | Sym  s  -> Ok [Load s]
    | List [] -> failwith "Empty list"
    | List l  -> match l with
        | (f :: args) -> compile_list f args
        | _           -> failwith "Empty list"

and compile_list (f : value) (args : value list) : compile_result = match f with
    | Sym "def" -> (match args with
        | [Sym s; v] -> compile v >>= fun ir -> Ok (ir @ [Store s])
        | _          -> failwith "Invalid arguments to def")
    | Sym "if" -> (match args with
        | [c; t; e] -> compile c >>= fun c' ->
                       compile t >>= fun t' ->
                       compile e >>= fun e' ->
                       Ok (c' @ [JumpF (List.length t' + 1)]
                         @ t' @ [Jump (List.length e')]
                         @ e')
        | _         -> Error "Invalid arguments to if")
    | Sym "+" -> (match args with
        | [a; b] -> compile_binop Add a b
        | _      -> Error "Invalid arguments to +")
    | Sym "-" -> (match args with
        | [a; b] -> compile_binop Sub a b
        | _      -> Error "Invalid arguments to -")
    | Sym "*" -> (match args with
        | [a; b] -> compile_binop Mul a b
        | _      -> Error "Invalid arguments to *")
    | Sym "/" -> (match args with
        | [a; b] -> compile_binop Div a b
        | _      -> Error "Invalid arguments to /")
    | Sym "%" -> (match args with
        | [a; b] -> compile_binop Mod a b
        | _      -> Error "Invalid arguments to %")
    | Sym "=" -> (match args with
        | [a; b] -> compile_binop Equal a b
        | _      -> Error "Invalid arguments to =")
    | Sym "<" -> (match args with
        | [a; b] -> compile_binop Less a b
        | _      -> Error "Invalid arguments to <")
    | Sym ">" -> (match args with
        | [a; b] -> compile_binop Great a b
        | _      -> Error "Invalid arguments to >")
    | Sym "and" -> (match args with
        | [a; b] -> compile_binop And a b
        | _      -> Error "Invalid arguments to and")
    | Sym "or" -> (match args with
        | [a; b] -> compile_binop Or a b
        | _      -> Error "Invalid arguments to or")
    | Sym "not" -> (match args with
        | [a] -> compile a >>= fun a' -> Ok (a' @ [Not])
        | _   -> Error "Invalid arguments to not")
    | Sym "print" -> (match args with
        | [a] -> compile a >>= fun a' -> Ok (a' @ [Call "print"])
        | _   -> Error "Invalid arguments to print")
    | Sym s -> compiles args >>= fun args' ->
               Ok (args' @ [Call s])
    | _ -> Error "Invalid function"

and compile_binop (op : ir) (a : value) (b : value) : compile_result =
    compile a >>= fun a' ->
    compile b >>= fun b' ->
    Ok (a' @ b' @ [op])

and compiles (vs : value list) : compile_result = match vs with
    | [] -> Ok []
    | v :: vs -> compile v   >>= fun v' ->
                 compiles vs >>= fun vs' ->
                 Ok (v' @ vs')

