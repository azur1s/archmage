(********************
 * Type definitions *
 ********************)

type value =
    | Bool of bool
    | Int  of int
    | Str  of string
    | Sym  of string
    | List of value list

type ir =
    | Add   | Sub  | Mul | Div | Mod
    | Equal | Less | Great
    | And   | Or   | Not
    | Push  of ir_value
    | Call  of string
    | Load  of string
    | Store of string
    | Jump  of int (* Normal jump *)
    | JumpF of int (* Jump if false *)
and ir_value =
    | IRBool of bool
    | IRInt  of int
    | IRStr  of string
    | IRList of ir_value list

(*******************
 * Type formatters *
 *******************)

let rec fmt_value (v : value) : string = match v with
    | Bool b -> string_of_bool b
    | Int  i -> string_of_int i
    | Str  s -> "\"" ^ s ^ "\""
    | Sym  s -> s
    | List l -> "(" ^ String.concat " " (List.map fmt_value l) ^ ")"

let rec fmt_ir (ir : ir) : string = match ir with
    | Add     -> "add"
    | Sub     -> "sub"
    | Mul     -> "mul"
    | Div     -> "div"
    | Mod     -> "mod"
    | Equal   -> "equl"
    | Less    -> "less"
    | Great   -> "grea"
    | And     -> "and"
    | Or      -> "or"
    | Not     -> "not"
    | Push  v -> "push " ^ fmt_ir_value v
    | Call  s -> "call " ^ s
    | Load  s -> "load " ^ s
    | Store s -> "stor " ^ s
    | Jump  i -> "jmp  " ^ string_of_int i
    | JumpF i -> "jmpf " ^ string_of_int i
and fmt_ir_value (v : ir_value) : string = match v with
    | IRBool b -> if b then "$true" else "$false"
    | IRInt  i -> "$" ^ string_of_int i
    | IRStr  s -> "$\"" ^ s ^ "\""
    | IRList l -> "$(" ^ String.concat " " (List.map fmt_ir_value l) ^ ")"
and fmt_print_ir_value (v: ir_value) : string = match v with
    | IRBool b -> string_of_bool b
    | IRInt  i -> string_of_int i
    | IRStr  s -> s
    | IRList l -> "(" ^ String.concat " " (List.map fmt_print_ir_value l) ^ ")"

(***************
 * Compliation *
 ***************)

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

(**************
 * Evaluation *
 **************)

type env   = (string * ir_value) list ref
and  stack = ir_value list ref

let env_get (env : env) (s : string) : ir_value option =
    List.assoc_opt s !env
let env_set (env : env) (s : string) (v : ir_value) : unit =
    env := (s, v) :: !env

let stack_push (stack : stack) (v : ir_value) : unit =
    stack := v :: !stack
let stack_pop (stack : stack) : ir_value option =
    match !stack with
    | []    -> None
    | v::vs -> stack := vs; Some v

type evaluator = {
    (* Stack *)
    stack : stack;
    (* Programs *)
    progs : ir list;
    pc    : int ref;
    (* Environment *)
    env   : env;
}

let evaluator_new (irs : ir list) : evaluator = {
    stack = ref [];
    progs = irs;
    pc = ref 0;
    env   = ref [];
}

let jump (e : evaluator) (i : int) : unit =
    e.pc := !(e.pc) + i

let dbg_evaluator (e : evaluator) : unit =
    Printf.printf "\n\x1b[36mstack: \x1b[0m%s\n\x1b[36mnext: \x1b[0m%s (pc : %d)\n"
        (String.concat " " (List.map fmt_ir_value !(e.stack)))
        (if !(e.pc) >= List.length e.progs
         then "none"
         else fmt_ir (List.nth e.progs !(e.pc)))
        !(e.pc)

let ( >?= ) = Option.bind

let rec eval (e : evaluator ref) : unit =
    if !(!e.pc) >= List.length !e.progs then () else
    let p = List.nth !e.progs !(!e.pc) in
    jump !e 1;
    match p with
    | Add -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRInt a), Some (IRInt b)) -> stack_push !e.stack (IRInt (a + b)); eval e
        | _                                -> failwith "Invalid arguments to +")
    | Sub -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRInt a), Some (IRInt b)) -> stack_push !e.stack (IRInt (a - b)); eval e
        | _                                -> failwith "Invalid arguments to -")
    | Mul -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRInt a), Some (IRInt b)) -> stack_push !e.stack (IRInt (a * b)); eval e
        | _                                -> failwith "Invalid arguments to +")
    | Div -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRInt a), Some (IRInt b)) -> stack_push !e.stack (IRInt (a / b)); eval e
        | _                                -> failwith "Invalid arguments to -")
    | Mod -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRInt a), Some (IRInt b)) -> stack_push !e.stack (IRInt (a mod b)); eval e
        | _                                -> failwith "Invalid arguments to -")
    | Equal -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRBool a), Some (IRBool b)) -> stack_push !e.stack (IRBool (a = b)); eval e
        | (Some (IRInt  a), Some (IRInt  b)) -> stack_push !e.stack (IRBool (a = b)); eval e
        | _                                  -> failwith "Invalid arguments to =")
    | Less -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRInt a), Some (IRInt b)) -> stack_push !e.stack (IRBool (a < b)); eval e
        | _                                -> failwith "Invalid arguments to <")
    | Great -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRInt a), Some (IRInt b)) -> stack_push !e.stack (IRBool (a > b)); eval e
        | _                                -> failwith "Invalid arguments to >")
    | And -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRBool a), Some (IRBool b)) -> stack_push !e.stack (IRBool (a && b)); eval e
        | _                                  -> failwith "Invalid arguments to and")
    | Or -> (match (stack_pop !e.stack, stack_pop !e.stack) with
        | (Some (IRBool a), Some (IRBool b)) -> stack_push !e.stack (IRBool (a || b)); eval e
        | _                                  -> failwith "Invalid arguments to or")
    | Not -> (match stack_pop !e.stack with
        | Some (IRBool a) -> stack_push !e.stack (IRBool (not a)); eval e
        | _               -> failwith "Invalid arguments to not")
    | Push v -> stack_push !e.stack v; eval e
    | Call s -> (match s with
        | "print" -> (match stack_pop !e.stack with
            | Some v -> print_endline (fmt_print_ir_value v); eval e
            | None   -> failwith "Stack underflow")
        | f       -> failwith ("Unknown function: " ^ f))
    | Load s -> (match env_get !e.env s with
        | Some v -> stack_push !e.stack v; eval e
        | None   -> failwith "Unknown symbol")
    | Store s -> (match stack_pop !e.stack with
        | Some v -> env_set !e.env s v; eval e
        | None   -> failwith "Stack underflow")
    | Jump i -> jump !e i; eval e
    | JumpF i -> (match stack_pop !e.stack with
        | Some (IRBool b) -> if b
                             then eval e
                             else jump !e i; eval e
        | _               -> failwith "Invalid arguments to jmpf")
;;

(***************
 * Entry point *
 ***************)

let test = [
    List [Sym "def"; Sym "foo"; List [Sym "+"; Int 17; Int 17]];
    List [Sym "if";
        List [Sym "not"; List [Sym "="; Sym "foo"; Int 34]];
        List [Sym "print"; List [Sym "+"; List [Sym "*"; Sym "foo"; Int 2]; Int 1]];
        List [Sym "print"; Str "False"]];
    List [Sym "print"; Str "Yay :D"]];;

let main =
    match compiles test with
        | Ok ir ->
            let e = evaluator_new ir in
            (* print_endline (String.concat "\n" (List.map fmt_ir ir)); *)
            eval (ref e)
        | Error e -> print_endline ("Compile error: " ^ e)
