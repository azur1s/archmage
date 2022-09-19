open Types

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
    pc    = ref 0;
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

let rec eval (e : evaluator ref) : unit = if !(!e.pc) >= List.length !e.progs then () else
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
        | "join" -> (match (stack_pop !e.stack, stack_pop !e.stack) with
            | (Some (IRStr  a), Some (IRStr  b)) -> stack_push !e.stack (IRStr  (a ^ b)); eval e
            | (Some (IRList a), Some (IRList b)) -> stack_push !e.stack (IRList (a @ b)); eval e
            | _                                      -> failwith "Invalid arguments to join")
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

