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

