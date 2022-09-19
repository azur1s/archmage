open Types
open Comp
open Eval

let test = [
    List [Sym "def"; Sym "foo"; Key "Int"; List [Sym "+"; Int 17; Int 17]];
    List [Sym "if";
        List [Sym "not"; List [Sym "="; Sym "foo"; Int 34]];
        List [Sym "print"; List [Sym "+"; List [Sym "*"; Sym "foo"; Int 2]; Int 1]];
        List [Sym "print"; Str "False"]];
    List [Sym "print"; Str "Yay :D"]];;

let () =
    match compiles test with
    | Ok ir ->
        let e = evaluator_new ir in
        (* print_endline (String.concat "\n" (List.map fmt_ir ir)); *)
        eval (ref e)
    | Error e -> print_endline ("Compile error: " ^ e)
