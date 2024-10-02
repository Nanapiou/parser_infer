open Ast

let rec print_typ =
  let open Printf in
  function
  | TConst (TInt) -> print_string "TInt"
  | TConst (TBool) -> print_string "TBool"
  | QVar x -> printf "QVar %s" x
  | TVar ({ contents = Unbound (x, l) }) -> printf "TVar %s %d" x l
  | TVar ({ contents = Link t }) -> print_typ t
  | TArrow (t1, t2) ->
    print_char '(';
    print_typ t1;
    print_string " -> ";
    print_typ t2;
    print_char ')'

let rec print_expr = function
  | Var x -> print_string x
  | App (e1, e2) ->
    print_string "(";
    print_expr e1;
    print_string ") (";
    print_expr e2;
    print_char ')'
  | Fun (x, e) ->
    Printf.printf "fun %s -> (" x;
    print_expr e;
    print_char ')'
  | Int i -> print_int i
  | Bool b -> print_string (string_of_bool b)
  | If (e1, e2, e3) ->
    print_string "if (";
    print_expr e1;
    print_string ") then (";
    print_expr e2;
    print_string ") else (";
    print_expr e3;
    print_char ')'
  | Let (x, e1, e2) ->
    Printf.printf "let %s = (" x;
    print_expr e1;
    print_string ") in (";
    print_expr e2;
    print_char ')'