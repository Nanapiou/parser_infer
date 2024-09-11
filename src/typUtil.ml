open Ast

let rec print_typ = function
  | TConst (TInt) -> print_string "TInt"
  | TConst (TFloat) -> print_string "TFloat"
  | TConst (TBool) -> print_string "TBool"
  | TConst (TUnit) -> print_string "TUnit"
  | TVar n -> Printf.printf "TVar %d" n
  | TArrow (t1, t2) ->
    print_char '(';
    print_typ t1;
    print_string " -> ";
    print_typ t2;
    print_char ')'
  | TForall (l, t) ->
    print_string "TForall ";
    List.iter (Printf.printf "'%d ") l;
    print_string ". ";
    print_typ t

let print_constraints =
  List.iter (fun (t1, t2) ->
    print_typ t1;
    print_string " = ";
    print_typ t2;
    print_newline ()
  )

let rec print_substitutions = function
  | [] -> print_newline ()
  | (t, x) :: q ->
    print_char '{';
    print_typ t;
    print_char '/';
    print_int x;
    print_string "}; ";
    print_substitutions q