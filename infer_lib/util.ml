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

let print_enumerate_typ (l: enumerate_typ) =
    List.iter (fun (x, t) -> 
      match t with
      | None -> Printf.printf "| %s " x
      | Some t -> Printf.printf "| %s of " x; print_typ t
    )  l

let print_defined_typ = function
  | TDSimple t -> print_typ t 
  | TDEnumerate t -> print_enumerate_typ t

let rec print_expr = function
  | Var x -> Printf.printf "Var %s" x
  | App (e1, e2) ->
    print_string "App (";
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
  | Typdec (a, dt, e) ->
    Printf.printf "type %s = " a;
    print_defined_typ dt;
    print_string ";; ";
    print_expr e
  | Unitexpr -> print_string "()"