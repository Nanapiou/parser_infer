open Ast

let rec print_typ =
  let open Printf in
  function
  | TConst (TInt) -> print_string "TInt"
  | TConst (TBool) -> print_string "TBool"
  (* | QVar x -> printf "QVar %s" x *)
  | TVar ({ contents = Unbound (x, l) }) -> printf "%s.%d" x l
  | TVar ({ contents = Link t }) -> print_typ t
  | TArrow (t1, t2, {level_old; level_new}) ->
    Printf.printf "((%d,%d) " level_old level_new;
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

let rec repr = function
  | TVar ({ contents = Link t } as tvr) ->
    let t = repr t in 
    tvr := Link t;
    t
  | t -> t


(* get the level of a normalized type, which is not a bound TVar *)
let get_level : typ -> level = function
  | TVar {contents = Unbound (_, l)} -> l
  | TArrow (_, _, ls) -> ls.level_new
  | _ -> failwith "get_level: not a normalized type"