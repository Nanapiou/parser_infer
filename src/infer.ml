open Ast
open Main

type typ_env = typ Env.t
type constraints = (typ * typ) list (* Each (t, t) should have t = t *)

let varname = ref 0
let reset_varname () = varname := 0
let next_varname () = (* We'll consider that it gaves us a fresh variable (that's the case) even if some previous integers may be free *)
  let v = !varname in 
  varname := !varname + 1;
  TVar v

let rec infer_constraints (tenv: typ_env) (c: constraints) (e: expr): (constraints * typ) =
  match e with
  | Int _ -> (c, TConst TInt)
  | Bool _ -> (c, TConst TBool)
  | Var x -> (c, Env.find x tenv)
  | If (e1, e2, e3) -> infer_if tenv c e1 e2 e3
  | Fun (x, e) -> infer_fun tenv c x e
  | App (e1, e2) -> infer_app tenv c e1 e2 
  | Let _ -> failwith "Not implemented, cf polymorphism"
  | Binop (bop, e1, e2) -> infer_bop tenv c bop e1 e2 (* Will be deleted for natives functiuns *)

and infer_if tenv c e1 e2 e3 =
  let t = next_varname () in
  let (c1, t1) = infer_constraints tenv c e1 in 
  let (c2, t2) = infer_constraints tenv c e2 in 
  let (c3, t3) = infer_constraints tenv c e3 in
  (c @ c1 @ c2 @ c3 @ [(t1, TConst TBool); (t, t2); (t, t3)], t)

and infer_fun tenv c x e =
  let t = next_varname () in 
  let (c1, t2) = infer_constraints (Env.add x t tenv) c e in 
  (c @ c1, TArrow (t, t2))

and infer_app tenv c e1 e2 =
  let t = next_varname () in 
  let (c1, t1) = infer_constraints tenv c e1 in 
  let (c2, t2) = infer_constraints tenv c e2 in 
  (c @ c1 @ c2 @ [(t1, TArrow (t2, t))], t)

(* Only integers for now, will always be considered as int -> int -> bool | int, just changing for <= *)
and infer_bop _ _ _ _ _ = 
  failwith "Nah that's shit"

let default_tenv = 
  Env.empty |> 
  Env.add "( + )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TInt))) |>
  Env.add "( * )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TInt))) |>
  Env.add "( <= )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TBool)))

let infer_text text = 
  text |> parse |> infer_constraints default_tenv []

let rec print_typ = function
  | TConst (TInt) -> print_string "TInt"
  | TConst (TBool) -> print_string "TBool"
  | TVar n -> Printf.printf "TVar %d" n 
  | TArrow (t1, t2) -> 
    print_char '(';
    print_typ t1;
    print_string " -> ";
    print_typ t2;
    print_char ')'
  | TForall _ -> failwith "Not implemented"

let rec print_constraints = function
  | [] -> ()
  | (t1, t2) :: q ->
    print_typ t1;
    print_string " = ";
    print_typ t2;
    print_newline ();
    print_constraints q
