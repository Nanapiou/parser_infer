open Ast
open Main


type typ_env = typ Env.t
type constraints = (typ * typ) list (* Each (t, t) should have t = t *)
type substitution = typ * id (* A (t, 'x) means that we should subst t by 'x *)

let varname = ref 0
let reset_varname () = varname := 0
let next_varname () = (* We'll consider that it gaves us a fresh variable (that's the case) even if some previous integers may be free *)
  let v = !varname in 
  varname := !varname + 1;
  TVar v

(** [infer_constraints tenv e] create constraints for [e] in the environnement [tenv]. *)
let rec infer_constraints (tenv: typ_env) (e: expr): (constraints * typ) =
  match e with
  | Int _ -> ([], TConst TInt)
  | Bool _ -> ([], TConst TBool)
  | Var x -> ([], Env.find x tenv)
  | If (e1, e2, e3) -> infer_if tenv e1 e2 e3
  | Fun (x, e) -> infer_fun tenv x e
  | App (e1, e2) -> infer_app tenv e1 e2 
  | Let _ -> failwith "Not implemented, cf polymorphism"
  | Binop (bop, e1, e2) -> infer_bop tenv bop e1 e2 (* Will be deleted for natives functiuns *)

and infer_if tenv e1 e2 e3 =
  let t = next_varname () in
  let (c1, t1) = infer_constraints tenv e1 in 
  let (c2, t2) = infer_constraints tenv e2 in 
  let (c3, t3) = infer_constraints tenv e3 in
  (c1 @ c2 @ c3 @ [
    (t1, TConst TBool);
    (t, t2);
    (t, t3)],
  t)

and infer_fun tenv x e =
  let t = next_varname () in 
  let (c1, t2) = infer_constraints (Env.add x t tenv) e in 
  (c1, TArrow (t, t2))

and infer_app tenv e1 e2 =
  let t = next_varname () in 
  let (c1, t1) = infer_constraints tenv e1 in 
  let (c2, t2) = infer_constraints tenv e2 in 
  ((t1, TArrow (t2, t)) :: c1 @ c2, t)

(* Only integers for now, will always be considered as int -> int -> bool | int, just changing for <= *)
and infer_bop _ _ _ _ = 
  failwith "Nah that's shit"

(** [include_var_typ t v] return true if [v] occur in [t] as a [TVar v], otherwise it returns false. *)
let rec include_var_typ t v =
  match t with
  | TVar n -> n = v
  | TConst _ -> false 
  | TArrow (t1, t2) -> include_var_typ t1 v || include_var_typ t2 v
  | TForall _ -> failwith "Not implemented, cf polymorphism"

(** [apply_substitutions t subs] apply each subtitution of the [subs] list from right to left on the type [t] *)
let rec apply_substitutions (t: typ) (subs: substitution list) =
  List.fold_right (fun (t', x) t ->
    substitute_typ t x t'
  ) subs t

(** [substitute_typ t x t'] correspond to the [t{t'/x}] relation. *)
and substitute_typ t x t' =
  match t with
  | TVar n when x = n -> t'
  | TConst _ | TVar _ -> t 
  | TArrow (t1, t2) -> TArrow (substitute_typ t1 x t', substitute_typ t2 x t')
  | TForall _ -> failwith "Not implemented, cf polymorphism"

(** [unify c] returns a list of subtitutions which unify the set of constraints. *)
let unify (c: constraints): substitution list =
  let rec aux c subs =
    match c with
    | [] -> subs
    | (t1, t2) :: q ->
      match t1, t2 with
      | TConst TInt, TConst TInt | TConst TBool, TConst TBool -> aux q subs (* Ignoring, trivial case *)
      | TVar n1, TVar n2 when n1 = n2 -> aux q subs (* Same, but need a when... *)
      | TVar x, t when not (include_var_typ t x) -> (* x and t if x doesn't occur in t *)
        aux (List.map (fun (t1, t2) -> (substitute_typ t1 x t, substitute_typ t2 x t)) q) ((t, x) :: subs)
      | t, TVar x when not (include_var_typ t x) -> (* Same, but reversed (the when forced me to do like this) *)
        aux (List.map (fun (t1, t2) -> (substitute_typ t1 x t, substitute_typ t2 x t)) q) ((t, x) :: subs)
      | TArrow (t1, t2), TArrow (t1', t2') -> aux ((t1, t1') :: (t2, t2') :: q) subs (* a->b = c->d <=> a = c && b = d *)
      | TForall _, _ | _, TForall _ -> failwith "Not implemented, cf polymorphism"
      | _ -> failwith "No unifier" (* No case found, no unifier. *)
  in
  aux c []

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
  | TForall _ -> failwith "Not implemented, cf polymorphism"

let print_constraints =
  List.iter (fun (t1, t2) ->
    print_typ t1;
    print_string " = ";
    print_typ t2;
    print_newline ()
  )

let rec print_substitutions (subs: substitution list) =
  match subs with
  | [] -> print_newline ()
  | (t, x) :: q ->
    print_char '{';
    print_typ t;
    print_char '/';
    print_int x;
    print_string "}; ";
    print_substitutions q

let default_tenv = 
  Env.empty |>
  Env.add "( + )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TInt))) |>
  Env.add "( * )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TInt))) |>
  Env.add "( <= )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TBool)))

(* Infer a string by parsing, finding constraints, unifying them and applying substitutions *)
let infer string =
  let (c, t) = string |> parse |> infer_constraints default_tenv in
  c |> unify |> apply_substitutions t
