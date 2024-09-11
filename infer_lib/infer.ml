open Ast
open Parse

module Parse = Parse
module Util = Util
module IntSet = Set.Make(Int)

exception NoUnifier of typ * typ

type typ_env = typ Env.t
type constraints = (typ * typ) list (* Each (t, t) should have t = t *)
type substitution = typ * id (* A (t, 'x) means that we should subst t by 'x *)

let varname = ref 0
let reset_varname () = varname := 0
let next_varname () = (* We'll consider that it gaves us a fresh variable (that's the case) even if some previous integers may be free *)
  let v = !varname in 
  varname := !varname + 1;
  TVar v

(** [include_var_typ t v] return true if [v] occur in [t] as a [TVar v], otherwise it returns false. *)
let rec include_var_typ t v =
  match t with
  | TVar n -> n = v
  | TConst _ -> false 
  | TArrow (t1, t2) -> include_var_typ t1 v || include_var_typ t2 v
  | TForall (_, t) -> include_var_typ t v

(** [apply_substitutions subs t] apply each subtitution of the [subs] list from right to left on the type [t] *)
let rec apply_substitutions (subs: substitution list) (t: typ) =
  List.fold_right (fun (t', x) t ->
    substitute_typ t' x t
  ) subs t

and apply_substitutions_env (subs: substitution list) (tenv: typ_env) =
  Env.map (apply_substitutions subs) tenv

(** [substitute_typ t' x t] correspond to the [t{t'/x}] relation. *)
and substitute_typ t' x t =
  match t with
  | TVar n when x = n -> t'
  | TConst _ | TVar _ -> t 
  | TArrow (t1, t2) -> TArrow (substitute_typ t' x t1, substitute_typ t' x t2)
  | TForall (_, t) -> substitute_typ t' x t


(** [unify c] returns a list of subtitutions which unify the set of constraints. *)
let unify (c: constraints): substitution list =
  let rec aux c subs =
    match c with
    | [] -> subs
    | (t1, t2) :: q ->
      match t1, t2 with
      | TConst TInt, TConst TInt | TConst TBool, TConst TBool | TConst TUnit, TConst TUnit -> aux q subs (* Ignoring, trivial case *)
      | TVar n1, TVar n2 when n1 = n2 -> aux q subs (* Same, but need a when... *)
      | TVar x, t when not (include_var_typ t x) -> (* x and t if x doesn't occur in t *)
        aux (List.map (fun (t1, t2) -> (substitute_typ t x t1, substitute_typ t x t2)) q) ((t, x) :: subs)
      | t, TVar x when not (include_var_typ t x) -> (* Same, but reversed (the when forced me to do like this) *)
        aux (List.map (fun (t1, t2) -> (substitute_typ t x t1, substitute_typ t x t2)) q) ((t, x) :: subs)
      | TArrow (t1, t2), TArrow (t1', t2') -> aux ((t1, t1') :: (t2, t2') :: q) subs (* a->b = c->d <=> a = c && b = d *)
      | TForall _, _ | _, TForall _ -> failwith "Nop" (* Shouldn't happen, no contraints created on let ... in, or this is the only case where forall are created *)
      | _ -> raise (NoUnifier (t1, t2))
  in
  aux c []

(** [instantiate t] gives a new type instance based on [t] which is a TForall.
    If [t] isn't a TForall, returns [t] itself. *)
let instantiate t = match t with
  | TForall (l, t) -> List.fold_left (
    fun t x ->
      substitute_typ (next_varname ()) x t
  ) t l
  | _ -> t

let generalize c1 tenv x t1 =
  let rec get_var_set_from_typ t = match t with
    | TConst _ -> IntSet.empty
    | TVar n -> IntSet.empty |> IntSet.add n 
    | TArrow (t1, t2) -> IntSet.union (get_var_set_from_typ t1) (get_var_set_from_typ t2)
    | TForall (_, t1) -> get_var_set_from_typ t1
  in
  let get_var_set_from_env tenv =
    Env.fold (fun _ t set -> IntSet.union (get_var_set_from_typ t) set) tenv IntSet.empty
  in
  let s = unify c1 in
  let u1 = apply_substitutions s t1 in
  let tenv1 = apply_substitutions_env s tenv in
  let u1_set = get_var_set_from_typ u1 in
  let env1_set = get_var_set_from_env tenv1 in 
  let new_vars = IntSet.diff u1_set env1_set in 
  Env.add x (TForall (IntSet.to_list new_vars, u1)) tenv1
  (* I don't know which way is better... Unifying at each let declaration, or at the very end? *)
  (* let u1_set = get_var_set_from_typ t1 in
  let env1_set = get_var_set_from_env tenv in
  let new_vars = IntSet.diff u1_set env1_set in 
  Env.add x (TForall (IntSet.to_list new_vars, t1)) tenv *)

(** [infer_constraints tenv e] create constraints for [e] in the environnement [tenv]. *)
let rec infer_constraints (tenv: typ_env) (e: expr): (constraints * typ) =
  match e with
  | Int _ -> ([], TConst TInt)
  | Bool _ -> ([], TConst TBool)
  | Unitexpr -> ([], TConst TUnit)
  | Var x -> ([], instantiate (Env.find x tenv))
  | If (e1, e2, e3) -> infer_if tenv e1 e2 e3
  | Fun (x, e) -> infer_fun tenv x e
  | App (e1, e2) -> infer_app tenv e1 e2 
  | Let (x, e1, e2) -> infer_let tenv x e1 e2
  | Typdec _ -> failwith "stfu"
  (* | Binop (_, _, _) -> assert false *)
    (* infer_bop tenv bop e1 e2 *)

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
  ((t1, TArrow (t2, t)) :: c1 @ c2, t) (* |!| By doing this, losing an order idea in constraints (I still don't know if there's an use, but I feel so) *)

and infer_let tenv x e1 e2 =
  let (c1, t1) = infer_constraints tenv e1 in
  let tenv' = generalize c1 tenv x t1 in 
  let (c2, t2) = infer_constraints tenv' e2 in 
  (c1 @ c2, t2)

(* Only integers for now, will always be considered as int -> int -> bool | int, just changing for <= *)
(* and infer_bop _ _ _ _ = 
  failwith "Nah that's shit" *)

let default_tenv = 
  Env.empty |>
  Env.add "( + )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TInt))) |>
  Env.add "( * )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TInt))) |>
  Env.add "( <= )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TBool)))

(* Infer a string by parsing, finding constraints, unifying them and applying substitutions *)
let infer string =
  let (c, t) = string |> parse |> infer_constraints default_tenv in
  let s = c |> unify in 
  apply_substitutions s t