open Ast
open Main


(** [env] is the type of an environment, which maps
    a string to a value. *)
type env = value Env.t

(** [value] is the type of a lambda calculus value.
    In the environment model, that is a closure. *)
and value = 
  | VClosure of string * expr * env
  | VInt of int 
  | VBool of bool
  (* | VCouple of value * value
  | VLeft of value
  | VRight of value *)

let unbound_var_err = "Unbound variable"
let not_a_closure_err = "Application of a not closure"
let bop_types_err = "Bop types suck"
let not_a_couple_err = "This is not a couple"
let bad_match_err = "Bad match dumbass"
let bad_if_error = "Bad if dumbass"

type scope_rule = Lexical | Dynamic
let scope = Dynamic

(** [eval env e] is the [<env, e> ==> v] relation. *)
let rec eval (env : env) (e : expr) : value = match e with
  | Var x -> eval_var env x
  | App (e1, e2) -> eval_app env e1 e2
  | Fun (x, e) -> VClosure (x, e, env)
  | Int n -> VInt n 
  | Bool b -> VBool b
  | Binop (bop, e1, e2) -> eval_bop env bop e1 e2
  (* | Couple (e1, e2) -> eval_couple env e1 e2
  | Fst e -> eval_fst env e
  | Snd e -> eval_snd env e
  | Left e -> eval_left env e
  | Right e -> eval_right env e
  | Match (e, x1, e1, x2, e2) -> eval_match env e x1 e1 x2 e2  *)
  | If (e1, e2, e3) -> eval_if env e1 e2 e3
  | Let (x, e1, e2) -> eval_let env x e1 e2

(** [eval_var env x] is the [v] such that [<env, x> ==> v]. *)
and eval_var env x = 
  try Env.find x env with Not_found -> failwith unbound_var_err

(** [eval_app env e1 e2] is the [v] such that [<env, e1 e2> ==> v]. *)
and eval_app env e1 e2 = 
  match eval env e1 with
  | VClosure (x, e, defenv) -> begin
      let v2 = eval env e2 in
      let base_env_for_body = 
        match scope with
        | Lexical -> defenv
        | Dynamic -> env in
      let env_for_body = Env.add x v2 base_env_for_body in
      eval env_for_body e
    end
  | _ -> failwith not_a_closure_err

and eval_bop env bop e1 e2 =
  let v1 = eval env e1 in 
  let v2 = eval env e2 in
  match bop, v1, v2 with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Mult, VInt a, VInt b -> VInt (a * b)
  | Leq, VBool a, VBool b -> VBool (a <= b)
  | _ -> failwith bop_types_err

(* and eval_couple env e1 e2 =
  let v1 = eval env e1 in 
  let v2 = eval env e2 in
  VCouple (v1, v2)

and eval_fst env e =
  let c = eval env e in 
  match c with
  | VCouple (a, _) -> a
  | _ -> failwith not_a_couple_err

and eval_snd env e =
  let c = eval env e in 
  match c with
  | VCouple (_, b) -> b
  | _ -> failwith not_a_couple_err

and eval_left env e =
  let v = eval env e in 
  VLeft v

and eval_right env e =
  let v = eval env e in 
  VRight v

and eval_match env e x1 e1 x2 e2 =
  let v = eval env e in 
  match v with
  | VLeft v1 -> 
    let env' = Env.add x1 v1 env in 
    eval env' e1
  | VRight v2 -> 
    let env' = Env.add x2 v2 env in 
    eval env' e2
  | _ -> failwith bad_match_err *)

and eval_if env e1 e2 e3 =
  match eval env e1 with
  | VBool true -> eval env e2
  | VBool false -> eval env e3
  | _ -> failwith bad_if_error

and eval_let env x e1 e2 = 
  let v = eval env e1 in 
  let env' = Env.add x v env in 
  eval env' e2

(** [interp s] interprets [s] by parsing
    and evaluating it with the big-step model,
    starting with the empty environment. *)
let interp (s : string) : value =
  s |> parse |> eval Env.empty
