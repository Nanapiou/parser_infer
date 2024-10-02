open Ast 
module StringDict = Map.Make(String)
module Util = Util
module Parse = Parse

exception NoUnifier of typ * typ
exception OccurCheck


type env = typ StringDict.t

let current_level = ref 0 
let enter_level () = incr current_level 
let exit_level () = decr current_level

let counter_sym = ref 0 
let reset_sym () = counter_sym := 0 
let get_sym () =
  let n = !counter_sym in 
  incr counter_sym;
  if n < 26 then "'" ^ (String.make 1 (char_of_int (int_of_char 'a' + n)))
  else "'t" ^ (string_of_int n)

let newvar () =
  TVar (ref (Unbound (get_sym (), !current_level)))

let rec gen: typ -> typ = function
  | TVar {contents = Unbound (name,l)} 
      when l > !current_level -> QVar name
  | TVar {contents = Link ty} -> gen ty
  | TArrow (ty1,ty2) -> TArrow (gen ty1, gen ty2)
  | ty -> ty

let inst = 
  let rec loop subst = function
    | QVar name -> 
        begin
          try (List.assoc name subst, subst)
          with Not_found ->
            let tv = newvar () in
            (tv, (name,tv)::subst)
        end
    | TVar {contents = Link ty} -> loop subst ty
    | TArrow (ty1,ty2) -> 
        let (ty1,subst) = loop subst ty1 in
        let (ty2,subst) = loop subst ty2 in
        (TArrow (ty1,ty2), subst)
    | ty -> (ty, subst)
  in fun ty -> fst (loop [] ty) 


let rec occurs (tvr: tv ref): typ -> unit = function
  | TVar tvr' when tvr == tvr' -> raise OccurCheck
  | TVar ({contents = Unbound (name, l')} as tv) ->
      let min_level = 
        (match !tvr with Unbound (_,l) -> min l l' | _ -> l') in
      tv := Unbound (name, min_level)
  | TVar {contents = Link ty} -> occurs tvr ty
  | TArrow (t1,t2)            -> occurs tvr t1; occurs tvr t2
  | _ -> ()


let rec unify (t1: typ) (t2: typ): unit = 
  if t1 = t2 then () else match t1, t2 with
  | (TVar {contents = Link t1},t2)
  | (t1,TVar {contents = Link t2}) -> unify t1 t2
  | (TVar ({contents = Unbound _} as tv),t')
  | (t',TVar ({contents = Unbound _} as tv)) -> occurs tv t'; tv := Link t'
  | (TArrow (tyl1,tyl2), TArrow (tyr1,tyr2)) ->
      unify tyl1 tyr1;
      unify tyl2 tyr2
  | t1, t2 -> raise (NoUnifier (t1, t2))

let rec infer_base (tenv: env): expr -> typ = function
  | Int _ -> TConst TInt
  | Bool _ -> TConst TBool
  | Var x -> inst @@ StringDict.find x tenv
  | Fun (x, e) -> infer_fun tenv x e
  | Let (x, e, e') -> infer_let tenv x e e'
  | App (e, e') -> infer_app tenv e e'
  | If (eb, e, e') -> infer_if tenv eb e e'
and infer_fun tenv x e =
  let tx = newvar () in 
  TArrow (tx, infer_base (StringDict.add x tx tenv) e)
and infer_let tenv x e e' =
  enter_level ();
  let te = infer_base tenv e in
  exit_level ();
  infer_base (StringDict.add x (gen te) tenv) e'
and infer_app tenv e e' =
  let te = infer_base tenv e in 
  let te' = infer_base tenv e' in 
  let ty = newvar () in 
  unify te (TArrow (te', ty));
  ty
and infer_if tenv eb e e' =
  let teb = infer_base tenv eb in
  let te = infer_base tenv e in
  let te' = infer_base tenv e' in
  let tret = newvar () in
  unify (TConst TBool) teb;
  unify te tret;
  unify te' tret;
  tret


let default_tenv = 
  StringDict.empty |>
  StringDict.add "( + )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TInt))) |>
  StringDict.add "( * )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TInt))) |>
  StringDict.add "( <= )" (TArrow (TConst TInt, TArrow (TConst TInt, TConst TBool)))

let infer (txt: string) = infer_base default_tenv (Parse.parse txt)