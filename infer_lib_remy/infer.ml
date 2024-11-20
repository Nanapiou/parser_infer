open Ast 
open Util
module StringDict = Map.Make(String)
module Util = Util
module Parse = Parse

exception NoUnifier of typ * typ
exception OccurCheck of typ


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

let new_arrow ty1 ty2: typ =
  TArrow(ty1, ty2, {level_new = !current_level; level_old = !current_level})


(* Delayed occurs check. We do not do the occurs check when unifying
   a free type variable. Therefore, we may construct a cyclic type.
   The following function, executed only at the end of the type checking,
   checks for no cycles in the type.
   Incidentally, OCaml does allow cycles in the type: types are generally
   (equi-)recursive in OCaml.
*)
let rec cycle_free : typ -> unit = function
  | TVar {contents = Unbound _} | TConst _ -> ()
  | TVar {contents = Link ty}   -> cycle_free ty
  | TArrow (_,_,ls) when ls.level_new = marked_level -> failwith "occurs check"
  | TArrow (t1,t2,ls) ->
      let level = ls.level_new in
      ls.level_new <- marked_level;
      cycle_free t1;
      cycle_free t2;
      ls.level_new <- level


(*
  Update the level of the type so that it does not exceed the
  given level l.
  Invariant: a level of a type can only decrease 
  (assigning the type generic_level is special, and does
  not count as the `update')
  The existing level of the type cannot be generic_level
  (quantified variables must be specially instantiated) or
  marked_level (in which case, we encounter a cycle).
  If the type to update is composite and its new and old
  levels were the same and the new level is updated to a
  smaller level, the whole type is put into the 
  to_be_level_adjusted queue for later traversal and adjustment
  of the levels of components.
  This work queue to_be_level_adjusted is akin to the list
  of assignments from the old generation to the new maintained by a
  generational garbage collector (such as the one in OCaml).
  The update_level itself takes constant time.
*)
let to_be_level_adjusted = ref []
let reset_level_adjustment () = to_be_level_adjusted := []

let update_level (l: level): typ -> unit = function
  | TVar ({contents = Unbound (n,l')} as tvr) -> 
      assert (not (l' = generic_level));
      if l < l' then
        tvr := Unbound (n,l)
  | TArrow (_,_,ls) as ty -> 
      assert (not (ls.level_new = generic_level));
      if ls.level_new = marked_level then failwith "occurs check";
      if l < ls.level_new then begin
        if ls.level_new = ls.level_old then
          to_be_level_adjusted := ty :: !to_be_level_adjusted;
        ls.level_new <- l
      end
  | TConst _ -> ()
  | _ -> failwith "Update levels"      

(* Sound generalization: generalize (convert to quantified vars) 
   only those free TVars whose level is greater than the current.
   These TVars belong to dead regions.
   A quantified var is a TVar at the generic_level.
   We traverse only those parts of the type that may contain
   type variables at the level greater than the current.
   If a type has the level of the current or smaller, all
   of its components have the level not exceeding the current -- and
   so that type does not have to be traversed.
   After generalization, a constructed type receives the generic_level
   if at least one of its components is quantified.

   However, before generalization we must perform the pending level updates.
   After all, a pending update may decrease the level of a type
   variable (promote it to a wider region) and thus save the variable 
   from quantification.
   We do not need to do all of the pending updates: only those that
   deal with types whose level_old > current_level. If 
   level_old <= current_level, the type contains no generalizable type
   variables anyway.
*)
let force_delayed_adjustments () =
  let rec loop acc level ty = 
    match repr ty with
    | TVar ({contents = Unbound (name,l)} as tvr) when l > level ->
        tvr := Unbound (name,level); acc
    | TArrow (_,_,ls) when ls.level_new = marked_level ->
        raise (OccurCheck ty)
    | TArrow (_,_,ls) as ty ->
        if ls.level_new > level then ls.level_new <- level;
        adjust_one acc ty
    | _ -> acc
  (* only deals with composite types *)
  and adjust_one acc = function
    | TArrow (_, _, ls) as ty when ls.level_old <= !current_level ->
        ty::acc                         (* update later *)
    | TArrow (_, _, ls) when ls.level_old = ls.level_new ->
        acc                             (* already updated *)
    | TArrow (ty1, ty2, ls) ->
        let level = ls.level_new in
        ls.level_new <- marked_level;
        let acc = loop acc level ty1 in
        let acc = loop acc level ty2 in
        ls.level_new <- level;
        ls.level_old <- level; 
        acc
    | _ -> assert false
  in
  to_be_level_adjusted :=
    List.fold_left adjust_one [] !to_be_level_adjusted



let gen : typ -> unit = fun ty ->
  force_delayed_adjustments ();
  let rec loop ty =
    match repr ty with
    | TVar ({contents = Unbound (name,l)} as tvr)
      when l > !current_level -> tvr := Unbound (name,generic_level)
    | TArrow (ty1,ty2,ls) when ls.level_new > !current_level ->
      let ty1 = repr ty1 and ty2 = repr ty2 in
      loop ty1; loop ty2;
      let l = max (get_level ty1) (get_level ty2) in
      ls.level_old <- l; ls.level_new <- l (* set the exact level upper bound *)
    | _ -> ()
  in loop ty


(* instantiation: replace schematic variables with fresh TVars.
   Only the components at generic_level are traversed, since only
   those may contain quantified type variables.
*)
let inst (ty: typ): typ = 
  let rec loop (subst: typ StringDict.t) = function
    | TVar {contents = Unbound (name, l)} when
        l = generic_level -> 
        begin
          match StringDict.find_opt name subst with
          | Some ty -> ty, subst
          | None -> 
              let tv = newvar () in
              tv, StringDict.add name tv subst
        end
    | TVar {contents = Link ty} -> loop subst ty
    | TArrow (ty1, ty2, ls) when ls.level_new = generic_level ->
        let ty1, subst = loop subst ty1 in
        let ty2, subst = loop subst ty2 in
        new_arrow ty1 ty2, subst
    | ty -> ty, subst
  in
  fst (loop StringDict.empty (repr ty))


(* Unifying a free variable tv with a type t takes constant time:
   it merely links tv to t (setting the level of t to tv if tv's
   level was smaller). Therefore, cycles may be created accidentally,
   and the complete update of type levels may have to be done
   at a later time.
   Incidentally, another unification may need to traverse the type
   with the pending level update. That unification will do the level
   update along the way.
*)

let rec unify (t1: typ) (t2: typ): unit =
  if t1 == t2 then ()                   (* t1 and t2 are physically the same *)
  else match (repr t1, repr t2) with
  | (TVar ({contents = Unbound (_,l1)} as tv1) as t1, (* unify two free vars *)
    (TVar ({contents = Unbound (_,l2)} as tv2) as t2)) ->
      if tv1 == tv2 then ()             (* the same variable *)
      else
       (* bind the higher-level var *)
       if l1 > l2 then tv1 := Link t2 else tv2 := Link t1
  | (TVar ({contents = Unbound (_,l)} as tv),t')
  | (t',TVar ({contents = Unbound (_,l)} as tv)) -> 
      update_level l t';
      tv := Link t'
  | (TArrow (tyl1,tyl2,ll), TArrow (tyr1,tyr2,lr)) ->
      if ll.level_new = marked_level || lr.level_new = marked_level then
        failwith "cycle: occurs check";
      let min_level = min ll.level_new lr.level_new in
      ll.level_new <- marked_level; lr.level_new <- marked_level;
      unify_lev min_level tyl1 tyr1;
      unify_lev min_level tyl2 tyr2;
      ll.level_new <- min_level; lr.level_new <- min_level
  | (TConst c1, TConst c2) when c1 = c2 -> ()
  | _ -> raise (NoUnifier (t1, t2))

and unify_lev l ty1 ty2 =
  let ty1 = repr ty1 in
  update_level l ty1;
  unify ty1 ty2

let rec infer_base (tenv: env): expr -> typ = function
  | Int _ -> TConst TInt
  | Bool _ -> TConst TBool
  | Var x -> inst @@ StringDict.find x tenv
  | Fun (x, e) -> infer_fun tenv x e
  | Let (x, e, e') -> infer_let tenv x e e'
  | App (e, e') -> infer_app tenv e e'
  | If (eb, e, e') -> infer_if tenv eb e e'
and infer_fun tenv x e =
  let ty_x = newvar () in
  let ty_e = infer_base (StringDict.add x ty_x tenv) e in
  new_arrow ty_x ty_e
and infer_let tenv x e e' =
  enter_level ();
  let ty_e = infer_base tenv e in
  exit_level ();
  gen ty_e;
  infer_base (StringDict.add x ty_e tenv) e'
and infer_app tenv e e' =
  let ty_fun = infer_base tenv e in
  let ty_arg = infer_base tenv e' in
  let ty_res = newvar () in
  unify ty_fun (new_arrow ty_arg ty_res);
  ty_res
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
  StringDict.add "( + )" (new_arrow (TConst TInt) (new_arrow (TConst TInt) (TConst TInt))) |>
  StringDict.add "( * )" (new_arrow (TConst TInt) (new_arrow (TConst TInt) (TConst TInt))) |>
  StringDict.add "( <= )" (new_arrow (TConst TInt) (new_arrow (TConst TInt) (TConst TBool)))

let infer (txt: string) = infer_base default_tenv (Parse.parse txt)