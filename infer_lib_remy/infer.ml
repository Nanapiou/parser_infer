open Ast
open Util
module StringDict = Map.Make (String)

(* To use them outside the lib *)
module Util = Util
module Parse = Parse
module Lexer = Lexer
module Parser = Parser

exception NoUnifier of typ * typ
exception OccurCheck of typ
exception ConstructorArguments of typ list * typ list

type env = typ StringDict.t

(* |!| The first typ *should be* a TConstructor *)
(* If some types are vars, thoses are quantified and linked together *)
type constructors_env = (typ * typ list) StringDict.t

let current_level = ref 0
let enter_level () = incr current_level
let exit_level () = decr current_level
let counter_sym = ref 0
let reset_sym () = counter_sym := 0

let get_sym () =
  let n = !counter_sym in
  incr counter_sym;
  if n < 26 then "'" ^ String.make 1 (char_of_int (int_of_char 'a' + n))
  else "'t" ^ string_of_int n

let newvar () = TVar (ref (Unbound (get_sym (), !current_level)))

let new_arrow ty1 ty2 : typ =
  TArrow (ty1, ty2, { level_new = !current_level; level_old = !current_level })

let new_tuple l : typ =
  TTuple (l, { level_new = !current_level; level_old = !current_level })

let new_constructor l x =
  TConstructor (l, x, { level_new = !current_level; level_old = !current_level })

(** Delayed occurs check. We do not do the occurs check when unifying a free
    type variable. Therefore, we may construct a cyclic type. The following
    function, executed only at the end of the type checking, checks for no
    cycles in the type. Incidentally, OCaml does allow cycles in the type: types
    are generally (equi-)recursive in OCaml. *)
let rec cycle_free : typ -> unit = function
  | TVar { contents = Unbound _ } | TConstant _ -> ()
  | TVar { contents = Link ty } -> cycle_free ty
  | TArrow (_, _, ls) as t when ls.level_new = marked_level ->
      raise (OccurCheck t)
  | TArrow (t1, t2, ls) ->
      let level = ls.level_new in
      ls.level_new <- marked_level;
      cycle_free t1;
      cycle_free t2;
      ls.level_new <- level
  | TTuple (l, ls) | TConstructor (l, _, ls) ->
      let level = ls.level_new in
      ls.level_new <- marked_level;
      List.iter cycle_free l;
      ls.level_new <- level
  | TTempConstructor _ -> failwith "Cycle free erro"

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

let update_level (l : level) : typ -> unit = function
  | TVar ({ contents = Unbound (n, l') } as tvr) ->
      assert (not (l' = generic_level));
      if l < l' then tvr := Unbound (n, l)
  | (TArrow (_, _, ls) as ty)
  | (TTuple (_, ls) as ty)
  | (TConstructor (_, _, ls) as ty) ->
      assert (not (ls.level_new = generic_level));
      if ls.level_new = marked_level then raise (OccurCheck ty);
      if l < ls.level_new then (
        if ls.level_new = ls.level_old then
          to_be_level_adjusted := ty :: !to_be_level_adjusted;
        ls.level_new <- l)
  | TConstant _ -> ()
  | _ -> failwith "Update levels, maybe a catch-all match"

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
    | TVar ({ contents = Unbound (name, l) } as tvr) when l > level ->
        tvr := Unbound (name, level);
        acc
    | (TArrow (_, _, ls) | TTuple (_, ls) | TConstructor (_, _, ls))
      when ls.level_new = marked_level ->
        raise (OccurCheck ty)
    | (TArrow (_, _, ls) as ty)
    | (TTuple (_, ls) as ty)
    | (TConstructor (_, _, ls) as ty) ->
        if ls.level_new > level then ls.level_new <- level;
        adjust_one acc ty
    | TConstant _ | TVar _ -> acc
    | TTempConstructor _ -> failwith "force_adjustments error"
  (* only deals with composite types *)
  and adjust_one acc = function
    | (TArrow (_, _, ls) as ty)
    | (TTuple (_, ls) as ty)
    | (TConstructor (_, _, ls) as ty)
      when ls.level_old <= !current_level ->
        ty :: acc (* update later *)
    | (TArrow (_, _, ls) | TTuple (_, ls) | TConstructor (_, _, ls))
      when ls.level_old = ls.level_new ->
        acc (* already updated *)
    | TArrow (ty1, ty2, ls) ->
        let level = ls.level_new in
        ls.level_new <- marked_level;
        let acc = loop acc level ty1 in
        let acc = loop acc level ty2 in
        ls.level_new <- level;
        ls.level_old <- level;
        acc
    | TTuple (l, ls) | TConstructor (l, _, ls) ->
        let level = ls.level_new in
        ls.level_new <- marked_level;
        let acc = List.fold_left (fun acc cur -> loop acc level cur) acc l in
        ls.level_new <- level;
        ls.level_old <- level;
        acc
    | _ -> failwith "Delayed adjustment failed, maybe catch-all case"
  in
  to_be_level_adjusted := List.fold_left adjust_one [] !to_be_level_adjusted

let gen (ty : typ) : unit =
  force_delayed_adjustments ();
  let rec loop ty =
    match repr ty with
    | TVar ({ contents = Unbound (name, l) } as tvr)
      when l > !current_level ->
        tvr := Unbound (name, generic_level)
    | TArrow (ty1, ty2, ls) when ls.level_new > !current_level ->
        let ty1 = repr ty1 and ty2 = repr ty2 in
        loop ty1;
        loop ty2;
        let l = max (get_level ty1) (get_level ty2) in
        ls.level_old <- l;
        ls.level_new <- l (* set the exact level upper bound *)
    | TTuple (l, ls) | TConstructor (l, _, ls) when ls.level_new > !current_level ->
        let l = List.map repr l in
        List.iter loop l;
        let lvl =
          List.fold_left
            (fun acc cur -> max acc (get_level cur))
            (get_level (List.hd l))
            (List.tl l)
        in
        ls.level_old <- lvl;
        ls.level_new <- lvl (* set the exact level upper bound *)
    | TConstant _ | TVar _ | TArrow _ | TTuple _ | TConstructor _ -> ()
    | TTempConstructor _ -> failwith "gen error"
  in
  loop ty

(* instantiation: replace schematic variables with fresh TVars.
   Only the components at generic_level are traversed, since only
   those may contain quantified type variables.
*)
let inst (ty: typ): typ =
  let rec loop (subst: typ StringDict.t): typ -> typ * typ StringDict.t = function
    | TVar { contents = Unbound (name, l) }
      when l = generic_level -> (
        match StringDict.find_opt name subst with
        | Some ty -> (ty, subst)
        | None ->
            let tv = newvar () in
            (tv, StringDict.add name tv subst))
    | TVar { contents = Link ty } -> loop subst ty
    | TArrow (ty1, ty2, ls)
      when ls.level_new = generic_level ->
        let ty1, subst = loop subst ty1 in
        let ty2, subst = loop subst ty2 in
        (new_arrow ty1 ty2, subst)
    | TTuple (l, ls) when ls.level_new = generic_level ->
        let flip (a, b) = (b, a) in
        let subst, l =
          List.fold_left_map (fun subst t -> flip (loop subst t)) subst l
        in
        (new_tuple l, subst)
    | TConstructor (l, x, ls) when ls.level_new = generic_level ->
        let flip (a, b) = (b, a) in
        let subst, l =
          List.fold_left_map (fun subst t -> flip (loop subst t)) subst l
        in
        (new_constructor l x, subst)
    | (TVar _ as ty)
    | (TConstant _ as ty)
    | (TArrow _ as ty)
    | (TTuple _ as ty)
    | (TConstructor _ as ty) ->
        (ty, subst)
    | TTempConstructor _ -> failwith "inst error"
  in
  fst (loop StringDict.empty (repr ty))

let inst_constr cenv x =
  let constr_holder, ty_l_holder = StringDict.find x cenv in
  (* It's ugly af, may be updated in the future... *)
  (* Both may contain same quantified types, so I need to instantiate them in the same call, or I need to upgrade the [inst] function. I chose this ugly way *)
  let temp_arrow =
    TArrow
      ( constr_holder,
        TTuple
          (ty_l_holder, { level_new = generic_level; level_old = generic_level }),
        { level_new = generic_level; level_old = generic_level } )
  in
  let temp_arrow = inst temp_arrow in
  match temp_arrow with
  | TArrow (a, b, _) ->
      (a, match b with TTuple (l, _) -> l | _ -> failwith "Shouldn't happen.")
  | _ -> failwith "Shouldn't happen."
(* End of the ugly *)

(* Unifying a free variable tv with a type t takes constant time:
   it merely links tv to t (setting the level of t to tv if tv's
   level was smaller). Therefore, cycles may be created accidentally,
   and the complete update of type levels may have to be done
   at a later time.
   Incidentally, another unification may need to traverse the type
   with the pending level update. That unification will do the level
   update along the way.
*)

let rec unify (t1 : typ) (t2 : typ) : unit =
  if t1 == t2 then () (* t1 and t2 are physically the same *)
  else
    match (repr t1, repr t2) with
    | ( (TVar ({ contents = Unbound (_, l1) } as tv1) as t1),
        (* unify two free vars *)
        (TVar ({ contents = Unbound (_, l2) } as tv2) as t2) ) ->
        if tv1 == tv2 then () (* the same variable *)
        else if
          (* bind the higher-level var *)
          l1 > l2
        then tv1 := Link t2
        else tv2 := Link t1
    | TVar ({ contents = Unbound (_, l) } as tv), t'
    | t', TVar ({ contents = Unbound (_, l) } as tv) ->
        update_level l t';
        tv := Link t'
    | TArrow (tyl1, tyl2, ll), TArrow (tyr1, tyr2, lr) ->
        if ll.level_new = marked_level then raise (OccurCheck t1)
        else if lr.level_new = marked_level then raise (OccurCheck t2);
        let min_level = min ll.level_new lr.level_new in
        ll.level_new <- marked_level;
        lr.level_new <- marked_level;
        unify_lev min_level tyl1 tyr1;
        unify_lev min_level tyl2 tyr2;
        ll.level_new <- min_level;
        lr.level_new <- min_level
    | TTuple (l1, ll), TTuple (l2, lr) when List.length l1 = List.length l2 ->
        if ll.level_new = marked_level then raise (OccurCheck t1)
        else if lr.level_new = marked_level then raise (OccurCheck t2);
        let min_level = min ll.level_new lr.level_new in
        ll.level_new <- marked_level;
        lr.level_new <- marked_level;
        List.iter2 (unify_lev min_level) l1 l2;
        ll.level_new <- min_level;
        lr.level_new <- min_level
    | TConstructor (l1, x1, ll), TConstructor (l2, x2, lr)
      when x1 = x2 && List.length l1 = List.length l2 ->
        if ll.level_new = marked_level then raise (OccurCheck t1)
        else if lr.level_new = marked_level then raise (OccurCheck t2);
        let min_level = min ll.level_new lr.level_new in
        ll.level_new <- marked_level;
        lr.level_new <- marked_level;
        List.iter2 (unify_lev min_level) l1 l2;
        ll.level_new <- min_level;
        lr.level_new <- min_level
    | TConstant c1, TConstant c2 when c1 = c2 -> ()
    | _ -> raise (NoUnifier (t1, t2))

and unify_lev l ty1 ty2 =
  let ty1 = repr ty1 in
  update_level l ty1;
  unify ty1 ty2

let manage_match_pattern cenv tenv e : env * typ =
  let rec aux tenv = function
    | Int _ -> (tenv, TConstant TInt)
    | String _ -> (tenv, TConstant TString)
    | Bool _ -> (tenv, TConstant TBool)
    | Unit -> (tenv, TConstant TUnit)
    | Var x ->
        let ty_x = newvar () in
        (StringDict.add x ty_x tenv, ty_x)
    | Tuple l ->
        let tenv, ty_l = List.fold_left_map aux tenv l in
        (tenv, new_tuple ty_l)
    | Constructor (x, l) ->
        let tenv, ty_l = List.fold_left_map aux tenv l in
        let constr, ty_l_bis = inst_constr cenv x in
        if List.length ty_l <> List.length ty_l_bis then raise (ConstructorArguments (ty_l, ty_l_bis)) else
        List.iter2 unify ty_l ty_l_bis;
        (tenv, constr)
    | If (_, _, _) | Match (_, _) | Fun (_, _) | App (_, _) | Let (_, _, _, _)
      ->
        failwith "Invalid match pattern"
  in
  aux tenv e

let rec infer_base (cenv : constructors_env) (tenv : env) : expr -> typ =
  function
  | Int _ -> TConstant TInt
  | Bool _ -> TConstant TBool
  | String _ -> TConstant TString
  | Unit -> TConstant TUnit
  | Var x -> inst @@ StringDict.find x tenv
  | Match (e, l) -> infer_match cenv tenv e l
  | Fun (x, e) -> infer_fun cenv tenv x e
  | Tuple l -> infer_tuple cenv tenv l
  | Let (r, x, e, e') -> infer_let cenv tenv r x e e'
  | App (e, e') -> infer_app cenv tenv e e'
  | If (eb, e, e') -> infer_if cenv tenv eb e e'
  | Constructor (x, l) -> infer_constructor cenv tenv x l

and infer_constructor cenv tenv x l =
  let ty_l = List.map (infer_base cenv tenv) l in
  let constr, ty_l_bis = inst_constr cenv x in
  if List.length ty_l <> List.length ty_l_bis then raise (ConstructorArguments (ty_l, ty_l_bis)) else
  List.iter2 unify ty_l ty_l_bis;
  match constr with
  | TConstructor (ls, x, _) -> new_constructor ls x (* To set the level to the current one (instead of the generic), so it is just about optimization *)
  | _ -> failwith "infer_constructor, shouldn't happen"

and infer_match cenv tenv e l =
  let ty_e = infer_base cenv tenv e in
  let ty_l =
    List.map
      (fun (e1, e2) ->
        let tenv, ty_e1 = manage_match_pattern cenv tenv e1 in
        (ty_e1, infer_base cenv tenv e2))
      l
  in
  let patter_h, val_h = List.hd ty_l in
  unify patter_h ty_e;
  iter_double
    (fun (p1, v1) (p2, v2) ->
      unify p1 p2;
      unify v1 v2)
    ty_l;
  val_h

and infer_fun cenv tenv x e =
  let ty_x = newvar () in
  let ty_e = infer_base cenv (StringDict.add x ty_x tenv) e in
  new_arrow ty_x ty_e

and infer_tuple cenv tenv l = new_tuple (List.map (infer_base cenv tenv) l)

and infer_let cenv tenv r x e e' =
  if r then (
    let ty_x = newvar () in
    enter_level ();
    let ty_e = infer_base cenv (StringDict.add x ty_x tenv) e in
    unify ty_e ty_x;
    exit_level ();
    gen ty_e;
    infer_base cenv (StringDict.add x ty_e tenv) e')
  else (
    enter_level ();
    let ty_e = infer_base cenv tenv e in
    exit_level ();
    gen ty_e;
    infer_base cenv (StringDict.add x ty_e tenv) e')

and infer_app cenv tenv e e' =
  let ty_fun = infer_base cenv tenv e in
  let ty_arg = infer_base cenv tenv e' in
  let ty_res = newvar () in
  unify ty_fun (new_arrow ty_arg ty_res);
  ty_res

and infer_if cenv tenv eb e e' =
  let teb = infer_base cenv tenv eb in
  let te = infer_base cenv tenv e in
  let te' = infer_base cenv tenv e' in
  let tret = newvar () in
  unify (TConstant TBool) teb;
  unify te tret;
  unify te' tret;
  tret

let default_tenv =
  (* Harcoding operators as functions (it is normally a full feature) *)
  StringDict.empty
  |> StringDict.add "( + )"
       (new_arrow (TConstant TInt)
          (new_arrow (TConstant TInt) (TConstant TInt)))
  |> StringDict.add "( * )"
       (new_arrow (TConstant TInt)
          (new_arrow (TConstant TInt) (TConstant TInt)))
  |> StringDict.add "( <= )"
       (new_arrow (TConstant TInt)
          (new_arrow (TConstant TInt) (TConstant TBool)))
  |> StringDict.add "( = )"
       ((fun () ->
          (* Ugly way to use the same var, but anyway... *)
          enter_level ();
          let x = newvar () in
          let t = new_arrow x (new_arrow x (TConstant TBool)) in
          exit_level ();
          gen t;
          t)
          ())

let default_cenv : constructors_env =
  let a_list =
    TConstructor
      ( [ TVar (ref (Unbound ("a", generic_level))) ],
        "list",
        { level_new = generic_level; level_old = generic_level } )
  in
  StringDict.empty
  |> StringDict.add "EmptyList" (a_list, [])
  |> StringDict.add "NodeList"
       (a_list, [ TVar (ref (Unbound ("a", generic_level))); a_list ])

(* Gives an env containing every variable of the code linked to their types *)
let infer (txt : string) : typ StringDict.t =
  let decs = Parse.parse txt in
  let (tenv, _) : env * constructors_env =
    List.fold_left
      (fun (env, cenv) -> function
        | Dexpr (r, x, e) ->
            (* Same as a "let .. in .." *)
            if r then (
              let ty_x = newvar () in
              enter_level ();
              let ty_e = infer_base cenv (StringDict.add x ty_x env) e in
              unify ty_e ty_x;
              exit_level ();
              gen ty_e;
              cycle_free ty_e;
              (StringDict.add x ty_e env, cenv))
            else (
              enter_level ();
              let ty_e = infer_base cenv env e in
              exit_level ();
              gen ty_e;
              cycle_free ty_e;
              (StringDict.add x ty_e env, cenv))
        | Dtype (ptyps, x, t) -> (
            match t with
            | TTempConstructor elts ->
                let constr_holder =
                  TConstructor
                    ( ptyps,
                      x,
                      { level_old = generic_level; level_new = generic_level }
                    )
                in
                ( env,
                  List.fold_left
                    (fun acc (n, l) -> StringDict.add n (constr_holder, l) acc)
                    cenv elts )
            | _ -> (env, cenv)))
      (default_tenv, default_cenv)
      decs
  in
  tenv
(* infer_base default_tenv (Parse.parse txt) *)

let infer_and_print txt = 
  let env = infer txt in     
  try
    StringDict.iter (fun x t ->
      Printf.printf "%s: " x; Util.print_typ t; print_newline ()
    ) env
  with NoUnifier (t1, t2) ->
    print_endline "No unifier:";
    Util.print_typ t1; print_string " <> "; Util.print_typ t2;
  print_newline ()
