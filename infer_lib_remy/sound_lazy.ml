(*
  Simple Hindley-Milner type checker for pure lambda-calculus with let

  Explanation of the efficient generalization -- Remy algorithm

  The sound and even more efficient generalization with levels computed
  on demand,2 and the incremental occurs check and generalization.
*)

(* The language: lambda-calculus with let *)
type varname = string

type exp = 
  | Var of varname                      (* variable                *)
  | App of exp * exp                    (* application: e1 e2      *)
  | Lam of varname * exp                (* abstraction: fun x -> e *)
  | Let of varname * exp * exp          (* let x = e in e2         *)

(* The types to infer *)
(* Types without quantified variables are simple types;
   those containing quantified variables are type schemas.
   A quantified variable is a TVar whose level is generic_level.

   Since quantifiers are always on the outside in the HM system, 
   they are implied and not explicitly represented.

   Unlike sound_eager.ml, all types, not only type variables,
   have levels. Normally, the level of a composite type is
   an upper bound on the levels of its components. 
   If a type belongs to a region n, all its subcomponents should be alive when
   the region n is still alive. 
   However, levels are determined incrementally. Therefore,
   Composite types have two levels: level_old is always
   an upper bound for the levels of the components;
   level_new is equal or less than level_old. If
   level_new is less than level_old, the type is being promoted
   to a higher region. The type needed to be traversed and
   all of its components adjusted so their levels do not
   exceed level_new. Generalization will perform such an
   adjustment of levels for some types.

   During type traversals, level_new may have the value
   marked_level to signify that the type is being traversed.
   Encountering a type at the marked_level during the traversals
   means that we detect a cycle, created during unification
   without the occurs check.
*)
type level = int
let generic_level = 100_000_000           (* as in OCaml typing/btype.ml *)
let marked_level  = -1                  (* for marking a node, to check*)
                                        (* for cycles                  *)
type typ =
  | TVar of tv ref
  | TArrow of typ * typ * levels
and tv = Unbound of string * level | Link of typ
and levels = {mutable level_old : level; mutable level_new : level}

(* Chase the links of bound variables, returning either
   a free variable or a constructed type.
   OCaml's typing/btype.ml has the same function with the same name.
   Unlike OCaml, we do path compression.
*)
let rec repr : typ -> typ = function
  | TVar ({contents = Link t} as tvr) -> 
      let t = repr t in
      tvr := Link t; t
  | t -> t

(* get the level of a normalized type, which is not a bound TVar *)
let get_level : typ -> level = function
  | TVar {contents = Unbound (_,l)} -> l
  | TArrow (_,_,ls)                 -> ls.level_new
  | _ -> assert false

let gensym_counter = ref 0
let reset_gensym : unit -> unit =
  fun () -> gensym_counter := 0

let gensym : unit -> string = fun () ->
  let n = !gensym_counter in
  let () = incr gensym_counter in
  if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
            else "t" ^ string_of_int n

(* Determining the |let|-nesting level during the type-checking, 
   or just the _level_.
   Each top-level expression to type-check is implicitly wrapped into a let.
   So, the numbering starts with 1.
*)
let current_level = ref 1
let reset_level () = current_level := 1

let reset_type_variables () =       (* name from OCaml's typing/typetext.ml *)
  reset_gensym ();
  reset_level ()

(* Increase level *)
let enter_level () =
  incr current_level
(* Restore level *)
let leave_level () =
  decr current_level


(* Make a fresh type variable and an arrow type *)
let newvar (): typ = TVar (ref (Unbound (gensym (), !current_level)))

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
  | TVar {contents = Unbound _} -> ()
  | TVar {contents = Link ty}   -> cycle_free ty
  | TArrow (_,_,ls) when ls.level_new = marked_level -> failwith "occurs check"
  | TArrow (t1,t2,ls) ->
      let level = ls.level_new in
      ls.level_new <- marked_level;
      cycle_free t1;
      cycle_free t2;
      ls.level_new <- level

(* Main unification *)
(* Quantified variables are unexpected: they should've been instantiated *)
(* The occurs check is lazy; therefore, cycles could be created
   accidentally. We have to watch for them.
*)

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
  | _ -> failwith "Update levels"


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
    | _ -> failwith "Cannot unify"
  (* everything else is the unification error *)

and unify_lev l ty1 ty2 =
  let ty1 = repr ty1 in
  update_level l ty1;
  unify ty1 ty2

(* The type environment *)
type env = (varname * typ) list

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
        failwith "occurs check"
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
    List.fold_left adjust_one [] !to_be_level_adjusted;
;;


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
;;

(* instantiation: replace schematic variables with fresh TVars.
   Only the components at generic_level are traversed, since only
   those may contain quantified type variables.
*)
let inst : typ -> typ = 
  let rec loop subst = function
    | TVar {contents = Unbound (name,l)} when
        l = generic_level -> 
        begin
          try (List.assoc name subst, subst)
          with Not_found ->
            let tv = newvar () in
            (tv, (name,tv)::subst)
        end
    | TVar {contents = Link ty} -> loop subst ty
    | TArrow (ty1,ty2,ls) when ls.level_new = generic_level ->
        let (ty1,subst) = loop subst ty1 in
        let (ty2,subst) = loop subst ty2 in
        (new_arrow ty1 ty2, subst)
    | ty -> (ty, subst)
  in fun ty -> fst (loop [] ty)
;;


(* Trivial type checker. Type checking errors are delivered
   as exceptions
*)
let rec typeof : env -> exp -> typ = fun env -> function
  | Var x     -> inst (List.assoc x env)
  | Lam (x,e) -> 
      let ty_x = newvar () in
      let ty_e = typeof ((x,ty_x)::env) e in
      new_arrow ty_x ty_e
  | App (e1,e2) ->
      let ty_fun = typeof env e1 in
      let ty_arg = typeof env e2 in
      let ty_res = newvar () in
      unify ty_fun (new_arrow ty_arg ty_res);
      ty_res
  | Let (x,e,e2) -> 
      enter_level ();
      let ty_e = typeof env e in
      leave_level ();
      gen ty_e;
      typeof ((x,ty_e)::env) e2
;;


(* Type-check the top-level expression *)
let top_type_check : exp -> typ = fun exp ->
  reset_type_variables ();
  reset_level_adjustment ();
  let ty = typeof [] exp in
  cycle_free ty;
  ty
;;


(* tests *)
(* partial pattern-match is a part of the test *)
[@@@ warning "-8"]

let id = Lam("x",Var"x");;
let c1 = Lam("x",Lam("y",App (Var"x",Var"y")));;

let 
 TArrow (TVar {contents = Unbound ("a", 1)},
 TVar {contents = Unbound ("a", 1)}, {level_old = 1; level_new = 1})
   = 
  top_type_check id
;;

let 
 TArrow
 (TVar
   {contents =
     Link
      (TArrow (TVar {contents = Unbound ("b", 1)},
        TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}))},
 TArrow (TVar {contents = Unbound ("b", 1)},
  TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}),
 {level_old = 1; level_new = 1})
 =
   top_type_check c1
;;

let 
 TArrow
 (TArrow (TVar {contents = Unbound ("d", 1)},
   TVar {contents = Unbound ("e", 1)}, {level_old = 1; level_new = 1}),
 TArrow (TVar {contents = Unbound ("d", 1)},
  TVar {contents = Unbound ("e", 1)}, {level_old = 1; level_new = 1}),
 {level_old = 1; level_new = 1})
 =
 top_type_check (Let ("x",c1,Var"x"));;

let 
 TArrow (TVar {contents = Unbound ("b", 1)},
  TVar {contents = Unbound ("b", 1)}, {level_old = 1; level_new = 1})
 =
 top_type_check (Let ("y",Lam ("z",Var"z"), Var"y"));;

let
 TArrow (TVar {contents = Unbound ("a", 1)},
 TArrow (TVar {contents = Unbound ("c", 1)},
  TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}),
 {level_old = 1; level_new = 1})
 =
 top_type_check (Lam ("x", Let ("y",Lam ("z",Var"z"), Var"y")));;

let
 TArrow (TVar {contents = Link (TVar {contents = Unbound ("c", 1)})},
 TVar {contents = Link (TVar {contents = Unbound ("c", 1)})},
 {level_old = 1; level_new = 1})
 =
 top_type_check (Lam ("x", Let ("y",Lam ("z",Var"z"), 
                                    App (Var"y",Var"x"))));;

try 
 let _ = top_type_check (Lam ("x",App (Var"x",Var"x"))) in
 assert false;
 with Failure e -> print_endline e
;;

try 
 let _ = top_type_check (Let ("x",Var"x",Var"x")) in
 assert false
 with Not_found -> print_endline "unbound var"
;;

(* Max Heiber's example *)
(* fun y -> y (fun z -> y z) *)
try 
  let _ =
    top_type_check (Lam ("y", App (Var"y",(Lam ("z", (App (Var"y", Var"z")))))))
 in assert false
 with Failure e -> print_endline  e
;;

(* The problem pointed out by kirang
  fun x -> fun y -> fun k -> k (k x y) (k y x)
 no longer occurs here, since we do path compression
*)
let 
 TArrow (TVar {contents = Unbound ("a", 1)},
 TArrow (TVar {contents = Link (TVar {contents = Unbound ("a", 1)})},
  TArrow
   (TVar
     {contents =
       Link
        (TArrow (TVar {contents = Unbound ("a", 1)},
          TVar
           {contents =
             Link
              (TArrow
                (TVar {contents = Link (TVar {contents = Unbound ("a", 1)})},
                TVar {contents = Link (TVar {contents = Unbound ("a", 1)})},
                {level_old = 1; level_new = 1}))},
          {level_old = 1; level_new = 1}))},
   TVar {contents = Link (TVar {contents = Unbound ("a", 1)})},
   {level_old = 1; level_new = 1}),
  {level_old = 1; level_new = 1}),
 {level_old = 1; level_new = 1})
 =
 top_type_check (Lam ("x", Lam ("y", Lam ("k",
     App (App (Var "k", App (App (Var "k", Var "x"), Var "y")), 
          App (App (Var "k", Var "y"), Var "x"))
                                 ))))

(* id can be `self-applied', on the surface of it *)
let 
 TVar
 {contents =
   Link
    (TArrow (TVar {contents = Unbound ("c", 1)},
      TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}))}
 =
 top_type_check (Let ("id",id, App (Var"id",Var"id")));;

let 
TArrow (TVar {contents = Unbound ("i", 1)},
 TVar {contents = Unbound ("i", 1)}, {level_old = 1; level_new = 1})
 =
 top_type_check (Let ("x",c1,
                    Let ("y",
                          Let ("z",App(Var"x",id), Var "z"),
                         Var"y")));;

(*
fun x -> fun y -> let x = x y in fun x -> y x;;
- : (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'a -> 'b = <fun>
*)
let 
TArrow
 (TVar
   {contents =
     Link
      (TArrow
        (TVar
          {contents =
            Link
             (TArrow (TVar {contents = Unbound ("d", 1)},
               TVar {contents = Unbound ("e", 1)},
               {level_old = 1; level_new = 1}))},
        TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}))},
 TArrow
  (TVar
    {contents =
      Link
       (TArrow (TVar {contents = Unbound ("d", 1)},
         TVar {contents = Unbound ("e", 1)}, {level_old = 1; level_new = 1}))},
  TArrow (TVar {contents = Unbound ("d", 1)},
   TVar {contents = Unbound ("e", 1)}, {level_old = 1; level_new = 1}),
  {level_old = 1; level_new = 1}),
 {level_old = 1; level_new = 1})
 =
 top_type_check (Lam ("x", Lam("y",Let ("x",App (Var"x",Var"y"),
                                  Lam ("x",App (Var"y",Var"x"))))));;

(* now sound generalization ! *)
let
TArrow (TVar {contents = Unbound ("a", 1)},
 TVar {contents = Unbound ("a", 1)}, {level_old = 1; level_new = 1})
 =
 top_type_check (Lam ("x", Let ("y",Var"x", Var"y")));;

(* now sound generalization ! *)
let 
TArrow (TVar {contents = Unbound ("a", 1)},
 TArrow (TVar {contents = Unbound ("c", 1)},
  TVar {contents = Unbound ("a", 1)}, {level_old = 1; level_new = 1}),
 {level_old = 1; level_new = 1})
 =
 top_type_check (Lam ("x", Let ("y",Lam ("z",Var"x"), Var"y")));;

(* now sound generalization ! *)
let
TArrow
 (TVar
   {contents =
     Link
      (TArrow (TVar {contents = Unbound ("b", 1)},
        TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}))},
 TArrow (TVar {contents = Unbound ("b", 1)},
  TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}),
 {level_old = 1; level_new = 1})
 =
 top_type_check (Lam ("x", Let ("y",Lam ("z",App (Var"x",Var"z")), Var"y")));;


(* now sound generalization ! *)
let 
TArrow
 (TVar
   {contents =
     Link
      (TArrow (TVar {contents = Unbound ("b", 1)},
        TVar
         {contents =
           Link
            (TArrow (TVar {contents = Unbound ("b", 1)},
              TVar {contents = Unbound ("d", 1)},
              {level_old = 1; level_new = 1}))},
        {level_old = 1; level_new = 1}))},
 TArrow (TVar {contents = Unbound ("b", 1)},
  TVar {contents = Unbound ("d", 1)}, {level_old = 1; level_new = 1}),
 {level_old = 1; level_new = 1})
 =
 top_type_check (Lam ("x", Lam("y",Let ("x",App (Var"x",Var"y"),
                                    App (Var"x",Var"y")))));;



(* now sound generalization ! *)
try 
 let _ = top_type_check (Lam ("x",Let("y",Var"x", App (Var"y",Var"y")))) in
 assert false
 with Failure e -> print_endline e
;;

(* now sound generalization ! *)
(* fun x -> let y = let z = x (fun x -> x) in z in y;;
   - : (('a -> 'a) -> 'b) -> 'b = <fun>
*)
let
TArrow
 (TVar
   {contents =
     Link
      (TArrow
        (TArrow (TVar {contents = Unbound ("b", 1)},
          TVar {contents = Unbound ("b", 1)}, {level_old = 1; level_new = 1}),
        TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1}))},
 TVar {contents = Unbound ("c", 1)}, {level_old = 1; level_new = 1})
 =
 top_type_check (Lam ("x",
                    Let ("y",
                          Let ("z",App(Var"x",id), Var "z"),
                          Var"y")));;


print_endline "\nAll Done\n";;
