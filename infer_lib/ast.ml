(** The type of the abstract syntax tree (AST). *)

type id = int 
type name = string
type char_place = int * int

(* type bop = 
  | Add 
  | Mult
  | Leq *)

type tconst =
  | TInt 
  | TBool
  | TUnit
  | TFloat

type typ =
  | TVar of int
  | TConst of tconst 
  | TArrow of typ * typ
  | TForall of id list * typ
  (* | TTimes of typ array *)

type located_typ = char_place * typ

type constructor = name * (typ option)
type enumerate_typ = constructor list

type defined_typ =
  | TDSimple of typ
  | TDEnumerate of enumerate_typ

type expr =
  | Var of name
  | App of expr * expr
  | Fun of name * expr
  | Int of int
  | Bool of bool
  | If of expr * expr * expr 
  | Let of name * expr * expr
  | Typdec of name * defined_typ * expr
  | Unitexpr
  (* | Binop of bop * expr * expr 
  | Couple of expr * expr
  | Fst of expr
  | Snd of expr
  | Left of expr
  | Right of expr
  | Match of expr * string * expr * string * expr *)
  

type located_expr = char_place * expr

