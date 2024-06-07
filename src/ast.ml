(** The type of the abstract syntax tree (AST). *)

type id = int 
type name = string

type bop = 
  | Add 
  | Mult
  | Leq

type expr =
  | Var of string
  | App of expr * expr
  | Fun of string * expr
  | Int of int
  | Bool of bool
  (* | Binop of bop * expr * expr 
  | Couple of expr * expr
  | Fst of expr
  | Snd of expr
  | Left of expr
  | Right of expr
  | Match of expr * string * expr * string * expr *)
  | If of expr * expr * expr 
  | Let of string * expr * expr

type tconst =
  | TInt 
  | TBool

type typ =
  | TVar of int
  | TConst of tconst 
  | TArrow of typ * typ
  | TForall of id list * typ