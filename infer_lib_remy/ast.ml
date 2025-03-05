type level = int

let generic_level = 100000000 (* as in OCaml typing/btype.ml *)
let marked_level = -1

type varname = string

type expr =
  | Var of varname
  | Fun of varname * expr
  | App of expr * expr
  | Int of int
  | Bool of bool
  | String of string
  | If of expr * expr * expr
  | Let of varname * expr * expr
  | Tuple of expr list

type qname = string

type typ =
  | TVar of tv ref
  | TConst of tconst
  | TArrow of typ * typ * levels
  | TTuple of typ list * levels
and tv = Unbound of qname * level | Link of typ
and levels = { mutable level_old : level; mutable level_new : level }
and tconst = TInt | TBool | TString
