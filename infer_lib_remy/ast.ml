type level = int

let generic_level = 100_000_000 (* as in OCaml typing/btype.ml *)
let marked_level = -1

type varname = string

type expr =
  | Var of varname
  | Fun of varname * expr
  | App of expr * expr
  | Int of int
  | Bool of bool
  | String of string
  | Match of expr * (expr * expr) list
  | Constructor of string * expr list
  | If of expr * expr * expr
  | Let of bool * varname * expr * expr
  | Tuple of expr list
  | Unit

type qname = string

type typ =
  | TVar of tv ref
  | TConstant of tconstant
  | TArrow of typ * typ * levels
  | TTuple of typ list * levels
  | TConstructor of typ list * string * levels
  | TTempConstructor of typ list * (string * typ list) list (* Used when parsing in order to build the constructors' env *)
and tv = Unbound of qname * level | Link of typ
and levels = { mutable level_old : level; mutable level_new : level }
and tconstant = TInt | TBool | TString | TUnit

type declaration =
  | Dexpr of bool * varname * expr
  | Dtype of varname * typ
