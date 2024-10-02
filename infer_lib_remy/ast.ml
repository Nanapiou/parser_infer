type varname = string
type level = int

type expr =
  | Var of varname
  | Fun of varname * expr
  | App of expr * expr
  | Int of int
  | Bool of bool
  | If of expr * expr * expr 
  | Let of varname * expr * expr

type qname = string
type typ =
  | TVar of tv ref
  | QVar of qname
  | TConst of tconst
  | TArrow of typ * typ 
and tv = Unbound of qname * level | Link of typ
and tconst = TInt | TBool