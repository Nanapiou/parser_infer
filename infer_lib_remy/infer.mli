module StringDict :
  sig
    type key = string
    type 'a t = 'a Parse.Env.t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
module Util = Util
module Parse = Parse
module Lexer = Lexer
module Parser = Parser
exception NoUnifier of Ast.typ * Ast.typ
exception OccurCheck of Ast.typ
type env = Ast.typ StringDict.t
val current_level : int ref
val enter_level : unit -> unit
val exit_level : unit -> unit
val counter_sym : int ref
val reset_sym : unit -> unit
val get_sym : unit -> string
val newvar : unit -> Ast.typ
val new_arrow : Ast.typ -> Ast.typ -> Ast.typ
val new_tuple : Ast.typ list -> Ast.typ
val cycle_free : Ast.typ -> unit
val to_be_level_adjusted : Ast.typ list ref
val reset_level_adjustment : unit -> unit
val update_level : int -> Ast.typ -> unit
val force_delayed_adjustments : unit -> unit
val gen : Ast.typ -> unit
val inst : Ast.typ -> Ast.typ
val unify : Ast.typ -> Ast.typ -> unit
val unify_lev : int -> Ast.typ -> Ast.typ -> unit
val infer_base : env -> Ast.expr -> Ast.typ
val infer_fun : env -> string -> Ast.expr -> Ast.typ
val infer_tuple : env -> Ast.expr list -> Ast.typ
val infer_let : env -> string -> Ast.expr -> Ast.expr -> Ast.typ
val infer_app : env -> Ast.expr -> Ast.expr -> Ast.typ
val infer_if : env -> Ast.expr -> Ast.expr -> Ast.expr -> Ast.typ
val default_tenv : Ast.typ StringDict.t
val infer : string -> Ast.typ StringDict.t
