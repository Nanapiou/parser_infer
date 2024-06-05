open Ast

(** [Env] is module to help with environments, which 
are maps that have strings as keys. *)
module Env = Map.Make(String)

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
