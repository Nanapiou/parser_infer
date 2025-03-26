open Ast

module Env = Map.Make(String)


(** [parse s] parses [s] into a list of declarations. *)
let parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let decs: declaration list = Parser.prog Lexer.read lexbuf in
  decs
