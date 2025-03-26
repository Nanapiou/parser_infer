open Ast

module Env = Map.Make(String)


(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let decs: declaration list = Parser.prog Lexer.read lexbuf in
  match decs with
  | Dexpr (_, e) :: _ -> e 
  | _ -> assert false
