{
open Parser
}

let white = [' ' '\t' '\n']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let capid = ['A'-'Z'] letter*
let digit = '-'? ['0'-'9']+

rule read = 
  parse
  | white { read lexbuf }
  | "type" { TYPE }
  | "int" { INT_TYPE }
  | "float" { FLOAT_TYPE }
  | "unit" { UNIT_TYPE }
  | "bool" { BOOL_TYPE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { ADD }
  | "*" { MULT }
  | "<=" { LEQ }
  | "fun" { FUN }
  | "->" { ARROW }
  | "true" { TRUE }
  | "false" { FALSE }
  (* | "," { COMA }
  | "fst" { FST }
  | "snd" { SND }
  | "Left" { LEFT }
  | "Right" { RIGHT }
  | "match" { MATCH }
  | "with" { WITH } *)
  | "|" { VERTBAR } 
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "=" { EQUALS }
  | "of" { OF }
  | "in" { IN }
  | ";" { SEMICOLON }
  | "()" { UNIT }
  | digit { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | capid { CAPID (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }