{
open Parser
}

let white = [' ' '\t' '\n']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let digit = '-'? ['0'-'9']+

rule read = 
  parse
  | white { read lexbuf }
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
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | ";" { SEMICOLON }
  | digit { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }