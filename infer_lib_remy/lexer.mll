{
open Parser
}

let white = [' ' '\t' '\n']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | ['0'-'9'] | '_')*
let digit = '-'? ['0'-'9']+
let string = '"' [^'"']* '"'

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
  | "," { COMA }
  (* | "fst" { FST }
  | "snd" { SND }
  | "Left" { LEFT }
  | "Right" { RIGHT }
  | "match" { MATCH }
  | "with" { WITH } *)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "rec" { REC }
  | "=" { EQUALS }
  | "in" { IN }
  | ";" { SEMICOLON }
  | "type" { TYPE }
  | "int" { INT_TYPE }
  | "string" { STRING_TYPE }
  | "unit" { UNIT_TYPE }
  | "bool" { BOOL_TYPE }
  | string { STRING (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf) - 2)) }
  | digit { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
