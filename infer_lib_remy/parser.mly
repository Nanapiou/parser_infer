%{
open Ast

(** [make_apply e [e1; e2; ...]] makes the application  
    [e e1 e2 ...]).  Requires: the list argument is non-empty. *)
let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> App (e, e')
  | h :: ((_ :: _) as t) -> make_apply (App (e, h)) t
%}

%token SEMICOLON
%token EOF
%token <int> INT 
%token <string> ID
%token <string> STRING


%token LPAREN RPAREN
%token ADD MULT LEQ
%token FUN ARROW
%token TRUE FALSE
// %token COMA
// %token FST SND
// %token LEFT RIGHT
// %token MATCH WITH
// %token VERTBAR OF
%token IF THEN ELSE
%token LET EQUALS IN
// %token UNIT

%left ADD 
%left MULT
// %right ARROW
%left LEQ

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| e = simpl_expr { e }
	| e = simpl_expr; es = simpl_expr+ { make_apply e es }
	| FUN; x = ID; ARROW; e = expr { Fun (x, e) }
	| LET; id = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (id, e1, e2) }
	| LET; id = ID; EQUALS; e1 = expr; SEMICOLON; SEMICOLON; e2 = expr { Let (id, e1, e2) }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
	;

simpl_expr:
	| LPAREN; e = expr; RPAREN { e }
	| LPAREN; ADD; RPAREN { Var ("( + )") }
	| LPAREN; MULT; RPAREN { Var ("( * )") }
	| LPAREN; LEQ; RPAREN { Var ("( <= )") }
	| e1 = simpl_expr; ADD; e2 = simpl_expr { App (App (Var ("( + )"), e1), e2) }
	| e1 = simpl_expr; LEQ; e2 = simpl_expr { App (App (Var ("( <= )"), e1), e2) }
	| e1 = simpl_expr; MULT; e2 = simpl_expr { App (App (Var ("( * )"), e1), e2) }
  	
	| TRUE { Bool true }
	| FALSE { Bool false }
	| i = INT { Int i }
	| s = STRING { String s }
	| x = ID { Var x }
	;
