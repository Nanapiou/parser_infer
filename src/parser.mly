(* This file uses some advanced parsing techniques
   to parse juxtaposed applications [e1 e2 e3] the
	 same way as OCaml does. *)

%{
open Ast

(** [make_apply e [e1; e2; ...]] makes the application  
    [e e1 e2 ...]).  Requires: the list argument is non-empty. *)
let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> App (e, e')
	| h :: ((_ :: _) as t) -> make_apply (App (e, h)) t
%}

%token EOF
%token <int> INT 
%token <string> ID
%token LPAREN RPAREN
%token ADD MULT LEQ
%token FUN ARROW
%token TRUE FALSE
%token COMA
%token FST SND
%token LEFT RIGHT
%token MATCH WITH VERTBAR
%token IF THEN ELSE
%token LET EQUALS IN

%left ADD 
%left MULT 
%left LEQ

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr:
	| e = simpl_expr { e }
	| e = simpl_expr; es = simpl_expr+ { make_apply e es }
	| FST; e = simpl_expr { Fst (e) }
	| SND; e = simpl_expr { Snd (e) }
	| LEFT; e = simpl_expr { Left (e) }
	| RIGHT; e = simpl_expr { Right (e) }
	| FUN; x = ID; ARROW; e = expr { Fun (x, e) }
	| e1 = expr; ADD; e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr; LEQ; e2 = expr { Binop (Leq, e1, e2) }
	| e1 = expr; MULT; e2 = expr { Binop (Mult, e1, e2) }
	| LET; id = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (id, e1, e2) }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
	| MATCH; e = expr; WITH; VERTBAR?; LEFT; x1 = ID; ARROW; e1 = expr; VERTBAR; RIGHT; x2 = ID; ARROW; e2 = expr { Match (e, x1, e1, x2, e2) }
	;

simpl_expr:
  	| LPAREN; e1 = expr; COMA; e2 = expr; RPAREN { Couple (e1, e2) } 
  	| LPAREN; e = expr; RPAREN { e } 
	| TRUE { Bool true }
	| FALSE { Bool false }
	| i = INT { Int i }
	| x = ID { Var x }
  ;
