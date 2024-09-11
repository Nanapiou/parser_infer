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
%token TYPE
%token INT_TYPE
%token FLOAT_TYPE
%token UNIT_TYPE
%token BOOL_TYPE
%token <int> INT 
%token <string> ID
%token <string> CAPID
%token LPAREN RPAREN
%token ADD MULT LEQ
%token FUN ARROW
%token TRUE FALSE
// %token COMA
// %token FST SND
// %token LEFT RIGHT
// %token MATCH WITH
%token VERTBAR
%token IF THEN ELSE
%token LET EQUALS IN
%token UNIT

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
	| FUN; x = ID; ARROW; e = expr { Fun (x, e) }
	| LET; id = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (id, e1, e2) }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
	| TYPE; id = ID; EQUALS; t = type_dec { Typdec (id, t) }
	;

type_dec:
	| BOOL_TYPE { TDSimple (TConst TBool) }
	| INT_TYPE { TDSimple (TConst TInt) }
	| UNIT_TYPE { TDSimple (TConst TUnit) }
	| UNIT_TYPE { TDSimple (TConst TFloat) }
	;

simpl_expr:
	| UNIT { Unitexpr }
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
	| x = ID { Var x }
  ;
