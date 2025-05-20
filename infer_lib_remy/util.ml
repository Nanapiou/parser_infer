open Ast
open Parser

let print_token = function
  | TRUE -> print_endline "TRUE"
  | THEN -> print_endline "THEN"
  | SEMICOLON -> print_endline "SEMICOLON"
  | RPAREN -> print_endline "RPAREN"
  | MULT -> print_endline "MULT"
  | LPAREN -> print_endline "LPAREN"
  | LET -> print_endline "LET"
  | LEQ -> print_endline "LEQ"
  | INT i -> Printf.printf "INT %d\n" i
  | IN -> print_endline "IN"
  | IF -> print_endline "IF"
  | ID s -> Printf.printf "ID %s\n" s
  | TID s -> Printf.printf "TID %s\n" s
  | CONSTRUCTOR s -> Printf.printf "CONSTRUCTOR %s\n" s
  | FUN -> print_endline "FUN"
  | FALSE -> print_endline "FALSE"
  | EQUALS -> print_endline "EQUALS"
  | EOF -> print_endline "EOF"
  | ELSE -> print_endline "ELSE"
  | ARROW -> print_endline "ARROW"
  | ADD -> print_endline "ADD"
  | MATCH -> print_endline "MATCH"
  | WITH -> print_endline "WITH"
  | VERTBAR -> print_endline "VERTBAR"
  | OF -> print_endline "OF"
  | UNIT -> print_endline "UNIT"
  | STRING s -> Printf.printf "STRING %s\n" s
  | COMA -> print_endline "COMA"
  | TYPE -> print_endline "TYPE"
  | UNIT_TYPE -> print_endline "UNIT_TYPE"
  | STRING_TYPE -> print_endline "STRING_TYPE"
  | INT_TYPE -> print_endline "INT_TYPE"
  | BOOL_TYPE -> print_endline "BOOL_TYPE"
  | REC -> print_endline "REC"

let rec print_typ =
  let open Printf in
  function
  | TConstant (TInt) -> print_string "TInt"
  | TConstant (TBool) -> print_string "TBool"
  | TConstant (TString) -> print_string "TString"
  | TConstant (TUnit) -> print_string "TUnit"
  (* | QVar x -> printf "QVar %s" x *)
  | TVar ({ contents = Unbound (x, l) }) -> printf "%s.%d" x l
  | TVar ({ contents = Link t }) -> print_typ t
  | TArrow (t1, t2, {level_old; level_new}) ->
    Printf.printf "((%d,%d) " level_old level_new;
    print_typ t1;
    print_string " -> ";
    print_typ t2;
    print_char ')'
  | TTuple (l, {level_old; level_new}) ->
    Printf.printf "((%d,%d) " level_old level_new;
    print_typ (List.hd l);
    List.iter (fun t -> print_string ", "; print_typ t) (List.tl l);
    print_char ')'
  | TConstructor (l, x, {level_old; level_new}) ->
    Printf.printf "(%d,%d) " level_old level_new;
    if l <> [] then begin
      print_typ (List.hd l);
      List.iter (fun t -> print_string ", "; print_typ t) (List.tl l);
      print_char ' ';
    end;
    print_string x
  | TTempConstructor _ -> failwith "print_typ error"

let string_of_type ty =
  let open Printf in
  let buf = Buffer.create 64 in

  let rec aux = function
    | TConstant TInt -> Buffer.add_string buf "TInt"
    | TConstant TBool -> Buffer.add_string buf "TBool"
    | TConstant TString -> Buffer.add_string buf "TString"
    | TConstant TUnit -> Buffer.add_string buf "TUnit"
    (* | QVar x -> bprintf buf "QVar %s" x *)
    | TVar { contents = Unbound (x, l) } ->
        bprintf buf "%s.%d" x l
    | TVar { contents = Link t } ->
        aux t
    | TArrow (t1, t2, { level_old; level_new }) ->
        bprintf buf "((%d,%d) " level_old level_new;
        aux t1;
        Buffer.add_string buf " -> ";
        aux t2;
        Buffer.add_char buf ')'
    | TTuple (l, { level_old; level_new }) ->
        bprintf buf "((%d,%d) " level_old level_new;
        (match l with
        | [] -> ()
        | h :: t ->
            aux h;
            List.iter (fun t ->
              Buffer.add_string buf ", ";
              aux t
            ) t);
        Buffer.add_char buf ')'
    | TConstructor (l, x, {level_old; level_new}) -> 
        bprintf buf "(%d,%d) " level_old level_new;
        (match l with
        | [] -> ()
        | h :: t ->
            aux h;
            List.iter (fun t ->
              Buffer.add_string buf ", ";
              aux t
            ) t);
        Buffer.add_char buf ' ';
        Buffer.add_string buf x
    | TTempConstructor _ -> failwith "string of type error"
  in
  aux ty;
  Buffer.contents buf


let rec print_expr = function
  | Var x -> print_string x
  | App (e1, e2) ->
    print_string "(";
    print_expr e1;
    print_string ") (";
    print_expr e2;
    print_char ')'
  | Fun (x, e) ->
    Printf.printf "fun %s -> (" x;
    print_expr e;
    print_char ')'
  | Int i -> print_int i
  | Bool b -> print_string (string_of_bool b)
  | String s -> Printf.printf "\"%s\"" s
  | Match (e, l) ->
    print_string "match ";
    print_expr e;
    print_string " with";
    List.iter (fun (e1, e2) -> print_char '\n'; print_expr e1; print_string " -> "; print_expr e2) l
  | If (e1, e2, e3) ->
    print_string "if (";
    print_expr e1;
    print_string ") then (";
    print_expr e2;
    print_string ") else (";
    print_expr e3;
    print_char ')'
  | Let (r, x, e1, e2) ->
    if r then Printf.printf "let rec %s = (" x else Printf.printf "let %s = (" x;
    print_expr e1;
    print_string ") in (";
    print_expr e2;
    print_char ')'
  | Tuple l ->
    print_char '(';
    print_expr (List.hd l);
    List.iter (fun t -> print_string ", "; print_expr t) (List.tl l);
    print_char ')'
  | Constructor (x, l) ->
    print_string x;
    if l <> [] then begin
      print_string " (";
      print_expr (List.hd l);
      List.iter (fun t -> print_string ", "; print_expr t) (List.tl l);
      print_char ')'
    end
  | Unit -> print_string "()"

let rec repr = function
  | TVar ({ contents = Link t } as tvr) ->
    let t = repr t in 
    tvr := Link t;
    t
  | t -> t


(* get the level of a normalized type, which is not a bound TVar *)
let get_level : typ -> level = function
  | TVar {contents = Unbound (_, l)} -> l
  | TArrow (_, _, ls) | TTuple (_, ls) | TConstructor (_, _, ls) -> ls.level_new
  | TConstant _ -> 0
  | (TVar _ as t) ->
     print_typ t;
     failwith ", get_level: not a normalized type"
  | TTempConstructor _ -> failwith "get_level error"

let tokenize_line s =
  let lexbuf = Lexing.from_string s in
  let rec loop () =
    let tok = Lexer.read lexbuf in
    let s = Lexing.lexeme lexbuf in
    if tok <> EOF then (s, tok) :: loop () else []
  in
  loop ()

let print_tokenized_line l =
  List.iter (Fun.compose (Printf.printf "%s ") fst) l

let rec iter_double f = function
  | [] | [_] -> ()
  | h1 :: h2 :: t -> f h1 h2; iter_double f t
