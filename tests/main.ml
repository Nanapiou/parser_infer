open Infer

let launch () =
  let tokenized_line = Util.tokenize_line "let x = 8 in x" in
  let rec loop = function
  | [] -> ()
  | (s, tok) :: t ->
    Printf.printf "%s -- " s;
    Util.print_token tok;
    loop t
  in
  loop tokenized_line;
  Util.print_tokenized_line tokenized_line

let () = launch ()