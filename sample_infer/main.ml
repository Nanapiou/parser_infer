open Infer

let file = open_in "./sample.txt"
let text = In_channel.input_all file

(* let () = Util.print_expr @@ Pase.parse text; print_newline () *)
let () = try
    Util.print_typ  @@ infer text
  with NoUnifier (t1, t2) ->
    print_endline "No unifier:";
    Util.print_typ t1; print_string " <> "; Util.print_typ t2

let () = print_newline ()

