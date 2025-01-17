open Infer

let file = open_in "./patho.txt"
let text = In_channel.input_all file

(* let () = Util.print_expr @@ Pase.parse text; print_newline () *)
let () = try
    (* Util.print_typ *) ignore @@ infer text
  with NoUnifier (t1, t2) ->
    print_endline "No unifier:";
    Util.print_typ t1; print_string " <> "; Util.print_typ t2

