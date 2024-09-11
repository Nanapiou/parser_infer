open Infer 

let () = TypUtil.print_typ @@ infer "let f = fun x -> x in if f true then f 0 else 2"