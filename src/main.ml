open Infer
open TypUtil

let () = print_typ @@ infer "let f = fun x -> x in if f true then 0 else f 666"