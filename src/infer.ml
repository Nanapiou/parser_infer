open Ast 

let () = 
  match Var "boom" with
  | _ -> assert false