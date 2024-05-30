open Ast 

let f () = 
  match Var "boom" with
  | _ -> assert false