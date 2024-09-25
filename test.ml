let is_digit c = int_of_char c >= int_of_char '0' && int_of_char c <= int_of_char '9'
let digit_to_int c = int_of_char c - int_of_char '0'

let ( @. ) f g x = f (g x) 

let rec get_digits = function
  | [] -> ([], [])
  | h :: t when is_digit h ->
    let digts, rest = get_digits t in 
    digit_to_int h :: digts, rest
  | l -> ([], l)

let list_to_int = List.fold_left (( + ) @. (( * ) 10)) 0
