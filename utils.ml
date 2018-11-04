let rec print_list_helper lst =
  match lst with
  | [] -> "END" 
  | h::t -> (h ^ "," ^ (print_list_helper t))

let print_list lst = print_endline (print_list_helper lst)

let rec create_int_list n =
  match n with
  | 0 -> []
  | t -> t::(create_int_list (n-1))
