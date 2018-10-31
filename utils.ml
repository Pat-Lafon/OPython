let rec print_list_helper lst =
  match lst with
  | [] -> "END" 
  | h::t -> (h ^ "," ^ (print_list_helper t))

let print_list lst = print_endline (print_list_helper lst)