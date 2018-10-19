type value = Int of int| String of string | Var of string
type t = (string*value)list

let empty : t = []

let is_empty (d:t) : bool = (d = [])

let rec insert (k:string) (v:value) (d:t) : t = 
  if List.mem_assoc k d then insert k v (List.remove_assoc k d)
  else (k,v)::d

let find (k:string) (d:t) = 
  try Some (List.assoc k d) with
  | Not_found -> None

let member (k:string) (d:t) : bool = List.mem_assoc k d