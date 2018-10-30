type value = Int of int | Float of float | String of string 
           | Bool of bool | VList of value list | Function of (string list * string)

type t = (string*value) list

let empty : t = []

let rec insert (k:string) (v:value) (d:t) : t = 
  if List.mem_assoc k d then (k,v)::(List.remove_assoc k d)
  else (k,v)::d

let find (k:string) (d:t) : value option = 
  try Some (List.assoc k d) with
  | Not_found -> None

let member (k:string) (d:t) : bool = List.mem_assoc k d