type value = Int of int | Float of float | String of string | Bool of bool | List of value list
type t = (string*value) list

val empty : t 
(*
val is_empty : t -> bool 
*)
val insert : string -> value -> t -> t

val find : string -> t -> value option

val member : string -> t -> bool