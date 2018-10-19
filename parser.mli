open State

type op = Plus | Minus | Divide | Multiply
type expr = Binary of (expr * op * expr) | Unary of (op * expr) | Value of State.value | Variable of string

val get_idx : string -> char -> int option

val is_assignment : string -> bool

val parse_expr : string -> expr

val parse_assignment : string -> string option * expr
