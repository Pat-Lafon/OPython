open State

type op = Plus | Minus | Divide | Multiply | Modular | Exponent | Floor_Divide 
        | Equal | Not_Equal | And | Or | Not | Complement
type expr = Binary of (expr * op * expr) | Unary of (op * expr) | Value of State.value | Variable of string

val get_idx : string -> char -> int option

val is_assignment : string -> bool

val parse_expr_helper : string -> char * op -> expr

val parse_expr : string -> (char * op) list -> expr

val parse_assignment : string -> string option * expr

val parse_line : string -> string option * expr
