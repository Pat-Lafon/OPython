open State

type op = Plus | Minus | Divide | Floor_Divide | Multiply | Modular | Exponent 
        | Equal | Not_Equal | And | Or | Not | Complement

type expr = Binary of (expr * op * expr) | Unary of (op * expr) | Value of State.value | Variable of string | List of expr list

exception SyntaxError of string
exception TypeError of string
exception NameError of string
exception OverflowError of string
exception IndentationError of string
exception ZeroDivisionError of string
exception EmptyInput
exception Multiline of (expr * string)

type line_type = Assignment | Expression | If of (expr * string) 
                | Empty | Else | Line of string | Elif of (expr * string)
(* type line_type = Assignment | Expression | If | While | Elif | Else | Empty *)

(*
val get_idx : string -> string -> int option

val is_assignment : string -> bool

val parse_expr_helper : string -> char * op -> expr

val parse_expr : string -> (string * op) list -> expr

val parse_assignment : string -> string option * expr
*)
val parse_line : string -> string option * expr

val line_type : string -> line_type

val parse_multiline : string -> line_type