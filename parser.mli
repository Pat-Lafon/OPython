open State

type op = Plus | Minus | Divide | Floor_Divide | Multiply | Modular | Exponent 
        | Equal | Not_Equal | Greater_Than | Less_Than | Greater_Equal 
        | Less_Equal | And | Or | Not | Complement

type expr = Binary of (expr * op * expr) | Unary of (op * expr) 
          | Value of State.value | Variable of string | List of expr list | Function of (string * expr list)

type line_type = Assignment | Expression | If of (expr * string) 
               | Empty | Else | Line of string | Elif of (expr * string) 
               | While of (expr * string)

exception SyntaxError of string
exception TypeError of string
exception NameError of string
exception OverflowError of string
exception IndentationError of string
exception ZeroDivisionError of string
exception EmptyInput
exception IfMultiline of (expr * string)
exception WhileMultiline of (expr * string)

val get_idx : string -> string -> int

val parse_line : string -> string option * expr

val line_type : string -> line_type

val parse_multiline : string -> line_type