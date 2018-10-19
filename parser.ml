open State

type op = Plus | Minus | Divide | Multiply
type expr = Binary of (expr * op * expr) | Unary of (op * expr) | Value of State.value

let parse_line (line : string) = 