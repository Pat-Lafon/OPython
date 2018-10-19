open Parser
open State

let rec evaluate (exp : expr) : value = 
  match exp with 
  | Binary (e1, op, e2) -> Int (0)
  | Unary (op, e1) -> Int (0)
  | Value (x) -> x