open Parser
open State

let rec evaluate (exp : expr) : value = 
  match exp with 
  | Binary (e1, op, e2) -> begin
      match op with 
        Plus -> (evaluate e1) + (evaluate e2)
                  Minus -> (evaluate e1) - (evaluate e2)
                             Divide -> (evaluate e1) / (evalaute e2)
                                         Multiply -> (evaluate e1) * (evaluate e2)


      | Unary (op, e1) -> Int (0)
      | Value (x) -> x