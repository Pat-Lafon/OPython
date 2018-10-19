open Parser
open State

let helper_plus = function 
  | (Int (x), Int(y)) -> Int(x+y)
  | (String (x), String (y)) -> String (x ^ y)
  | _ -> failwith "wrong types"

let helper_minus = function 
  | (Int (x), Int(y)) -> Int(x-y)
  | _ -> failwith "wrong types"

let helper_multiply = function 
  | (Int (x), Int(y)) -> Int (x * y)
  | _ -> failwith "wrong types"

let helper_divide = function 
  | (Int (x), Int(y)) -> Int(int_of_float (floor ((float_of_int x)/.(float_of_int y))))
  | _ -> failwith "wrong types"

let rec evaluate (exp : expr) (st: State.t): value = 
  match (exp, st) with 
  | (Binary (e1, op, e2), st) -> begin 
      match op with 
      | Plus -> helper_plus (evaluate e1, evaluate e2)
      | Minus -> helper_minus (evaluate e1, evaluate e2)
      | Multiply -> helper_multiply (evaluate e1, evaluate e2)
      | Divide -> helper_divide (evaluate e1, evaluate e2)
    end
  | Unary (op, e1), st -> begin 
      match (op,evaluate e1 st) with 
      | (Plus, Int (x)) -> Int (x)
      | (Minus, Int (x)) -> Int (-x)
      | _ -> failwith "wrong types"
    end
  | Variable (x), st -> evaluate (State.find x st) (st)
  | Value (x), st -> x

(* let eval input st = match input with
   | Some s, expr -> insert s (evaluate expr) st
   | None, expr -> print (evaluate expr)
*)
