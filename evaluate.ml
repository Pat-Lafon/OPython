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

let rec eval (exp : expr) (st : State.t) : value = 
  match (exp,st) with 
  | (Binary (e1, op, e2), st) -> begin 
      match op with 
      | Plus -> helper_plus (eval e1 st, eval e2 st)
      | Minus -> helper_minus (eval e1 st, eval e2 st)
      | Multiply -> helper_multiply (eval e1 st, eval e2 st)
      | Divide -> helper_divide (eval e1 st, eval e2 st)
    end
  | Unary (op, e1), st-> begin 
      match (op,eval e1 st) with 
      | (Plus, Int (x)) -> Int (x)
      | (Minus, Int (x)) -> Int (-x)
      | _ -> failwith "wrong types"
    end
  | Variable (x),st -> begin 
      match State.find x st with 
      | Some t -> t
      | None -> failwith "undefined types"
    end

  | Value (x) ,st -> x

let evaluate input st = match input with
  | Some s, expr -> insert s (eval expr st) st
  | None, expr -> (match (eval expr st) with 
    | Int i -> print_endline (string_of_int i); st
    | String s -> print_endline s; st)
