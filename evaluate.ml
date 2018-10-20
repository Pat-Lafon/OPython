open Parser
open State
open Error

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

(* AS OF PYTHON3, DIVISION RETURNS A FLOAT WHEN IT SHOULD BE A FLOAT, INT OTHERWISE *)
let helper_divide = function 
  | (Int (x), Int(y)) -> Int(x/y)
  | _ -> failwith "wrong types"

let rec eval (exp : expr) (st : State.t) : value = match exp with 
  | Binary (e1, op, e2) -> 
    (match op with 
     | Plus -> helper_plus (eval e1 st, eval e2 st)
     | Minus -> helper_minus (eval e1 st, eval e2 st)
     | Multiply -> helper_multiply (eval e1 st, eval e2 st)
     | Divide -> helper_divide (eval e1 st, eval e2 st)
     | _ -> failwith "unimplemented")
  | Unary (op, e1) ->
    (match (op, eval e1 st) with 
     | (Plus, Int (x)) -> Int (x)
     | (Minus, Int (x)) -> Int (-x)
     (* If able, say what type was input *)
     | (Plus, _) -> raise (TypeError "bad operand type for unary +")
     | (Minus, _) -> raise (TypeError "bad operand type for unary -")
     | _ -> raise (SyntaxError "invalid syntax"))
  | Variable x -> 
    (match State.find x st with 
     | Some t -> t
     | None -> raise (NameError ("name '"^x^"' is not defined")))
  | Value x -> x

let print (value:State.value):unit = 
  (match value with
   | Int x -> string_of_int x
   | Float x -> string_of_float x
   | Bool x -> string_of_bool x
   | String x -> x) |> print_endline

let evaluate input st = match input with
  | Some s, expr -> insert s (eval expr st) st
  | None, expr -> print (eval expr st); st
