open Parser
open State
open Error

let helper_plus = function 
  | (Int (x), Int(y)) -> Int(x+y)
  | (String (x), String (y)) -> String (x ^ y)
  | (Float (x), Float (y)) -> Float (x +. y)
  | _ -> failwith "wrong types"

let helper_minus = function 
  | (Int (x), Int(y)) -> Int(x-y)
  | (Float (x), Float (y)) -> Float (x -. y)
  | _ -> failwith "wrong types"

let helper_multiply = function 
  | (Int (x), Int(y)) -> Int (x * y)
  | (Float (x), Float (y)) -> Float (x *. y)
  | _ -> failwith "wrong types"

let helper_mod = function 
  | (Int (x), Int(y)) -> Int (x mod y)
  | _ -> failwith "wrong types"

let helper_floor = function
  | (Int(x), Int(y)) -> floor(float_of_int(x)/.float_of_int(y))
  | (Float(x), Float(y)) -> floor(x/.y)
  | (Int(x), Float(y)) -> floor(float_of_int(x)/.y)
  | (Float(x), Int(y)) -> floor(x/.(float_of_int (y)))
  | _ -> failwith "wrong types"

let helper_exp = function 
  | (Int (x), Int(y)) -> Int (int_of_float (float_of_int x ** float_of_int y))
  | (Float (x), Float (y)) -> Float (x ** y)
  | (Int (x), Float(y)) -> Float ((float_of_int x) ** y)
  | (Float (x), Int(y)) -> Float (x ** (float_of_int y))
  | _ -> failwith "wrong types"

let helper_bool = function 
  | (Bool (x), Bool (y), "and") -> Bool (x && y)
  | (Bool (x), Bool (y), "or") -> Bool (x || y)
  | (Bool (x), Bool (y), "equals") -> Bool (x = y)
  | (Bool (x), Bool (y), "not equals") -> Bool (x != y)
  | (Bool (x), Int (y), "and") -> Int (y)
  | (Bool (x), Int (y), "or") -> Bool (x)
  | (Bool (x), Float (y), "and") -> Float (y)
  | (Bool (x), Float (y), "or") -> Bool (x)
  | (Int (x), Int (y), "and") -> Int (y)
  | (Int (x), Int (y), "or") -> Int (x)
  | (Int (x), Bool (y), "and") -> Bool (y)
  | (Int (x), Bool (y), "or") -> Int (x)
  | (Float (x), Float (y), "and") -> Float (y)
  | (Float (x), Float (y), "or") -> Float (x)
  | (Float (x), Bool (y), "and") -> Bool (y)
  | (Float (x), Bool (y), "or") -> Float (x)
  | _ -> failwith "wrong types"


(* AS OF PYTHON3, DIVISION RETURNS A FLOAT WHEN IT SHOULD BE A FLOAT, INT OTHERWISE *)
let helper_divide = function 
  | (Int (x), Int(y)) -> 
    if y = 0 then raise (ZeroDivisionError "division by zero") else Int(x/y)
  | (Float (x), Float (y)) -> 
    if y = 0. then raise (ZeroDivisionError "float division by zero") else Float (x /. y)
  | (Float (x), Int(y)) ->
    if y = 0 then raise (ZeroDivisionError "float division by zero") else Float (x /. (float_of_int y))
  | Int (x), Float(y) ->
    if y = 0. then raise (ZeroDivisionError "float division by zero") else Float ((float_of_int x) /. y)
  | _ -> failwith "wrong types"

let rec eval (exp : expr) (st : State.t) : value = match exp with 
  | Binary (e1, op, e2) -> 
    (match op with 
     | Plus -> helper_plus (eval e1 st, eval e2 st)
     | Minus -> helper_minus (eval e1 st, eval e2 st)
     | Multiply -> helper_multiply (eval e1 st, eval e2 st)
     | Divide -> helper_divide (eval e1 st, eval e2 st)
     | Floor_Divide -> helper_floor (eval e1 st, eval e2 st)
     | And -> helper_bool (eval e1 st, eval e2 st, "and")
     | Or -> helper_bool (eval e1 st, eval e2 st, "or")
     | Exponent -> helper_exp (eval e1 st, eval e2 st)
     | Equal -> helper_bool (eval e1 st, eval e2 st, "equals")
     | Not_Equal -> helper_bool (eval e1 st, eval e2 st, "not equals")
     | Modular -> helper_mod (eval e1 st, eval e2 st)
     | _ -> failwith "unimplemented")
  | Unary (op, e1) ->
    (match (op, eval e1 st) with 
     | (Plus, Int (x)) -> Int (x)
     | (Minus, Int (x)) -> Int (-x)
     | (Plus, Float (x)) -> Float (x)
     | (Minus, Float (x)) -> Float (-.x)
     (* If able, say what type was input *)
     | (Plus, _) -> raise (TypeError "bad operand type for unary +")
     | (Minus, _) -> raise (TypeError "bad operand type for unary -")
     | (Not, Bool (x)) -> Bool (not x)
     | (Not, Int (x)) -> Bool (false)
     | (Not, Float (x)) -> Bool (false)
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
   | String x -> "'" ^ x ^ "'") |> print_endline

let evaluate input st = match input with
  | Some s, expr -> insert s (eval expr st) st
  | None, expr -> print (eval expr st); st
