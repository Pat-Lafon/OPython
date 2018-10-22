open Parser
open State
open Error

let helper_plus = function 
  | (Int (x), Int(y)) -> Int(x+y)
  | (String (x), String (y)) -> String (x ^ y)
  | (Float (x), Float (y)) -> Float (x +. y)
  | (Int (x), Float (y)) -> Float (float_of_int x +. y)
  | (Float (x), Int (y)) -> Float (float_of_int y +. x)
  | (Bool (x), Int (y)) -> if x = true then Int (y+1) else Int (y)
  | (Bool (x), Float (y)) -> if x = true then Float (y +. float_of_int 1) else Float (y)
  | (Int (x), Bool (y)) -> if y = true then Int (x+1) else Int (x)
  | (Float (x), Bool (y)) -> if y = true then Float (x +. float_of_int 1) else Float (x)
  | _ -> raise (TypeError ("unsupported operand"))

let helper_minus = function 
  | (Int (x), Int(y)) -> Int(x-y)
  | (Float (x), Float (y)) -> Float (x -. y)
  | (Int (x), Float (y)) -> Float (float_of_int x -. y)
  | (Float (x), Int (y)) -> Float (float_of_int y -. x)
  | (Bool (x), Int (y)) -> if x = true then Int (1-y) else Int (-y)
  | (Bool (x), Float (y)) -> if x = true then Float (float_of_int 1 -. y) else Float (-.y)
  | (Int (x), Bool (y)) -> if y = true then Int (x-1) else Int (x)
  | (Float (x), Bool (y)) -> if y = true then Float (x -. float_of_int 1) else Float (x)
  | _ -> raise (TypeError ("unsupported operand"))

let helper_multiply = function 
  | (Int (x), Int(y)) -> Int (x * y)
  | (Float (x), Float (y)) -> Float (x *. y)
  | (Int (x), Float (y)) -> Float (float_of_int x *. y)
  | (Float (x), Int (y)) -> Float (float_of_int y *. x)
  | (Bool (x), Int (y)) -> if x = true then Int (y) else Int (0)
  | (Bool (x), Float (y)) -> if x = true then Float (y *. float_of_int 1) else Float (0.0)
  | (Int (x), Bool (y)) -> if y = true then Int (x) else Int (0)
  | (Float (x), Bool (y)) -> if y = true then Float (x *. float_of_int 1) else Float (0.0)
  | _ -> raise (TypeError ("unsupported operand"))

let helper_mod = function 
  | (Int (x), Int(y)) -> if x != 0 then Int (x mod y) 
    else raise (ZeroDivisionError ("modulo by zero"))
  | (Float (x), Int(y)) -> if y > 0 
    then Float (x -. float_of_int y *. floor (x/. (float_of_int y))) 
    else if y < 0 then Float (float_of_int y +. x -. float_of_int y *. floor (x/. (float_of_int y)))
    else raise (ZeroDivisionError ("modulo by zero"))
  | (Int (x), Float(y)) -> if y > 0.0 
    then Float (float_of_int x -. y *. floor (float_of_int x/. y)) 
    else if y < 0.0 then Float (y +. float_of_int x -. y *. floor (float_of_int x/. y))
    else raise (ZeroDivisionError ("modulo by zero"))
  | (Float (x), Float(y)) -> if y > 0.0 
    then Float (x -. y *. floor (x/. y)) 
    else if y < 0.0 then Float (y +. x -. y *. floor (x/. y))
    else raise (ZeroDivisionError ("modulo by zero"))
  | (Float (x), Bool (y)) ->  if y = true then Float (x -. floor x)
    else raise (ZeroDivisionError ("modulo by zero"))
  | (Int (x), Bool (y)) ->  if y = true then Int (0)
    else raise (ZeroDivisionError ("modulo by zero"))
  | (Bool (x), Int(y)) -> if x = true 
    then Int (1 mod y) 
    else Int (0)
  | (Bool (x), Float (y)) -> if y > 0.0 
    then if x = true then Float (float_of_int 1 -. y *. floor (float_of_int 1/. y)) 
      else Float (float_of_int 1 -. y *. floor (float_of_int 1/. y))
    else if y < 0.0 then if x = true then Float (y +. float_of_int 1 -. y *. floor (float_of_int 1/. y))
      else Float (y +. float_of_int 0 -. y *. floor (float_of_int 0/. y))
    else raise (ZeroDivisionError ("modulo by zero"))
  | _ -> failwith "wrong types"

let helper_floor = function
  | (Int(x), Int(y)) -> if (y = 0) then raise (ZeroDivisionError "integer division or modulo by zero") else Int(int_of_float(floor(float_of_int(x)/.float_of_int(y))))
  | (Int(x), Float(y)) -> if (y = 0.) then raise (ZeroDivisionError "integer division or modulo by zero") else Float(floor(float_of_int(x)/.y))
  | (Int(x), Bool(y)) -> if (y = false) then raise (ZeroDivisionError "integer division or modulo by zero") else Int(x)
  | (Float(x), Float(y)) -> if (y = 0.) then raise (ZeroDivisionError "integer division or modulo by zero") else Float(floor(x/.y))
  | (Float(x), Int(y)) -> if (y = 0) then raise (ZeroDivisionError "integer division or modulo by zero") else Float(floor(x/.(float_of_int (y))))
  | (Float(x), Bool(y)) -> if (y = false) then raise (ZeroDivisionError "integer division or modulo by zero") else Float(x)
  | (Bool(x), Bool(y)) -> if (y = false) then raise (ZeroDivisionError "integer division or modulo by zero") else if (x = true) then Int(1) else Int(0)
  | (Bool(x), Int(y)) -> if (y = 0) then raise (ZeroDivisionError "integer division or modulo by zero") else if (x = true) then Int(int_of_float(floor(1.0/.float_of_int(y)))) else Int(0)
  | (Bool(x), Float(y)) -> if (y = 0.) then raise (ZeroDivisionError "integer division or modulo by zero") else if (x = true) then Float(floor(1.0/.y)) else Float(0.)
  | _ -> failwith "wrong types"

let helper_exp = function 
  | (Int (x), Int(y)) -> Int (int_of_float (float_of_int x ** float_of_int y))
  | (Int (x), Float(y)) -> Float ((float_of_int x) ** y)
  | (Int (x), Bool(y)) -> if (y = true) then Int(x) else Int(1)
  | (Float (x), Int(y)) -> Float (x ** (float_of_int y))
  | (Float (x), Float (y)) -> Float (x ** y)
  | (Float (x), Bool(y)) -> if (y = true) then Float(x) else Float(1.)
  | (Bool (x), Bool(y)) -> if (x = false) && (y = true) then Int(0) else Int(1)
  | (Bool (x), Int(y)) -> if (x = true) then Int(int_of_float(1.0 ** float_of_int(y))) else Int(0)
  | (Bool (x), Float(y)) -> if (x = true) then Float(1.0 ** y) else Float(0.)
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
  | (Int (x), Int (y)) -> if y = 0 then raise (ZeroDivisionError "division by zero") else Float(float_of_int(x/y))
  | (Int (x), Float (y)) -> if y = 0. then raise (ZeroDivisionError "float division by zero") else Float ((float_of_int x) /. y)
  | (Int (x), Bool (y)) -> if y = false then raise (ZeroDivisionError "float division by zero") else Float(float_of_int(x))
  | (Float (x), Float (y)) -> if y = 0. then raise (ZeroDivisionError "float division by zero") else Float (x /. y)
  | (Float (x), Int (y)) -> if y = 0 then raise (ZeroDivisionError "float division by zero") else Float (x /. (float_of_int y))
  | (Float (x), Bool (y)) -> if y = false then raise (ZeroDivisionError "float division by zero") else Float(x)
  | (Bool (x), Bool (y)) -> if y = false then raise (ZeroDivisionError "float division by zero") else if x = true then Float(1.0) else Float(0.)
  | (Bool (x), Int (y)) -> if y = 0 then raise (ZeroDivisionError "float division by zero") else if x = true then Float(1.0/.(float_of_int(y))) else Float(0.0)
  | (Bool (x), Float (y)) -> if y = 0. then raise (ZeroDivisionError "float division by zero") else if x = true then Float(1.0/.y) else Float(0.)
  | _ -> failwith "wrong types"

let if_decider = function
  | Int(0) -> false
  | String("") -> false
  | Bool(false) -> false
  | Float(0.0)  -> false
  | VList([]) -> false
  | _ -> true

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
     | (Complement, Int (x)) -> Int (-x-1) 
     | (Complement, Bool (x)) -> if x = true then Int (-2) else Int (-1)
     | (Not, Int (x)) -> if x = 0 then Bool(true) else Bool (false)
     | (Not, Float (x)) -> if x = 0. then Bool(true) else Bool (false)
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
   | Bool x -> string_of_bool x |> String.capitalize_ascii
   | String x -> "'" ^ x ^ "'"
   | VList x -> "[]") |> print_endline


let evaluate input st = match input with
  | Some s, expr -> insert s (eval expr st) st
  | None, expr -> print (eval expr st); st
