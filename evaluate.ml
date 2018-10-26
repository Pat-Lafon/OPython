open Parser
open State
open Error

let helper_plus = function 
  | Int x, Int y -> Int(x+y)
  | String x, String y -> String (x ^ y)
  | Float x, Float y -> Float (x +. y)
  | Int x, Float y -> Float (float_of_int x +. y)
  | Float x, Int y -> Float (float_of_int y +. x)
  | Bool x, Bool y -> Int(if x then 1 else 0 + if y then 1 else 0)
  | Bool x, Int y -> if x then Int (y+1) else Int  y
  | Bool x, Float y -> if x then Float (y +. float_of_int 1) else Float y
  | Int x, Bool y -> if y then Int (x+1) else Int x
  | Float x, Bool y -> if y then Float (x +. float_of_int 1) else Float x
  | String x, _ -> raise (TypeError ("unsupported operand"))
  | _, String y -> raise (TypeError ("unsupported operand"))
  | _, VList x -> raise (TypeError ("unsupported operand"))
  | VList x, _-> raise (TypeError ("unsupported operand"))

let helper_minus = function 
  | Int x, Int y -> Int(x-y)
  | Float x, Float y -> Float (x -. y)
  | Int x, Float y -> Float (float_of_int x -. y)
  | Float x, Int y -> Float (float_of_int y -. x)
  | Bool x, Bool y -> Int(if x then 1 else 0 - if y then 1 else 0)
  | Bool x, Int y -> if x then Int (1-y) else Int (-y)
  | Bool x, Float y -> if x then Float (float_of_int 1 -. y) else Float (-.y)
  | Int x, Bool y -> if y then Int (x-1) else Int x
  | Float x, Bool y -> if y then Float (x -. float_of_int 1) else Float x
  | String x, _ -> raise (TypeError ("unsupported operand"))
  | _, String y -> raise (TypeError ("unsupported operand"))
  | _, VList x -> raise (TypeError ("unsupported operand"))
  | VList x, _-> raise (TypeError ("unsupported operand"))

let helper_multiply = function 
  | Int x, Int y -> Int (x * y)
  | Float x, Float y -> Float (x *. y)
  | Int x, Float y -> Float (float_of_int x *. y)
  | Float x, Int y -> Float (float_of_int y *. x)
  | Bool x, Int y -> if x then Int y else Int 0
  | Bool x, Float y -> if x then Float (y *. float_of_int 1) else Float 0.0
  | Int x, Bool y -> if y then Int x else Int 0
  | Float x, Bool y -> if y then Float (x *. float_of_int 1) else Float 0.0
  | String x, String y -> raise (TypeError "can't multiply sequence by non-int of type 'str'")
  | String x, Float y -> raise (TypeError "can't multiply sequence by non-int of type 'float'")
  | Float x, String y -> raise (TypeError "can't multiply sequence by non-int of type 'float'")
  | String x, Bool  y -> if y then String x else String ""
  | Bool x, String  y -> if x then String y else String ""
  | Bool x, Bool y -> if x && y then Int 1 else Int 0
  | Int x, String y -> raise (TypeError ("unsupported operand"))
  | String x, Int y -> raise (TypeError ("unsupported operand"))
  | _, VList x -> raise (TypeError ("unsupported operand"))
  | VList x, _-> raise (TypeError ("unsupported operand"))

let helper_mod = function 
  | Int x, Int y -> if x != 0 then Int (x mod y) 
    else raise (ZeroDivisionError ("modulo by zero"))
  | Float x, Int y -> if y > 0 
    then Float (x -. float_of_int y *. floor (x/. (float_of_int y))) 
    else if y < 0 then Float (float_of_int y +. x -. float_of_int y *. floor (x/. (float_of_int y)))
    else raise (ZeroDivisionError ("modulo by zero"))
  | Int x, Float y -> if y > 0.0 
    then Float (float_of_int x -. y *. floor (float_of_int x/. y)) 
    else if y < 0.0 then Float (y +. float_of_int x -. y *. floor (float_of_int x/. y))
    else raise (ZeroDivisionError ("modulo by zero"))
  | Float x, Float y -> if y > 0.0 
    then Float (x -. y *. floor (x/. y)) 
    else if y < 0.0 then Float (y +. x -. y *. floor (x/. y))
    else raise (ZeroDivisionError ("modulo by zero"))
  | Float x, Bool y ->  if y then Float (x -. floor x)
    else raise (ZeroDivisionError ("modulo by zero"))
  | Int x, Bool y ->  if y then Int 0
    else raise (ZeroDivisionError ("modulo by zero"))
  | Bool x, Bool y -> if y then Int 0 
    else raise (ZeroDivisionError ("modulo by zero"))
  | Bool x, Int y -> if x 
    then Int (1 mod y) 
    else Int (0)
  | Bool x, Float y -> if y > 0.0 
    then if x then Float (float_of_int 1 -. y *. floor (float_of_int 1/. y)) 
      else Float (float_of_int 1 -. y *. floor (float_of_int 1/. y))
    else if y < 0.0 then if x then Float (y +. float_of_int 1 -. y *. floor (float_of_int 1/. y))
      else Float (y +. float_of_int 0 -. y *. floor (float_of_int 0/. y))
    else raise (ZeroDivisionError ("modulo by zero"))
  | _, VList x -> raise (TypeError ("unsupported operand"))
  | VList x, _-> raise (TypeError ("unsupported operand"))
  | String x, _-> raise (TypeError ("unsupported operand"))
  | _, String x -> raise (TypeError ("unsupported operand"))

let helper_floor = function
  | Int x, Int y -> if y = 0 then raise (ZeroDivisionError "integer division or modulo by zero") 
    else Int(int_of_float(floor(float_of_int x/.float_of_int y)))
  | Int x, Float y -> if y = 0. then raise (ZeroDivisionError "integer division or modulo by zero") 
    else Float(floor(float_of_int x/.y))
  | Int x, Bool y -> if not y then raise (ZeroDivisionError "integer division or modulo by zero") 
    else Int x
  | Float x, Float y -> if y = 0. then raise (ZeroDivisionError "integer division or modulo by zero") 
    else Float(floor(x/.y))
  | Float x, Int y -> if y = 0 then raise (ZeroDivisionError "integer division or modulo by zero") 
    else Float(floor(x/.(float_of_int y)))
  | Float x, Bool y -> if not y then raise (ZeroDivisionError "integer division or modulo by zero") 
    else Float x
  | Bool x, Bool y -> if not y then raise (ZeroDivisionError "integer division or modulo by zero") 
    else if x then Int 1 else Int 0
  | Bool x, Int y -> if y = 0 then raise (ZeroDivisionError "integer division or modulo by zero") 
    else if x then Int(int_of_float(floor(1.0/.float_of_int y))) else Int 0
  | Bool x, Float y -> if y = 0. then raise (ZeroDivisionError "integer division or modulo by zero") 
    else if x then Float(floor(1.0/.y)) else Float 0.
  | _, VList x -> raise (TypeError ("unsupported operand"))
  | VList x, _-> raise (TypeError ("unsupported operand"))
  | String x, _-> raise (TypeError ("unsupported operand"))
  | _, String x -> raise (TypeError ("unsupported operand"))

let helper_exp = function 
  | Int x, Int y -> Int (int_of_float (float_of_int x ** float_of_int y))
  | Int x, Float y -> Float ((float_of_int x) ** y)
  | Int x, Bool y -> if y then Int x else Int 1
  | Float x, Int y -> Float (x ** (float_of_int y))
  | Float x, Float y -> Float (x ** y)
  | Float x, Bool y -> if y then Float x else Float 1.
  | Bool x, Bool y -> if not x && y then Int 0 else Int 1
  | Bool x, Int y -> if x then Int(int_of_float(1.0 ** float_of_int y)) else Int 0
  | Bool x, Float y -> if x then Float(1.0 ** y) else Float 0.
  | _, VList x -> raise (TypeError ("unsupported operand"))
  | VList x, _-> raise (TypeError ("unsupported operand"))
  | String x, _-> raise (TypeError ("unsupported operand"))
  | _, String x -> raise (TypeError ("unsupported operand"))

let helper_and = function
  | Bool x, Bool y -> Bool (x && y)
  | Bool x, Int  y -> if x = false then Bool(x) else if y = 0 then Int(0) else Int y
  | Bool x, Float  y -> if x = false then Bool(x) else if y = 0. then Int(0) else Float  y
  | Int x, Int  y -> if (x=0)||(y=0) then Int(0) else Int  y
  | Int x, Bool  y -> if x = 0 then Int(0) else Bool y
  | Int x, Float y -> if x = 0 then Int(0) else Float(y)
  | Float x, Float  y -> if (x=0.)||(y=0.) then Float(0.) else Float  y
  | Float x, Bool  y -> if x = 0. then Float(0.) else Bool  y
  | Float x, Int y -> if x = 0. then Float(0.) else Int(y)
  | _, VList x -> raise (TypeError ("unsupported operand"))
  | VList x, _-> raise (TypeError ("unsupported operand"))
  | String x, _-> raise (TypeError ("unsupported operand"))
  | _, String x -> raise (TypeError ("unsupported operand"))

let helper_or = function 
  | Bool x, Bool  y -> Bool (x || y)
  | Bool x, Int  y -> if x = true then Bool(x) else Int(y)
  | Bool x, Float  y -> if x = true then Bool(x) else Float(y)
  | Int x, Int  y -> if x=0 then Int(y) else if y=0 then Int(x) else Int(x)
  | Int x, Bool  y -> if x!=0 then Int(x) else Bool(y)
  | Int x, Float y -> if x=0 then Float(y) else Int(x)
  | Float x, Float  y -> if x=0. then Float(y) else if y=0. then Float(x) else Float(x)
  | Float x, Bool  y -> if x!=0. then Float(x) else Bool(y)
  | Float x, Int y -> if x=0. then Int(y) else Float(x)
  | _, VList x -> raise (TypeError ("unsupported operand"))
  | VList x, _-> raise (TypeError ("unsupported operand"))
  | String x, _-> raise (TypeError ("unsupported operand"))
  | _, String x -> raise (TypeError ("unsupported operand"))

(* both of these have tons of crosstypes, *)
let helper_equals = function
  | Bool x, Bool  y -> Bool (x = y)
  | _ -> failwith"not done"

let helper_not_equals = function
  | Bool x, Bool  y -> Bool (x != y)
  | _ -> failwith"not done"


(* AS OF PYTHON3, DIVISION RETURNS A FLOAT WHEN IT SHOULD BE A FLOAT, INT OTHERWISE *)
let helper_divide = function 
  | Int x, Int y -> if y = 0 then raise (ZeroDivisionError "division by zero") 
    else Float(float_of_int(x/y))
  | Int x, Float y -> if y = 0. then raise (ZeroDivisionError "float division by zero") 
    else Float ((float_of_int x) /. y)
  | Int x, Bool y -> if y = false then raise (ZeroDivisionError "division by zero") 
    else Float(float_of_int x)
  | Float x, Float y -> if y = 0. then raise (ZeroDivisionError "float division by zero")
    else Float (x /. y)
  | Float x, Int y -> if y = 0 then raise (ZeroDivisionError "float division by zero") 
    else Float (x /. (float_of_int y))
  | Float x, Bool y -> if y = false then raise (ZeroDivisionError "float division by zero") 
    else Float x
  | Bool x, Bool y -> if y = false then raise (ZeroDivisionError "division by zero") 
    else if x then Float(1.0) else Float 0.
  | Bool x, Int y -> if y = 0 then raise (ZeroDivisionError "division by zero") 
    else if x then Float(1.0/.(float_of_int y)) else Float(0.0)
  | Bool x, Float y-> if y = 0. then raise (ZeroDivisionError "float division by zero") 
    else if x then Float(1.0/.y) else Float 0.
  | _, VList x -> raise (TypeError ("unsupported operand"))
  | VList x, _-> raise (TypeError ("unsupported operand"))
  | String x, _-> raise (TypeError ("unsupported operand"))
  | _, String x -> raise (TypeError ("unsupported operand"))

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
     | And -> helper_and (eval e1 st, eval e2 st)
     | Or -> helper_or (eval e1 st, eval e2 st)
     | Exponent -> helper_exp (eval e1 st, eval e2 st)
     | Equal -> helper_equals (eval e1 st, eval e2 st)
     | Not_Equal -> helper_not_equals (eval e1 st, eval e2 st)
     | Modular -> helper_mod (eval e1 st, eval e2 st)
     | Not -> raise (SyntaxError "invalid syntax")
     | Complement -> raise (SyntaxError "Invalid syntax"))
  | Unary (op, e1) ->
    (match op, eval e1 st with 
     | Plus, Int x -> Int x
     | Minus, Int x -> Int (-x)
     | Plus, Float x -> Float x
     | Minus, Float x -> Float (-.x)
     (* If able, say what type was input *)
     | Plus, _ -> raise (TypeError "bad operand type for unary +")
     | Minus, _ -> raise (TypeError "bad operand type for unary -")
     | Not, Bool x -> Bool (not x)
     | Complement, Int x -> Int (-x-1) 
     | Complement, Bool x -> if x then Int (-2) else Int (-1)
     | Not, Int x -> if x = 0 then Bool true else Bool false
     | Not, Float x -> if x = 0. then Bool true else Bool false
     | _ -> raise (SyntaxError "invalid syntax"))
  | Variable x -> 
    (match State.find x st with 
     | Some t -> t
     | None -> raise (NameError ("name '"^x^"' is not defined")))
  | Value x -> x
  | List x -> let rec help = function 
      | [] -> []
      | h::t -> eval h st :: help t
    in VList(help x)

let rec to_string (value:State.value) : string = (match value with
    | VList x -> List.fold_left (fun x y -> x^(to_string y)^", ") "[" x |> 
                 (fun x -> if String.length x = 1 then x ^ "]" 
                   else String.sub x 0 (String.length x -2) ^ "]")
    | Int x -> string_of_int x
    | Float x -> string_of_float x
    | Bool x -> string_of_bool x |> String.capitalize_ascii
    | String x -> "'" ^ x ^ "'")

let print (value:State.value):unit = 
  (match value with
   | Int x -> string_of_int x
   | Float x -> string_of_float x
   | Bool x -> string_of_bool x |> String.capitalize_ascii
   | String x -> "'" ^ x ^ "'"
   | VList x -> "[]") |> print_endline

let print (value:State.value):unit = 
  value |> to_string |> print_endline

let evaluate input st = match input with
  | Some s, expr -> insert s (eval expr st) st
  | None, expr -> print (eval expr st); st
