open Parser
open State

let rec mul x y acc op = if y = 0 then acc else mul x (y-1) (op acc x) op

let helper_plus = function 
  | Int x, Int y -> Int(x+y)
  | Int x, Float y -> Float (float_of_int x +. y)
  | Int x, Bool y -> if y then Int (x+1) else Int x
  | Int x, String y -> raise (TypeError "unsupported operand type for +")
  | Int x, VList y -> raise (TypeError "unsupported operand type for +")
  | Float x, Int y -> Float (float_of_int y +. x)
  | Float x, Float y -> Float (x +. y)
  | Float x, Bool y -> if y then Float (x +. float_of_int 1) else Float x
  | Float x , String y -> raise (TypeError "unsupported operand type for +")
  | Float x, VList y -> raise (TypeError "unsupported operand type for +")
  | Bool x, Int y -> if x then Int (y+1) else Int  y
  | Bool x, Float y -> if x then Float (y +. float_of_int 1) else Float y
  | Bool x, Bool y -> Int(if x then 1 else 0 + if y then 1 else 0)
  | Bool x , String y -> raise (TypeError "unsupported operand type for +")
  | Bool x, VList y -> raise (TypeError "unsupported operand type for +")
  | String x, String y -> String (x ^ y)
  | String x, _ -> raise (TypeError "can only concatenate str to str")
  | VList x, VList y -> VList (x @ y)
  | VList x, _-> raise (TypeError "can only concatenate list to list")

let helper_multiply = function 
  | Int x, Int y -> Int (x * y)
  | Int x, Float y -> Float (float_of_int x *. y)
  | Int x, Bool y -> if y then Int x else Int 0
  | Int x, String y -> raise (TypeError ("unsupported operand type for *"))
  | Int x, VList y -> VList (mul y x [] (@))
  | Float x, Int y -> Float (float_of_int y *. x)
  | Float x, Float y -> Float (x *. y)
  | Float x, Bool y -> if y then Float (x *. float_of_int 1) else Float 0.0
  | Float x, String y -> raise (TypeError "can't multiply sequence by non-int of type 'float'")
  | Float x, VList y -> raise (TypeError "can't multiply sequence by non-int")
  | Bool x, Int y -> if x then Int y else Int 0
  | Bool x, Float y -> if x then Float (y *. float_of_int 1) else Float 0.0
  | Bool x, Bool y -> if x && y then Int 1 else Int 0
  | Bool x, String y -> if x then String y else String ""
  | Bool x, VList y -> if x then VList y else VList []
  | String x, Int y ->  String (mul x y "" (^))
  | String x, Float y -> raise (TypeError "can't multiply sequence by non-int of type 'float'")
  | String x, Bool  y -> if y then String x else String ""
  | String x, String y -> raise (TypeError "can't multiply sequence by non-int of type 'str'")
  | String x, VList y -> raise (TypeError "can't multiply sequence by non-int")
  | VList x, Int y -> VList (mul x y [] (@))
  | VList x, Float y -> raise (TypeError "can't multiply sequence by non-int")
  | VList x, Bool y -> if y then VList x else VList []
  | VList x, String y -> raise (TypeError "can't multiply sequence by non-int")
  | VList x, VList y -> raise (TypeError "can't multiply sequence by non-int")

let helper_divide = function 
  | _, Int 0 -> raise (ZeroDivisionError "division by zero")
  | _, Float 0. -> raise (ZeroDivisionError "float division by zero") 
  | _, Bool false -> raise (ZeroDivisionError "float division by zero") 
  | Int x, Int y -> if x mod 2 = 0 then Int(x/y) else Float(float_of_int x /. float_of_int y)
  | Int x, Float y -> Float(float_of_int x /. y)
  | Int x, Bool y -> Float(float_of_int x)
  | Float x, Int y -> Float (x /. float_of_int y)
  | Float x, Float y ->  Float (x /. y)
  | Float x, Bool y -> Float x
  | Bool x, Int y -> if x then Float(1.0/.(float_of_int y)) else Float(0.0)
  | Bool x, Float y-> if x then Float(1.0/.y) else Float 0.
  | Bool x, Bool y -> if x then Float 1.0 else Float 0.
  | String x, _ | _, String x -> raise (TypeError "unsupported operand type for /")
  | VList x, _ | _, VList x -> raise (TypeError "unsupported operand type for /")

let helper_floor exp = match helper_divide exp with
  | Int x -> Int x
  | Float x -> Int (int_of_float(floor x))
  | _ -> failwith "Not possible?"

let helper_mod = function 
  | _, Int 0 -> raise (ZeroDivisionError "modulo by zero")
  | _, Float 0. -> raise (ZeroDivisionError "modulo by zero")
  | _, Bool false -> raise (ZeroDivisionError "modulo by zero")
  | Int x, Int y -> Int (x mod y) 
  | Int x, Float y -> Float (mod_float (float_of_int x) y)
  | Int x, Bool y ->  Int 0
  | Float x, Int y -> Float (mod_float x (float_of_int y))
  | Float x, Float y -> Float (mod_float x y)
  | Float x, Bool y -> Float (mod_float x 1.)
  | Bool x, Int y -> if x then Int (1 mod y) else Int 0
  | Bool x, Float y -> if x then Float (mod_float 1. y) else Float 0.
  | Bool x, Bool y -> Float 0.
  | String x, _ | _, String x -> raise (TypeError "unsupported operand type for %")
  | VList x, _ | _, VList x -> raise (TypeError "unsupported operand type for %")

let helper_exp = function 
  | Int x, Int y -> Int (int_of_float (float_of_int x ** float_of_int y))
  | Int x, Float y -> Float ((float_of_int x) ** y)
  | Int x, Bool y -> if y then Int x else Int 1
  | Float x, Int y -> Float (x ** (float_of_int y))
  | Float x, Float y -> Float (x ** y)
  | Float x, Bool y -> if y then Float x else Float 1.
  | Bool x, Int y -> if x then Int(int_of_float(1.0 ** float_of_int y)) else Int 0
  | Bool x, Float y -> if x then Float(1.0 ** y) else Float 0.
  | Bool x, Bool y -> if not x && y then Int 0 else Int 1
  | VList x, _ | _, VList x -> raise (TypeError "unsupported operand type for **")
  | String x, _ | _, String x -> raise (TypeError "unsupported operand type for **")

let helper_and = function
  | Int x, y -> if x = 0 then Int 0 else y
  | Float x, y-> if x = 0. then Float 0. else y
  | Bool x, y -> if not x then Bool(x) else y
  | String x, y -> if x = "" then String "" else y
  | VList x, y -> if x = [] then VList x else y

let helper_or = function 
  | Int x, y -> if x<>0 then Int x else y
  | Float x, y -> if x <> 0. then Float x else y
  | Bool x, y -> if x then Bool x else y
  | String x, y -> if x <> "" then String x else y
  | VList x, y -> if x <> [] then VList x else y

let helper_equal = function
  | Int x, Int y -> Bool (x = y)
  | Int x, Float y -> Bool (float_of_int x = y)
  | Int x, Bool y -> if y then Bool(x=1) else Bool (x=0)
  | Float x, Int y -> Bool (x = float_of_int y)
  | Float x, Float y -> Bool (x = y)
  | Float x, Bool y -> if y then Bool(x=1.0) else Bool (x=0.0)
  | Bool x, Int y -> if x then Bool(y=1) else Bool (y=0)
  | Bool x, Float y -> if x then Bool(y=1.0) else Bool (y=0.0)
  | Bool x, Bool y -> Bool (x = y)
  | String x, _ | _, String x -> Bool false
  | VList x, _ | _, VList x -> Bool false

let helper_greater_than = function
  | Int x, Int y -> Bool (x > y)
  | Int x, Float y -> Bool (float_of_int x > y)
  | Int x, Bool y -> if y then Bool(x>1) else Bool (x>0)
  | Float x, Int y -> Bool (x > float_of_int y)
  | Float x, Float y -> Bool (x > y)
  | Float x, Bool y -> if y then Bool(x>1.0) else Bool (x>0.0)
  | Bool x, Int y -> if x then Bool(y<1) else Bool (y<0)
  | Bool x, Float y -> if x then Bool(y<1.0) else Bool (y<0.0)
  | Bool x, Bool y -> if x then Bool(not y) else Bool (false)
  | String x, _ | _, String x -> Bool false
  | VList x, _ | _, VList x -> Bool false

let helper_greater_equal = function
  | Int x, Int y -> Bool (x >= y)
  | Int x, Float y -> Bool (float_of_int x >= y)
  | Int x, Bool y -> if y then Bool(x>=1) else Bool (x>=0)
  | Float x, Int y -> Bool (x >= float_of_int y)
  | Float x, Float y -> Bool (x >= y)
  | Float x, Bool y -> if y then Bool(x>=1.0) else Bool (x>=0.0)
  | Bool x, Int y -> if x then Bool(y<=1) else Bool (y<=0)
  | Bool x, Float y -> if x then Bool(y<=1.0) else Bool (y<=0.0)
  | Bool x, Bool y -> if x then Bool(true) else Bool (not y)
  | String x, _ | _, String x -> Bool false
  | VList x, _ | _, VList x -> Bool false

let helper_greater_equal = function
  | Int x, Int y -> Bool (x >= y)
  | Int x, Float y -> Bool (float_of_int x >= y)
  | Int x, Bool y -> if y then Bool(x>=1) else Bool (x>=0)
  | Float x, Int y -> Bool (x >= float_of_int y)
  | Float x, Float y -> Bool (x >= y)
  | Float x, Bool y -> if y then Bool(x>=1.0) else Bool (x>=0.0)
  | Bool x, Int y -> if x then Bool(y<=1) else Bool (y<=0)
  | Bool x, Float y -> if x then Bool(y<=1.0) else Bool (y<=0.0)
  | Bool x, Bool y -> if x then Bool(true) else Bool (not y)
  | String x, _ | _, String x -> Bool false
  | VList x, _ | _, VList x -> Bool false

let helper_less_than = function
  | Int x, Int y -> Bool (x < y)
  | Int x, Float y -> Bool (float_of_int x < y)
  | Int x, Bool y -> if y then Bool(x<1) else Bool (x<0)
  | Float x, Int y -> Bool (x < float_of_int y)
  | Float x, Float y -> Bool (x < y)
  | Float x, Bool y -> if y then Bool(x<1.0) else Bool (x<0.0)
  | Bool x, Int y -> if x then Bool(y>1) else Bool (y>0)
  | Bool x, Float y -> if x then Bool(y>1.0) else Bool (y>0.0)
  | Bool x, Bool y -> if not x then Bool(true) else Bool (y)
  | String x, _ | _, String x -> Bool false
  | VList x, _ | _, VList x -> Bool false

let helper_less_equal = function
  | Int x, Int y -> Bool (x <= y)
  | Int x, Float y -> Bool (float_of_int x <= y)
  | Int x, Bool y -> if y then Bool(x<=1) else Bool (x<=0)
  | Float x, Int y -> Bool (x <= float_of_int y)
  | Float x, Float y -> Bool (x <= y)
  | Float x, Bool y -> if y then Bool(x<=1.0) else Bool (x<=0.0)
  | Bool x, Int y -> if x then Bool(y>=1) else Bool (y>=0)
  | Bool x, Float y -> if x then Bool(y>=1.0) else Bool (y>=0.0)
  | Bool x, Bool y -> if not x then Bool(y) else Bool (false)
  | String x, _ | _, String x -> Bool false
  | VList x, _ | _, VList x -> Bool false

let rec eval (exp : expr) (st : State.t) : value = match exp with 
  | Binary (e1, op, e2) -> 
    (match op with 
     | Plus -> helper_plus (eval e1 st, eval e2 st)
     | Minus -> helper_plus (eval e1 st, eval (Unary (Minus, e2)) st)
     | Multiply -> helper_multiply (eval e1 st, eval e2 st)
     | Divide -> helper_divide (eval e1 st, eval e2 st)
     | Floor_Divide -> helper_floor (eval e1 st, eval e2 st)
     | Or -> helper_or (eval e1 st, eval e2 st)
     | And -> helper_and (eval e1 st, eval e2 st)
     | Exponent -> helper_exp (eval e1 st, eval e2 st)
     | Equal -> helper_equal (eval e1 st, eval e2 st)
     | Not_Equal -> eval (Unary (Not, Binary(e1, Equal, e2) )) st
     | Modular -> helper_mod (eval e1 st, eval e2 st)
     | Not -> raise (SyntaxError "invalid syntax")
     | Complement -> raise (SyntaxError "invalid syntax")
     | Greater_Than -> helper_greater_than (eval e1 st, eval e2 st)
     | Less_Than -> helper_less_than (eval e1 st, eval e2 st)
     | Greater_Equal -> 
     | Less_Equal -> )
  | Unary (op, e1) ->
    (match op, eval e1 st with 
     | Plus, Int x -> Int x
     | Plus, Float x -> Float x
     | Plus, Bool x -> if x then Int (1) else Int 0
     | Plus, _ -> raise (TypeError "bad operand type for unary +")
     | Minus, Int x -> Int (-x)
     | Minus, Float x -> Float (-.x)
     | Minus, Bool x -> if x then Int (-1) else Int 0
     | Minus, _ -> raise (TypeError "bad operand type for unary -")
     | Not, Int x -> if x = 0 then Bool true else Bool false
     | Not, Float x -> if x = 0. then Bool true else Bool false
     | Not, Bool x -> Bool (not x)
     | Not, String x -> if String.length x = 0 then Bool true else Bool false
     | Not, VList x -> if x = [] then Bool true else Bool false
     | Complement, Int x -> Int (-x-1)
     | Complement, Bool x -> if x then Int (-2) else Int (-1)
     | Complement, _ -> raise (TypeError "bad operand type for unary ~")
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

let if_decider = function
  | Int(0) -> false
  | String("") -> false
  | Bool(false) -> false
  | Float(0.0)  -> false
  | VList([]) -> false
  | _ -> true

let to_bool (exp : expr) (st : State.t) = 
  eval exp st |> if_decider

let rec to_string (value:State.value) : string = (match value with
    | VList x -> List.fold_left (fun x y -> x^(to_string y)^", ") "[" x |> 
                 (fun x -> if String.length x = 1 then x ^ "]" 
                   else String.sub x 0 (String.length x -2) ^ "]")
    | Int x -> string_of_int x
    | Float x -> string_of_float x
    | Bool x -> string_of_bool x |> String.capitalize_ascii
    | String x -> "'" ^ x ^ "'")

let print (value:State.value):unit = value |> to_string |> print_endline

let evaluate input st = match input with
  | Some s, expr -> insert s (eval expr st) st
  | None, expr -> print (eval expr st); st
