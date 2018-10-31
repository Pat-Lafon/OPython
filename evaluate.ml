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
  | Bool x, Bool y -> Int(if x then 1 + (if y then 1 else 0) else 0 + (if y then 1 else 0))
  | Bool x , String y -> raise (TypeError "unsupported operand type for +")
  | Bool x, VList y -> raise (TypeError "unsupported operand type for +")
  | String x, String y -> String (x ^ y)
  | String x, _ -> raise (TypeError "can only concatenate str to str")
  | VList x, VList y -> VList (ref(!x @ !y))
  | VList x, _-> raise (TypeError "can only concatenate list to list")
  | Function t, _-> raise (TypeError "unsupported operand type function for +")
  | _, Function t-> raise (TypeError "unsupported operand type function for +")

let helper_multiply = function 
  | Int x, Int y -> Int (x * y)
  | Int x, Float y -> Float (float_of_int x *. y)
  | Int x, Bool y -> if y then Int x else Int 0
  | Int x, String y -> raise (TypeError ("unsupported operand type for *"))
  | Int x, VList y -> VList (ref(mul !y x [] (@)))
  | Float x, Int y -> Float (float_of_int y *. x)
  | Float x, Float y -> Float (x *. y)
  | Float x, Bool y -> if y then Float (x *. float_of_int 1) else Float 0.0
  | Float x, String y -> raise (TypeError "can't multiply sequence by non-int of type 'float'")
  | Float x, VList y -> raise (TypeError "can't multiply sequence by non-int")
  | Bool x, Int y -> if x then Int y else Int 0
  | Bool x, Float y -> if x then Float (y *. float_of_int 1) else Float 0.0
  | Bool x, Bool y -> if x && y then Int 1 else Int 0
  | Bool x, String y -> if x then String y else String ""
  | Bool x, VList y -> if x then VList y else VList (ref([]))
  | String x, Int y ->  String (mul x y "" (^))
  | String x, Float y -> raise (TypeError "can't multiply sequence by non-int of type 'float'")
  | String x, Bool  y -> if y then String x else String ""
  | String x, String y -> raise (TypeError "can't multiply sequence by non-int of type 'str'")
  | String x, VList y -> raise (TypeError "can't multiply sequence by non-int")
  | VList x, Int y -> VList (ref(mul !x y [] (@)))
  | VList x, Float y -> raise (TypeError "can't multiply sequence by non-int")
  | VList x, Bool y -> if y then VList x else VList (ref([]))
  | VList x, String y -> raise (TypeError "can't multiply sequence by non-int")
  | VList x, VList y -> raise (TypeError "can't multiply sequence by non-int")
  | Function t, _-> raise (TypeError "unsupported operand type function for *")
  | _, Function t-> raise (TypeError "unsupported operand type function for *")

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
  | Function t, _-> raise (TypeError "unsupported operand type function for /")
  | _, Function t-> raise (TypeError "unsupported operand type function for /")

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
  | Function t, _-> raise (TypeError "unsupported operand type function for %")
  | _, Function t-> raise (TypeError "unsupported operand type function for %")

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
  | Function t, _-> raise (TypeError "unsupported operand type function for **")
  | _, Function t-> raise (TypeError "unsupported operand type function for **")

let helper_and = function
  | Int x, y -> if x = 0 then Int 0 else y
  | Float x, y-> if x = 0. then Float 0. else y
  | Bool x, y -> if not x then Bool(x) else y
  | String x, y -> if x = "" then String "" else y
  | VList x, y -> if !x = [] then VList x else y
  | Function x, y -> y

let helper_or = function 
  | Int x, y -> if x<>0 then Int x else y
  | Float x, y -> if x <> 0. then Float x else y
  | Bool x, y -> if x then Bool x else y
  | String x, y -> if x <> "" then String x else y
  | VList x, y -> if !x <> [] then VList x else y
  | Function x, y -> Function x

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
  | Function (args1, body1), Function (args2, body2) -> Bool (body1 = body2)
  | _, _ -> Bool false

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
  | _ -> raise (NameError ("Operation not defined for functions"))

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
  | _ -> raise (NameError ("Operation not defined for functions"))

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
  | _ -> raise (NameError ("Operation not defined for functions"))

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
  | _ -> raise (NameError ("Operation not defined for given types"))

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
  | _ -> raise (NameError ("Operation not defined for given types"))


(** [eval exp st] takes a variant expression and returns the value of the 
    expression. Evaluates arithmetic expressions, defined variables, and defined
    functions to values. 

    Ex: (eval (23 * (-2)) is a binary expression containing a [Value] expression (23) and 
    a [Unary] expression (-2) with an additional [Mult] operator. *)
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
     | Greater_Equal -> helper_greater_equal (eval e1 st, eval e2 st)
     | Less_Equal -> helper_less_equal (eval e1 st, eval e2 st))
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
     | Not, VList x -> if !x = [] then Bool true else Bool false
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
    in let rf = ref (help x) in VList(rf)
  | Function (f, lst) -> 
    if (List.mem f built_in_function_names) then 

      (List.assoc f built_in_functions) lst st 
    else if (List.mem_assoc f st) 
    then run_function f lst st
    else raise (NameError ("name '"^f^"' is not defined"))

and index (lst : expr list) (st : State.t) : State.value  = let func = function
    | Int x, Int y -> (x = y)
    | Int x, Float y -> (float_of_int x = y)
    | Int x, Bool y -> if y then (x=1) else (x=0)
    | Float x, Int y -> (x = float_of_int y)
    | Float x, Float y -> (x = y)
    | Float x, Bool y -> if y then (x=1.0) else (x=0.0)
    | Bool x, Int y -> if x then (y=1) else (y=0)
    | Bool x, Float y -> if x then (y=1.0) else (y=0.0)
    | Bool x, Bool y -> (x = y)
    | String x, _ | _, String x -> false
    | VList x, _ | _, VList x -> false
    | Function (args1, body1), Function (args2, body2) -> (body1 = body2)
    | _, _ -> false
  in let idx value l = List.find (fun x -> func (x,value)) l 
  in match List.map (fun x -> eval x st) lst with
  | h::VList(t)::[] -> idx h !t
  | String(s1)::String(s)::[] -> 
    let rec search sub str = if (String.length sub > String.length str) then 
        String.length sub else 
      if String.sub str 0 (String.length sub) = sub then
        0 else (1 + search sub (String.sub str 1 (String.length sub)))
    in if (search s1 s >= String.length s) then Int(-1) else Int(search s1 s)
  | _ -> raise (TypeError ("Operation not supported"))

and splice (lst : expr list) (st : State.t) : State.value = 
  let rec helper lst x y z = if z = 0 then 
      raise (ValueError "Third argument must not be zero") else 
    if y >= List.length lst then helper lst x (List.length lst) z else 
    if x >= y then [] else List.nth lst x :: helper lst (x+z) y z in
  let decider x len = if x < - len then 0 else 
    if x < 0 then x + len else if x > len then len else x in
  let splice_helper lst x y z = 
    helper lst (decider x (List.length lst)) (decider y (List.length lst)) (decider z (List.length lst)) in
  let rec helper_str str x y z = if z = 0 then 
      raise (ValueError "Third argument must not be zero") else 
    if y >= String.length str then helper_str str x (List.length lst) z else 
    if x >= y then "" else String.concat "" ([String.sub str x 1;  helper_str str (x+z) y z]) in
  let splice_str str x y z =
    helper_str str (decider x (String.length str)) 
      (decider y (String.length str)) (decider z (String.length str)) in
  match lst with
  | h1::h2::h3::[] -> begin match (eval h1 st, eval h2 st, eval h3 st) with 
      | String(s), String(""), String("") -> String(s)
      | String(s), String(""), Int(x) -> String(splice_str s 0 x 1)
      | String(s), Int(x), String("") -> String(splice_str s x (String.length s) 1)
      | String(s), Int(x), Int(y) -> String(splice_str s x y 1)
      | VList(l), String(""), String("") -> VList(l)
      | VList(l), String(""), Int(x) -> VList(ref(splice_helper !l 0 x 1))
      | VList(l), Int(x), String("") -> VList(ref(splice_helper !l x (List.length !l) 1))
      | VList(l), Int(x), Int(y) -> VList(ref(splice_helper !l x y 1))
      | _ -> raise (TypeError ("Operation not supported"))
    end
  | h1::h2::h3::h4::[] -> begin match (eval h1 st,eval h2 st, eval h3 st, eval h4 st) with 
      | String(s), String(""), String(""), String("") -> String(s)
      | String(s), String(""), String(""), Int(x) -> String(splice_str s 0 (String.length s) x)
      | String(s), String(""), Int(x), String("") -> String(splice_str s 0 x 1)
      | String(s), Int(x), String(""), String("") -> String(splice_str s x (String.length s) 1) 
      | String(s), Int(x), Int(y), String("") -> String(splice_str s x y 1) 
      | String(s), Int(x), String(""), Int(y) -> String(splice_str s x (String.length s) y) 
      | String(s), String(""), Int(x), Int(y) -> String(splice_str s 0 x y)  
      | String(s), Int(x), Int(y), Int(z) -> String(splice_str s x y z) 
      | VList(l), String(""), String(""), String("") -> VList(l)
      | VList(l), String(""), String(""), Int(x) -> VList(ref(splice_helper !l 0 (List.length !l) x))
      | VList(l), String(""), Int(x), String("") -> VList(ref(splice_helper !l 0 x 1))
      | VList(l), Int(x), String(""), String("") -> VList(ref(splice_helper !l x (List.length !l) 1))
      | VList(l), Int(x), Int(y), String("") -> VList(ref(splice_helper !l x y 1))
      | VList(l), Int(x), String(""), Int(y) -> VList(ref(splice_helper !l x (List.length !l) y))
      | VList(l), String(""), Int(x), Int(y) -> VList(ref(splice_helper !l 0 x y))
      | VList(l), Int(x), Int(y), Int(z) -> VList(ref(splice_helper !l x y z))
      | _ -> raise (TypeError ("Operation not supported"))
    end 
  | _ -> raise (TypeError ("Operation not supported"))


and run_function f_name expr_args global_st = 
  match List.assoc f_name global_st with
  | Function(string_args, body) -> 
    let func_st = create_function_state expr_args string_args State.empty global_st in
    interpret func_st (String.split_on_char '\n' (String.trim body))
  | _ -> raise (NameError (f_name ^ " cannot be called"))

and create_function_state exprs args func_st global_st = 
  match exprs, args with
  | [], [] -> func_st
  | expr::e_t, arg::a_t -> 
    let value = eval expr global_st in create_function_state e_t a_t (State.insert arg value func_st) global_st
  | _, _ -> raise (NameError ("Arguments in function do not match"))

and append (explist : expr list) (st : State.t) = 

  let vallist = List.map (fun x -> eval x st) explist in
  match vallist with
  | lst::valu::[] -> 
    (match lst with
     | VList x -> x := List.rev(valu::List.rev(!x)); VList(x)
     | _ -> failwith("not a list")
    )
  | _ -> failwith("not enough args")

and helper_printt (explist : expr list) (st : State.t)=
  match explist with
  | [] -> ""
  | h::t -> to_string(eval h st) ^ " " ^ helper_printt t st

and printt (explist : expr list) (st: State.t) =
  String(helper_printt explist st) (*|> print_endline *)

and len (lst : expr list) (st : State.t) : State.value = match lst with
  | h::[] -> begin match eval h st with 
      | VList(l) -> Int(List.length !l)
      | String(s) -> Int (String.length s)
      | _ -> raise (TypeError ("Object of that type has no len()"))
    end
  | _ -> raise (TypeError("Length takes exactly one argument"))

and helper_range s f i = if i = 0 then 
    raise (ValueError "Third argument must not be zero") else 
  if i > 0 then (if s >= f then [] else 
                   Int(s) :: helper_range (s+i) f i) else 
    (if s <= f then [] else Int(s) :: helper_range (s+i) f i)


and range (lst : expr list) (st : State.t) : State.value = match lst with
  | h::[] -> begin match eval h st with 
      | Int(a) -> VList (ref(helper_range 0 a 1))
      | _ -> raise (TypeError ("Unsupported type for this operation"))
    end
  | h1::h2::[] -> begin match (eval h1 st,eval h2 st) with 
      | (Int(a),Int(b)) -> VList (ref(helper_range a b 1))
      | _ -> raise (TypeError ("Unsupported type for this operation"))
    end
  | h1::h2::h3::[] -> begin 
      match (eval h1 st, eval h2 st, eval h3 st) with 
      | (Int(a),Int(b),Int(c)) -> VList (ref(helper_range a b c))
      | _ -> raise (TypeError ("Unsupported type for this operation"))
    end
  | _ -> raise (TypeError("Range takes at most three arguments"))




(** Type casts *)
and chr (explist : expr list) (st: State.t) =
  let vallist = List.map (fun x -> eval x st) explist in
  match vallist with
  | h::[] ->(
      match h with 
      | Int(x) -> if (x>=0) && (x<=1114111) then String(Char.escaped((Uchar.to_char(Uchar.of_int(x))))) else failwith("int out of bounds")
      | _ -> failwith("requires int")
    )
  | _ -> failwith("needs 1 arg, this is not 1 arg")


and bool (explist: expr list) (st: State.t) = 
  let vallist = List.map (fun x -> eval x st) explist in
  match vallist with
  | h::[] ->(
      match h with 
      | Bool(x) -> if x then Bool true else Bool false
      | Int(x) -> if x = 0 then Bool false else Bool true
      | Float(x) -> if x = 0. then Bool false else Bool true
      | String(x) -> if x = "" then Bool false else Bool true
      | VList(x) -> if !x = [] then Bool false else Bool true
      | _ -> failwith("not really sure what to do with Function")
    )
  | [] -> Bool(false)
  | _ -> failwith("neither empty nor 1 arg")

and float (explist: expr list) (st: State.t) = 
  let vallist = List.map (fun x -> eval x st) explist in
  match vallist with
  | h::[] ->(
      match h with 
      | Int(x) -> Float(float_of_int(x))
      | Float(x) -> Float(x)
      | String(x) -> if float_of_string_opt(x) <> None then Float(float_of_string(x)) else failwith("can't do that")
      | _ -> failwith("not really sure what to do with Function")
    )
  | [] -> Float(0.0)
  | _ -> failwith("neither empty nor 1 arg")

and int (explist: expr list) (st: State.t) = 
  let vallist = List.map (fun x -> eval x st) explist in
  match vallist with
  | h::[] ->(
      match h with 
      | Int(x) -> Int(x)
      | Float(x) -> Int(int_of_float(x))
      | String(x) -> if int_of_string_opt(x) <> None then Int(int_of_string(x)) else failwith("can't do that")
      | Bool x -> if x then Int 1 else Int 0
      | VList _ -> raise (TypeError "int() argument must be a string, a bytes-like object or a number, not 'list'")
      | Function _ -> failwith "Should not be possible"
    )
  | [] -> Int(0)
  | _ -> raise (TypeError "int() can't convert more than one argument")



and built_in_function_names = ["append"; "len"; "range"; 
                               "printt"; "chr"; "bool"; "float"; 
                               "int"; "range"; "splice"; "index"]

and built_in_functions = [("append", append); ("len", len); ("range", range); 
                          ("printt", printt); ("chr", chr); ("bool", bool); ("float", float); 
                          ("int",int); ("range", range); ("splice", splice); ("index", index)]


(** [evaluate input st] determines whether or not [input] is an assignment statement;
    If there is an assignment, the expression the variable is assigned to is evaluated
    and are placed in the state paired as an association list. If there is not an assignemnt,
    the expression is evaluated. The updated state is returned after either of the two cases
    occur.*)
and evaluate input st = match input with
  | Some s, expr -> insert s (eval expr st) st
  | None, expr -> print (eval expr st); st

and read_if (conds : expr list) (bodies : string list) (acc : string) (new_line : bool) (lines : string list) =
  if new_line then
    let () = print_string "... " in
    let line = read_line () in
    let depth = indent_depth line in
    if depth = 0 then
      (match parse_multiline line with
       | Empty -> (List.rev (Value(Bool(true))::conds), List.rev (""::(String.trim acc::bodies)))
       | Line line -> read_if conds bodies (acc ^ "\n" ^ line) new_line lines
       | If (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) body new_line lines
       | Elif (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) body new_line lines
       | Else -> read_if (Value(Bool(true))::conds) (String.trim acc::bodies) "" new_line lines
       | _ -> raise EmptyInput)
    else let indented_line = add_depth (String.trim line) (depth - 1) in
      read_if conds bodies (acc ^ "\n" ^ indented_line) new_line lines
  else (match lines with
      | [] -> List.rev (Value(Bool(true))::conds), List.rev (""::(String.trim acc::bodies))
      | h::t -> 
        let depth = indent_depth h in
        if depth = 0 then
          (match parse_multiline h with
           | Empty -> (List.rev (Value(Bool(true))::conds), List.rev (""::(String.trim acc::bodies)))
           | Line line -> read_if conds bodies (acc ^ "\n" ^ line) new_line t
           | If (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) body new_line t
           | Elif (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) body new_line t
           | Else -> read_if (Value(Bool(true))::conds) (String.trim acc::bodies) "" new_line t
           | _ -> raise EmptyInput)
        else let line = add_depth (String.trim h) (depth - 1) in
          read_if conds bodies (acc ^ "\n" ^ line) new_line t
    )
and read_while (cond : expr) (body : string) (lines : string list) =
  match lines with
  | [] -> read_while cond body [read_line ()]
  | h::t -> (match parse_multiline h with
      | Empty -> (cond, String.trim body)
      | _ -> read_while cond (body ^ "\n" ^ String.trim h) t)
and read_function (body : string) =
  let line = read_line () in
  match parse_multiline line with
  | Empty -> body
  | _ -> read_function (body ^ "\n" ^ String.trim line)
and interpret (st:State.t) (lines: string list) : State.value =
  match lines with
  | [] -> Bool(true)
  | h::t -> (match Parser.parse_line h |> (fun x -> evaluate x st) with
      | exception (SyntaxError x) -> print_endline ("SyntaxError: "^x); interpret st []
      | exception (NameError x) -> print_endline ("NameError: "^x); interpret st []
      | exception (TypeError x) -> print_endline ("TypeError: "^x); interpret st []
      | exception (OverflowError x) -> print_endline ("OverflowError: "^x); interpret st []
      | exception (IndentationError x) -> print_endline ("IndentationError"^x); interpret st []
      | exception (ZeroDivisionError x)-> print_endline ("ZeroDivisionError: "^x); interpret st []
      | exception EmptyInput -> interpret st []
      | exception (ReturnExpr expr) -> eval expr st
      | exception (IfMultiline (cond, body)) -> 
        (* Create list of conditions with corresponding line bodies *)
        let (conds, bodies) = read_if [cond] [] body (t = []) t in 
        interpret_if conds bodies st
      | exception (WhileMultiline (cond, init_body)) -> 
        (* Parse out the loop condition and body, process them in [interpret_while] *)
        let (while_cond, while_body) = read_while cond (String.trim init_body) t in
        let while_line = h in
        interpret_while while_cond while_body while_line st
      | exception (DefMultiline (name, args, init_body)) -> 
        (* Parse the body of the function *)
        let function_body = read_function (String.trim init_body) in
        let new_st  = evaluate (Some name, Value(Function(args, function_body))) st
        in interpret new_st []
      | newst -> interpret newst t)
and interpret_if (conds : expr list) (bodies : string list) (st: State.t) : State.value =
  (* Go through [conds] and respective [bodies] in order. If any condition evaluates to true,
     then we run the corresponding body through the interpreter and throw out the rest *)
  match conds, bodies with
  | cond::c_t, body::b_t -> (match eval cond st |> if_decider with
      | true -> interpret st (String.split_on_char '\n' body)
      | false -> interpret_if c_t b_t st)
  | _, _ -> raise (SyntaxError "Conditional statements and bodies mismatched")
and interpret_while (cond : expr) (body : string) (while_line) (st: State.t) : State.value = 
  match to_bool cond st with
  | true -> 
    (* If while conditional is true, then we want to interpret the body, and after that,
       interpret the loop condition until it's false *)
    let split_body = String.split_on_char '\n' body in
    let new_lines = split_body @ [while_line] @ split_body @ [""] in
    interpret st new_lines
  | false -> interpret st []


(**[if_decider val] takes in a [State.value] and returns false if the values match
   a "false" value of a respective type. The "empty" or "zero" of each type results in 
   false, and if "non-empty" or "non-zero" then true*) 
and if_decider = function
  | Int(0) -> false
  | String("") -> false
  | Bool(false) -> false
  | Float(0.0)  -> false
  | VList(a) -> if !a = [] then false else true
  | _ -> true

(**[to_bool exp st] evaluates an expression and passes the value through [if_decider].*)
and to_bool (exp : expr) (st : State.t) = 
  eval exp st |> if_decider

and to_string (value:State.value) : string = (match value with
    | VList x -> List.fold_left (fun x y -> x^(to_string y)^", ") "[" !x |> 
                 (fun x -> if String.length x = 1 then x ^ "]" 
                   else String.sub x 0 (String.length x -2) ^ "]")
    | Int x -> string_of_int x
    | Float x -> string_of_float x
    | Bool x -> string_of_bool x |> String.capitalize_ascii
    | Function (args, body) -> "<function 3100 at 0x10b026268>"
    | String x -> "'" ^ x ^ "'")

and print (value:State.value):unit = value |> to_string |> print_endline

let add_function (st: State.t) (fnc_name : string) (args : string list) (body : string) =
  let func = Function(args, body) in insert fnc_name func st


