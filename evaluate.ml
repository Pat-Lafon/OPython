open Parser
open State
open Utils
open Error
open Arithmetic
open Builtin

exception EarlyReturn of State.t

let printt (value:State.value): unit = match to_string value with
  | "NoneVal" -> ()
  | s -> print_endline s

(**[if_decider val] takes in a [State.value] and returns false if the values match
   a "false" value of a respective type. The "empty" or "zero" of each type 
   results in false, and if "non-empty" or "non-zero" then true*) 
let if_decider = function
  | Int(0) -> false
  | String("") -> false
  | Bool(false) -> false
  | Float(0.0)  -> false
  | NoneVal -> false
  | VList(a) -> if !a = [] then false else true
  | _ -> true

(** [eval exp st] takes a variant expression and returns the value of the 
    expression. Evaluates arithmetic expressions, defined variables, and defined
    functions to values. 

    Ex: (eval (23 * (-2)) is a binary expression containing a [Value] expression 
    (23) and a [Unary] expression (-2) with an additional [Mult] operator. *)
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
     | Not, NoneVal -> Bool (true)
     | Not, String x -> if String.length x = 0 then Bool true else Bool false
     | Not, VList x -> if !x = [] then Bool true else Bool false
     | Complement, Int x -> Int (-x-1)
     | Complement, Bool x -> if x then Int (-2) else Int (-1)
     | Complement, _ -> raise (TypeError "bad operand type for unary ~")
     | _ -> raise (SyntaxError "invalid syntax"))
  | Variable x -> 
    (match State.find x st with 
     | Some t -> t
     | None -> raise (NameError ("variable name '"^x^"' is not defined")))
  | Value x -> x
  | List x -> VList (ref (List.map (fun x -> eval x st) x))
  | Function (f, lst) -> 
    if (List.assoc_opt f built_in_functions <> None) 
    then List.assoc f built_in_functions (List.map (fun x -> eval x st) lst)
    else if (List.mem_assoc f st) then run_function f lst st
    else raise (NameError ("function name '"^f^"' is not defined"))

(** [evaluate input st] determines whether or not [input] is an assignment 
    statement; If there is an assignment, the expression the variable is assigned to
    is evaluatedand are placed in the state paired as an association list. If there
    is not an assignemnt,the expression is evaluated. The updated state is 
    returned after either of the two cases occur.*)
and evaluate input st = match input with
  | Some s, expr -> insert s (eval expr st) st
  | None, expr -> printt (eval expr st); st

(** Similar to functions in main.ml, used to evaluate functions *)
and read_if (conds : expr list) (bodies : string list) (acc : string) 
    (new_line : bool) (lines : string list) =
  if new_line then
    let () = print_string "... " in
    let line = read_line () in
    let depth = indent_depth line in
    if depth = 0 then
      (match parse_multiline line with
       | Empty -> (List.rev (Value(Bool(true))::conds), 
                   List.rev (""::(String.trim acc::bodies)), [])
       | Line line -> read_if conds bodies (acc ^ "\n" ^ line) new_line lines
       | If (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) 
                              body new_line lines
       | Elif (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) 
                                body new_line lines
       | Else -> read_if (Value(Bool(true))::conds) (String.trim acc::bodies) 
                   "" new_line lines
       | _ -> raise EmptyInput)
    else 
      let indented_line = add_depth (String.trim line) (depth - 1) in
      read_if conds bodies (acc ^ "\n" ^ indented_line) new_line lines
  else (match lines with
      | [] -> (List.rev (Value(Bool(true))::conds), List.rev 
                 (""::(String.trim acc::bodies)), [])
      | h::t -> 
        let depth = indent_depth h in
        if depth = 0 then
          (match parse_multiline h with
           | Empty -> (List.rev (Value(Bool(true))::conds), List.rev 
                         (""::(String.trim acc::bodies)), lines)
           | Line line -> (List.rev (Value(Bool(true))::conds), List.rev 
                             (""::(String.trim acc::bodies)), lines)
           (*| Line line -> read_if conds bodies (acc ^ "\n" ^ line) new_line t*)
           | If (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) 
                                  body new_line t
           | Elif (cond, body) -> read_if (cond::conds) 
                                    (String.trim acc::bodies) body new_line t
           | Else -> read_if (Value(Bool(true))::conds) 
                       (String.trim acc::bodies) "" new_line t
           | _ -> raise EmptyInput)
        else 
          let line = add_depth (String.trim h) (depth - 1) in
          read_if conds bodies (acc ^ "\n" ^ line) new_line t
    )

and read_while (cond : expr) (body : string) (lines : string list) 
    (new_line : bool) =
  match lines with
  | [] -> if new_line then (print_string "... "; 
                            read_while cond body [read_line ()] new_line)
    else (cond, String.trim body, [])
  | line::t -> 
    let depth = indent_depth line in
    if depth = 0 then
      (cond, String.trim body, lines)
    else let indent_line = add_depth (String.trim line) (depth - 1) in
      (match parse_multiline indent_line with
       | Empty -> (cond, String.trim body, lines)
       | _ -> read_while cond (body ^ "\n" ^ indent_line) t new_line)

and read_for (body : string) (lines : string list) (new_line : bool) =
  match lines with
  | [] -> if new_line then (print_string "... "; 
                            read_for body [read_line ()] new_line)
    else (String.trim body, [])
  | line::t -> 
    let depth = indent_depth line in
    if depth = 0 then
      (String.trim body, lines)
    else let indent_line = add_depth (String.trim line) (depth - 1) in
      (match parse_multiline indent_line with
       | Empty -> (String.trim body, lines)
       | _ -> read_for (body ^ "\n" ^ indent_line) t new_line)

and read_function (body : string) (lines : string list) (new_line : bool) =
  match lines with
  | [] -> if new_line then (print_string "... "; 
                            read_function body [read_line ()] new_line)
    else (String.trim body, [])
  | line::t -> 
    let depth = indent_depth line in
    if depth = 0 then (String.trim body, lines)
    else let indent_line = add_depth (String.trim line) (depth - 1) in
      (match parse_multiline line with
       | Empty -> (String.trim body, lines)
       | _ -> read_function (body ^ "\n" ^ indent_line) t new_line)

and interpret (st:State.t) (lines: string list) (new_line : bool) : State.t =
  match lines with
  | [] -> st
  | h::t -> (match Parser.parse_line h |> (fun x -> evaluate x st) with
      | exception (SyntaxError x) -> print_endline ("SyntaxError: "^x); 
        interpret st [] new_line
      | exception (NameError x) -> print_endline ("NameError: "^x); 
        interpret st [] new_line
      | exception (TypeError x) -> print_endline ("TypeError: "^x); 
        interpret st [] new_line
      | exception (OverflowError x) -> print_endline ("OverflowError: "^x); 
        interpret st [] new_line
      | exception (IndentationError x) -> print_endline ("IndentationError"^x); 
        interpret st [] new_line
      | exception (ZeroDivisionError x)-> print_endline 
                                            ("ZeroDivisionError: "^x); interpret st [] new_line
      | exception (ReturnExpr expr) -> raise (EarlyReturn(evaluate 
                                                            (Some "return", expr) st))
      | exception EmptyInput -> interpret st t new_line
      | exception (IfMultiline (cond, body)) -> 
        (* Create list of conditions with corresponding line bodies *)
        let (conds, bodies, remaining_lines)=read_if [cond] [] body (t = []) t in 
        let new_state = interpret_if conds bodies st in
        interpret new_state remaining_lines false
      | exception (ForMultiline (iter, arg, body)) -> 
        let (for_body, remaining_lines) = read_for body t (t = []) in 
        let old_state = st in
        let new_state = interpret_for iter arg body st in
        (* let new_state = interpret_if conds bodies st in *)
        interpret new_state remaining_lines false
      | exception (WhileMultiline (cond, init_body)) -> 
        (* Parse out the loop condition and body, process them in 
           [interpret_while] *)
        let (while_cond, while_body, remaining_lines) = read_while cond 
            (String.trim init_body) t new_line in
        let new_state = interpret_while while_cond while_body st in
        interpret new_state remaining_lines new_line
      | exception (DefMultiline (name, args, init_body)) -> 
        (* Parse the body of the function *)
        let (function_body, remaining_lines) = read_function 
            (String.trim init_body) t new_line in
        (* Assign function definition to function name in global state *)
        let new_st  = evaluate (Some name, 
                                Value(Function(name, args, function_body))) st in
        interpret new_st remaining_lines new_line
      | newst -> interpret newst t new_line)
and interpret_if (conds : expr list) (bodies : string list) (st: State.t)
  : State.t =
  (* Go through [conds] and respective [bodies] in order. If any condition 
     evaluates to true, then we run the corresponding body through the interpreter 
     and throw out the rest *)
  match conds, bodies with
  | cond::c_t, body::b_t -> (match eval cond st |> if_decider with
      | true -> interpret st (String.split_on_char '\n' body) false
      | false -> interpret_if c_t b_t st)
  | _, _ -> raise (SyntaxError "Conditional statements and bodies mismatched")
and interpret_while (cond : expr) (body : string) (st: State.t) : State.t = 
  match to_bool cond st with
  | true -> 
    (* If while conditional is true, then we want to interpret the body, 
       and after that, interpret the loop condition until it's false *)
    let new_lines = String.split_on_char '\n' body in
    let new_state = interpret st new_lines false in 
    interpret_while cond body new_state
  | false -> interpret st [] false

and interpret_for (iter : expr) (arg : string) (body : string) (st: State.t) : State.t = 
  match iter with
  | h::t -> 
    let new_lines = String.split_on_char '\n' body in
    let new_state = interpret st new_lines false in 
    interpret_while cond body new_state
  | [] -> interpret st [] false


(** [run_function f_name expr_args global_st] runs function [f_name] with arguments
    [expr_args] and returns the return value of the function *)
and run_function f_name expr_args global_st = 
  match List.assoc f_name global_st with
  | Function(name, string_args, body) as f -> 
    let func_st = create_function_state expr_args string_args global_st
        global_st f_name f in
    let new_state = (try interpret func_st (String.split_on_char '\n' 
                                              (String.trim body)) false with
                    | EarlyReturn st -> st) in
    (match State.find "return" new_state with
     | None -> NoneVal
     | Some x -> x)
  | _ -> raise (NameError (f_name ^ " cannot be called"))

(** Initialize a function scope using arguments passed in*)
and create_function_state exprs args func_st global_st func_name f = 
  match exprs, args with
  | [], [] -> State.insert func_name f func_st
  | expr::e_t, arg::a_t -> 
    let value = eval expr global_st 
    in create_function_state e_t a_t (State.insert arg value func_st) 
      global_st func_name f
  | _, _ -> raise (SyntaxError ("Arguments in function do not match"))

(**[to_bool exp st] evaluates an expression and passes the value through [if_decider].*)
and to_bool (exp : expr) (st : State.t) = 
  eval exp st |> if_decider

(**[to_string] returns the string of a value*)
and to_string (value:State.value) : string = 
  match value with
  | VList x -> List.fold_left (fun x y -> x^(to_string y)^", ") "[" !x |> 
               (fun x -> if String.length x = 1 then x ^ "]" 
                 else String.sub x 0 (String.length x -2) ^ "]")
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Bool x -> string_of_bool x |> String.capitalize_ascii
  | Function f -> 
    let (name, args, body) = f in
    let address = 2*(Obj.magic (ref f)) in
    "<function " ^ name ^ " at " ^ Printf.sprintf "0x%08x" address ^ ">"
  | String x -> "'" ^ x ^ "'"
  | NoneVal -> "None"

and printt (value:State.value) : unit = match to_string value with
  | "None" -> ()
  | s -> print_endline s

let add_function (st: State.t) (fnc_name : string) (args : string list) (body : string) =
  let func = Function(fnc_name, args, body) in insert fnc_name func st