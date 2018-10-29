open State
open Evaluate
open Parser

let rec read_if (conds : expr list) (bodies : string list) (acc : string) (new_line : bool) (lines : string list) =
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
    else 
      let indented_line = add_depth (String.trim line) (depth - 1) in
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
    else 
      let line = add_depth (String.trim h) (depth - 1) in
      read_if conds bodies (acc ^ "\n" ^ line) new_line t
    )

let rec read_while (cond : expr) (body : string) (lines : string list) =
  match lines with
  | [] -> print_string "... "; read_while cond body [read_line ()]
  | line::t -> 
    let depth = indent_depth line in
    let indent_line = if depth = 0 then line else add_depth (String.trim line) (depth - 1) in
    (match parse_multiline indent_line with
      | Empty -> (cond, String.trim body)
      | _ -> read_while cond (body ^ "\n" ^ indent_line) t)

let rec read_function (body : string) =
  print_string "... ";
  let line = read_line () in
  let depth = indent_depth line in
  let indent_line = if depth = 0 then line else add_depth (String.trim line) (depth - 1) in
  match parse_multiline line with
  | Empty -> String.trim body
  | _ -> read_function (body ^ "\n" ^ indent_line)

let rec interpret (st:State.t) (lines: string list) (new_line : bool) : State.t =
  match lines with
  | [] -> if new_line then (print_string ">>> "; interpret st [read_line ()] new_line) else st
  | h::t -> (match Parser.parse_line h |> (fun x -> Evaluate.evaluate x st) with
      | exception (SyntaxError x) -> print_endline ("SyntaxError: "^x); interpret st [] true
      | exception (NameError x) -> print_endline ("NameError: "^x); interpret st [] true
      | exception (TypeError x) -> print_endline ("TypeError: "^x); interpret st [] true
      | exception (OverflowError x) -> print_endline ("OverflowError: "^x); interpret st [] true
      | exception (IndentationError x) -> print_endline ("IndentationError"^x); interpret st [] true
      | exception (ZeroDivisionError x)-> print_endline ("ZeroDivisionError: "^x); interpret st [] true
      | exception EmptyInput -> print_endline "??"; interpret st [] true
      | exception (IfMultiline (cond, body)) -> 
        (* Create list of conditions with corresponding line bodies *)
        let (conds, bodies) = read_if [cond] [] body (t = []) t in interpret_if conds bodies st
      | exception (WhileMultiline (cond, init_body)) -> 
        (* Parse out the loop condition and body, process them in [interpret_while] *)
        let (while_cond, while_body) = read_while cond (String.trim init_body) t in
        let new_state = interpret_while while_cond while_body st in
        interpret new_state [] true
      | exception (DefMultiline (name, args, init_body)) -> 
        (* Parse the body of the function *)
        let function_body = read_function (String.trim init_body) in
        let new_st  = Evaluate.evaluate (Some name, Value(Function(args, function_body))) st
        in interpret new_st [] true
      | newst -> interpret newst t new_line)
and interpret_if (conds : expr list) (bodies : string list) (st: State.t) : State.t =
  (* Go through [conds] and respective [bodies] in order. If any condition evaluates to true,
     then we run the corresponding body through the interpreter and throw out the rest *)
  match conds, bodies with
  | cond::c_t, body::b_t -> (match Evaluate.eval cond st |> Evaluate.if_decider with
      | true -> interpret st (String.split_on_char '\n' body) true
      | false -> interpret_if c_t b_t st)
  | _, _ -> raise (SyntaxError "Conditional statements and bodies mismatched")
and interpret_while (cond : expr) (body : string) (st: State.t) : State.t = 
  match Evaluate.to_bool cond st with
  | true -> 
    (* If while conditional is true, then we want to interpret the body, and after that,
       interpret the loop condition until it's false *)
    let new_lines = String.split_on_char '\n' body in
    let new_state = interpret st new_lines false in interpret_while cond body new_state
  | false -> interpret st [] true

let _ = 
  ANSITerminal.(print_string [green]   "***A Python interpreter written in Ocaml***\n");
  ANSITerminal.(print_string [cyan]    "Authors: Patrick, Zaibo, William, and Eric!\n");
  ANSITerminal.(print_string [magenta] "------------------OPython------------------\n"); 
  (* test if statement *)
  (* interpret empty ["y=0"; "if y == 0:"; "x = 1"; "y = 1"; "else:"; "x = 2"; "y=2"] *)
  (* test while statement *)
  (* interpret empty ["x = 0"; "while x < 3:"; "if x == 0:"; "x = x + 2"; "else:"; "x = x + 1"; ""] *)
  (* interpret empty [] *)
  interpret empty [] true
  (* interpret empty ["def f(a, b):"; "if a:"; "return b"] *)
