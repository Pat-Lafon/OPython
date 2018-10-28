open State
open Evaluate
open Parser

let rec read_if (conds : expr list) (bodies : string list) (acc : string) =
  print_string "... ";
  match parse_multiline (read_line ()) with
  | Empty -> (List.rev (Value(Bool(true))::conds), List.rev (""::(String.trim acc::bodies)))
  | Line line -> read_if conds bodies (acc ^ "\n" ^ line)
  | If (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) body
  | Elif (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) body
  | Else -> read_if (Value(Bool(true))::conds) (String.trim acc::bodies) ""
  | _ -> raise EmptyInput

let rec read_while (cond : expr) (body : string) (lines : string list) =
  match lines with
  | [] -> print_string "... "; read_while cond body [read_line ()]
  | h::t -> (match parse_multiline h with
            | Empty -> (cond, String.trim body)
            | _ -> read_while cond (body ^ "\n" ^ String.trim h) t)

let rec read_function (body : string) =
  print_string "... ";
  let line = read_line () in
  match parse_multiline line with
  | Empty -> body
  | _ -> read_function (body ^ "\n" ^ String.trim line)

let rec interpret (st:State.t) (lines: string list) : unit =
  match lines with
  | [] -> print_string ">>> "; interpret st [read_line ()]
  | h::t -> (match Parser.parse_line h |> (fun x -> Evaluate.evaluate x st) with
    | exception (SyntaxError x) -> print_endline ("SyntaxError: "^x); interpret st []
    | exception (NameError x) -> print_endline ("NameError: "^x); interpret st []
    | exception (TypeError x) -> print_endline ("TypeError: "^x); interpret st []
    | exception (OverflowError x) -> print_endline ("OverflowError: "^x); interpret st []
    | exception (IndentationError x) -> print_endline ("IndentationError"^x); interpret st []
    | exception (ZeroDivisionError x)-> print_endline ("ZeroDivisionError: "^x); interpret st []
    | exception EmptyInput -> interpret st []
    | exception (IfMultiline (cond, body)) -> 
      (* Create list of conditions with corresponding line bodies *)
      let (conds, bodies) = read_if [cond] [] body in interpret_if conds bodies st
    | exception (WhileMultiline (cond, init_body)) -> 
      (* Parse out the loop condition and body, process them in [interpret_while] *)
      let (while_cond, while_body) = read_while cond (String.trim init_body) t in
      let while_line = h in
      interpret_while while_cond while_body while_line st
    | exception (DefMultiline (name, args, init_body)) -> 
      (* Parse the body of the function *)
      let function_body = read_function (String.trim init_body) in
      let new_st  = Evaluate.evaluate (Some name, Value(Function(args, function_body))) st
        in interpret new_st []
    | newst -> interpret newst t)
and interpret_if (conds : expr list) (bodies : string list) (st: State.t) : unit =
  (* Go through [conds] and respective [bodies] in order. If any condition evaluates to true,
  then we run the corresponding body through the interpreter and throw out the rest *)
  match conds, bodies with
  | cond::c_t, body::b_t -> print_endline ("@" ^ body ^ "@"); (match Evaluate.eval cond st |> Evaluate.if_decider with
      | true -> interpret st (String.split_on_char '\n' body)
      | false -> interpret_if c_t b_t st)
  | _, _ -> raise (SyntaxError "Conditional statements and bodies mismatched")
and interpret_while (cond : expr) (body : string) (while_line) (st: State.t) : unit = 
  match Evaluate.to_bool cond st with
  | true -> 
    (* If while conditional is true, then we want to interpret the body, and after that,
    interpret the loop condition until it's false *)
    let split_body = String.split_on_char '\n' body in
    let new_lines = split_body @ [while_line] @ split_body @ [""] in
     interpret st new_lines
  | false -> interpret st []

let rec run (st:State.t) (lines: string list) : unit =
  match lines with
  | [] -> ()
  | h::t -> (match Parser.parse_line h |> (fun x -> Evaluate.evaluate x st) with
    | exception (SyntaxError x) -> print_endline ("SyntaxError: "^x); run st []
    | exception (NameError x) -> print_endline ("NameError: "^x); run st []
    | exception (TypeError x) -> print_endline ("TypeError: "^x); run st []
    | exception (OverflowError x) -> print_endline ("OverflowError: "^x); run st []
    | exception (IndentationError x) -> print_endline ("IndentationError"^x); run st []
    | exception (ZeroDivisionError x)-> print_endline ("ZeroDivisionError: "^x); run st []
    | exception EmptyInput -> run st []
    | exception (IfMultiline (cond, body)) -> 
      (* Create list of conditions with corresponding line bodies *)
      let (conds, bodies) = read_if [cond] [] body in run_if conds bodies st
    | exception (WhileMultiline (cond, init_body)) -> 
      (* Parse out the loop condition and body, process them in [run_while] *)
      let (while_cond, while_body) = read_while cond (String.trim init_body) t in
      let while_line = h in
      run_while while_cond while_body while_line st
    | exception (DefMultiline (name, args, init_body)) -> 
      (* Parse the body of the function *)
      let function_body = read_function (String.trim init_body) in
      let new_st  = Evaluate.evaluate (Some name, Value(Function(args, function_body))) st
        in run new_st []
    | newst -> run newst t)
and run_if (conds : expr list) (bodies : string list) (st: State.t) : unit =
  (* Go through [conds] and respective [bodies] in order. If any condition evaluates to true,
  then we run the corresponding body through the runer and throw out the rest *)
  match conds, bodies with
  | cond::c_t, body::b_t -> print_endline ("@" ^ body ^ "@"); (match Evaluate.eval cond st |> Evaluate.if_decider with
      | true -> run st (String.split_on_char '\n' body)
      | false -> run_if c_t b_t st)
  | _, _ -> raise (SyntaxError "Conditional statements and bodies mismatched")
and run_while (cond : expr) (body : string) (while_line) (st: State.t) : unit = 
  match Evaluate.to_bool cond st with
  | true -> 
    (* If while conditional is true, then we want to run the body, and after that,
    run the loop condition until it's false *)
    let split_body = String.split_on_char '\n' body in
    let new_lines = split_body @ [while_line] @ split_body @ [""] in
     run st new_lines
  | false -> run st []

(* let _ = 
  ANSITerminal.(print_string [green]   "***A Python interpreter written in Ocaml***\n");
  ANSITerminal.(print_string [cyan]    "Authors: Patrick, Zaibo, William, and Eric!\n");
  ANSITerminal.(print_string [magenta] "------------------OPython------------------\n"); 
  interpret empty [] *)

let test lines = run empty lines