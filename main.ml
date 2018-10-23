open State
open Evaluate
open Parser

let rec if_multiline (conds : expr list) (bodies : string list) (acc : string) =
  print_string "... ";
  match parse_multiline (read_line ()) with
  | Empty -> (List.rev (Value(Bool(true))::conds), List.rev (""::(String.trim acc::bodies)))
  | Line line -> if_multiline conds bodies (acc ^ "\n" ^ line)
  | If (cond, body) -> if_multiline (cond::conds) (String.trim acc::bodies) body
  | Elif (cond, body) -> if_multiline (cond::conds) (String.trim acc::bodies) body
  | Else -> if_multiline (Value(Bool(true))::conds) (String.trim acc::bodies) ""
  | _ -> raise EmptyInput

let rec while_multiline (cond : expr) (body : string) (lines : string list) =
  match lines with
  | [] -> print_string "... "; while_multiline cond body [read_line ()]
  | h::t -> (match parse_multiline h with
            | Empty -> (cond, String.trim body)
            | _ -> while_multiline cond (body ^ "\n" ^ String.trim h) t)

let rec main (st:State.t) (lines: string list) : unit =
  match lines with
  | [] -> print_string ">>> "; main st [read_line ()]
  | h::t -> (match Parser.parse_line h |> (fun x -> Evaluate.evaluate x st) with
    | exception (SyntaxError x) -> print_endline ("SyntaxError: "^x); main st []
    | exception (NameError x) -> print_endline ("NameError: "^x); main st []
    | exception (TypeError x) -> print_endline ("TypeError: "^x); main st []
    | exception (OverflowError x) -> print_endline ("OverflowError: "^x); main st []
    | exception (IndentationError x) -> print_endline ("IndentationError"^x); main st []
    | exception (ZeroDivisionError x)-> print_endline ("ZeroDivisionError: "^x); main st []
    | exception EmptyInput -> main st []
    | exception (IfMultiline (cond, body)) -> 
      let (conds, bodies) = if_multiline [cond] [] body in main_if conds bodies st
    | exception (WhileMultiline (cond, init_body)) -> 
      let (while_cond, while_body) = while_multiline cond (String.trim init_body) t in
      let while_line = h in
      main_while while_cond while_body while_line st
    | newst -> main newst t)
and main_if (conds : expr list) (bodies : string list) (st: State.t) : unit =
  match conds, bodies with
  | cond::c_t, body::b_t -> print_endline ("@" ^ body ^ "@"); (match Evaluate.eval cond st |> Evaluate.if_decider with
      | true -> main st (String.split_on_char '\n' body)
      | false -> main_if c_t b_t st)
  | _, _ -> raise (SyntaxError "Conditional statements and bodies mismatched")
and main_while (cond : expr) (body : string) (while_line) (st: State.t) : unit = 
  match Evaluate.to_bool cond st with
  | true -> main st (String.split_on_char '\n' (body ^ "\n" ^ while_line ^ "\n" ^ body ^ "\n"))
  | false -> main st []

(* Execute the game engine. *)
let _ = 
  ANSITerminal.(print_string [green]   "***A Python interpreter written in Ocaml***\n");
  ANSITerminal.(print_string [cyan]    "Authors: Patrick, Zaibo, William, and Eric!\n");
  ANSITerminal.(print_string [magenta] "------------------OPython------------------\n"); 
  main empty ([])
