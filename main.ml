open State
open Evaluate
open Parser

let rec multiline (conds : expr list) (bodies : string list) (acc : string) =
  print_string "... ";
  match parse_multiline (read_line ()) with
  | Empty -> (List.rev (Value(Bool(true))::conds), List.rev (""::(String.trim acc::bodies)))
  | Line line -> multiline conds bodies (acc ^ "\n" ^ line)
  | If (cond, body) -> multiline (cond::conds) (String.trim acc::bodies) body
  | Elif (cond, body) -> multiline (cond::conds) (String.trim acc::bodies) body
  | Else -> multiline (Value(Bool(true))::conds) (String.trim acc::bodies) ""
  | _ -> raise EmptyInput

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
    | exception (Multiline (cond, body)) -> 
      let (conds, bodies) = multiline [cond] [] body in main_if conds bodies st
    | newst -> main newst t)
and main_if (conds : expr list) (bodies : string list) (st: State.t) : unit =
  match conds, bodies with
  | [], [] -> print_endline "wtf3?"; raise Not_found
  | [], _ -> print_endline "wtf?2"; raise Not_found 
  | _, [] -> print_endline "wtf?1"; raise Not_found
  | cond::c_t, body::b_t -> print_endline ("@" ^ body ^ "@"); (match Evaluate.eval cond st |> Evaluate.if_decider with
      | true -> main st (String.split_on_char '\n' body)
      | false -> main_if c_t b_t st)

(* Execute the game engine. *)
let _ = 
  ANSITerminal.(print_string [green]   "***A Python interpreter written in Ocaml***\n");
  ANSITerminal.(print_string [cyan]    "Authors: Patrick, Zaibo, William, and Eric!\n");
  ANSITerminal.(print_string [magenta] "------------------OPython------------------\n"); 
  main empty ([])
