open State
open Evaluate
open Parser

let rec multiline acc =
  print_string "...";
  match parse_multiline (read_line ()) with
  | exception EmptyInput -> String.trim acc
  | line -> multiline (acc ^ "\n" ^ line)

let rec print_lines = function
  | [] -> ()
  (* | h::t -> print_endline h *)
  | h::t -> print_endline h; print_lines t

let rec main (st:State.t) (lines: string list) : unit =
  (* print_lines lines; *)
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
    | exception (Multiline (cond, init_body)) -> 
      let body = multiline init_body in 
      (match Evaluate.eval cond st |> Evaluate.if_decider with
      | true -> main st (String.split_on_char '\n' body)
      | false -> main st [])
    | newst -> main newst t)

(* Execute the game engine. *)
let _ = 
  ANSITerminal.(print_string [green]   "***A Python interpreter written in Ocaml***\n");
  ANSITerminal.(print_string [cyan]    "Authors: Patrick, Zaibo, William, and Eric!\n");
  ANSITerminal.(print_string [magenta] "------------------OPython------------------\n"); 
  main empty ([])
