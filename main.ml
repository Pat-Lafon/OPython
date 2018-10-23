open State
open Evaluate
open Parser

let rec multiline cond acc =
  print_string "...";
  match parse_multiline (read_line ()) with
  | exception EmptyInput -> acc
  | line -> multiline cond line ^ "\n" ^ acc

(** [main ()] prompts for the game to play, then starts it. *)
let rec main (st:State.t) (foo: bool) : unit =
  if foo then print_string "... " else print_string ">>> ";
  match Parser.parse_line (read_line ()) |> (fun x -> Evaluate.evaluate x st) with
  | exception (SyntaxError x) -> print_endline ("SyntaxError: "^x); main st false
  | exception (NameError x) -> print_endline ("NameError: "^x); main st false
  | exception (TypeError x) -> print_endline ("TypeError: "^x); main st false
  | exception (OverflowError x) -> print_endline ("OverflowError: "^x); main st false
  | exception (IndentationError x) -> print_endline ("IndentationError"^x); main st false
  | exception (ZeroDivisionError x)-> print_endline ("ZeroDivisionError: "^x); main st false
  | exception EmptyInput -> main st false
  | exception (Multiline cond) -> let a = print_endline (multiline cond "") in main st false
  | newst -> main newst false

(* Execute the game engine. *)
let _ = 
  ANSITerminal.(print_string [green]   "***A Python interpreter written in Ocaml***\n");
  ANSITerminal.(print_string [cyan]    "Authors: Patrick, Zaibo, William, and Eric!\n");
  ANSITerminal.(print_string [magenta] "------------------OPython------------------\n"); 
  main empty false
