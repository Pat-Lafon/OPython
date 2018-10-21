open State
open Evaluate
open Parser
open Error

(** [main ()] prompts for the game to play, then starts it. *)
let rec main (st:State.t) : unit =
  print_string  ">>> ";
  match Parser.parse_line (read_line ()) |> (fun x -> Evaluate.evaluate x st) with
  | exception (SyntaxError x) -> print_endline ("SyntaxError: "^x); main st
  | exception (NameError x) -> print_endline ("NameError: "^x); main st
  | exception (TypeError x) -> print_endline ("TypeError: "^x); main st
  | exception (OverflowError x) -> print_endline ("OverflowError: "^x); main st
  | exception (IndentationError x) -> print_endline ("IndentationError"^x); main st
  | exception (ZeroDivisionError x)-> print_endline ("ZeroDivisionError: "^x); main st
  | exception EmptyInput -> main st
  | newst -> main(newst)

(* Execute the game engine. *)
let _ = 
  ANSITerminal.(print_string [magenta]
                  "-----------OPython-------------\n"); 
  main empty
