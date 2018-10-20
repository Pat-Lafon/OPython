open State
open Evaluate
open Parser
open Error

(** [main ()] prompts for the game to play, then starts it. *)
let rec main st =
  print_string  ">>> ";
  match Parser.parse_line (read_line ()) |> (fun x -> Evaluate.evaluate x st) with
  | exception End_of_file -> main st
  | exception (SyntaxError x) -> print_endline ("SyntaxError: "^x); main st
  | exception (NameError x) -> print_endline ("NameError: "^x); main st
  | exception (TypeError x) -> print_endline ("TypeError: "^x); main st
  | newst -> main(newst)

(* Execute the game engine. *)
let _ = 
  ANSITerminal.(print_string [red]
                  "-----------OPython-------------\n"); 
  main empty
