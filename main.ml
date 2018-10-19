open State
open Evaluate
open Parser

(** [main ()] prompts for the game to play, then starts it. *)
let rec main st =
  print_string  ">>> ";
  match read_line () with
  | exception End_of_file -> st
  | line -> main (Parser.parse_line line |> (fun x -> Evaluate.evaluate x st))

(* Execute the game engine. *)
let _ = 
  ANSITerminal.(print_string [red]
                  "OPython\n"); 
  main empty
