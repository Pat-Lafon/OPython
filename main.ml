open State
open Evaluate

(** [main ()] prompts for the game to play, then starts it. *)
let rec main st =
  print_string  ">>> ";
  match read_line () with
  | exception End_of_file -> ()
  | line -> parse_line line |> (fun x -> evaluate x st) |> main

(* Execute the game engine. *)
let () = 
  ANSITerminal.(print_string [red]
                  "OPython\n"); 
  main State.empty
