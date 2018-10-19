(** [main ()] prompts for the game to play, then starts it. *)
let rec main () =
  print_string  ">>> ";
  match read_line () with
  | exception End_of_file -> ()
  | line -> print_endline line; main ()

(* Execute the game engine. *)
let () = 
  ANSITerminal.(print_string [red]
                  "OPython\n"); 
  main ()
