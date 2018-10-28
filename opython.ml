open Main
open State
open Printf

let file_name = Sys.argv.(1)

let rec file_helper (file:in_channel) (lines : string list) = 
match Pervasives.input_line file with
| exception End_of_file -> Pervasives.close_in file; List.rev lines
| text -> file_helper file (text::lines)

let lines = file_helper (Pervasives.open_in file_name) []

let () = test lines