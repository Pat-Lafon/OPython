open Main
open State
open Printf

let args = Array.length Sys.argv

let _ = 
  if args = 1 then 
    (ANSITerminal.(print_string [green]   "***A Python interpreter written in Ocaml***\n");
    ANSITerminal.(print_string [cyan]    "Authors: Patrick, Zaibo, William, and Eric!\n");
    ANSITerminal.(print_string [magenta] "------------------OPython------------------\n"); 
    interpret State.empty [] true)
  else
    let file_name = Sys.argv.(1) in
    let rec file_helper (file:in_channel) (lines : string list) = 
      (match Pervasives.input_line file with
      | exception End_of_file -> Pervasives.close_in file; List.rev lines
      | "" -> file_helper file lines
      | text -> if String.get (String.trim text) 0 = '#' then file_helper file lines 
                else file_helper file (text::lines)) in
    let lines = file_helper (Pervasives.open_in file_name) [] in
    interpret State.empty lines false