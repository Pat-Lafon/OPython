open State

exception ReservedVarName
exception VarNameInvalid

type op = Plus | Minus | Divide | Multiply
type expr = Binary of (expr * op * expr) | Unary of (op * expr) | Value of State.value

<<<<<<< HEAD
let reserved_keywords = [
  "False";	"def"; "if"; "raise"; "None"; "del";	"import";	"return"; "True";	
  "elif";	"in";	"try"; "and";	"else";	"is";	"while"; "as";	"except";	"lambda";	
  "with"; "assert";	"finally";	"nonlocal";	"yield"; "break";	"for"; "not"; 
  "class";	"from";	"or";	"continue";	"global";	"pass"]

let is_var_name (s:string) : unit = 
  (* Check that the first letter is not an integer*)
  let _ = let num = Char.code s.[0] in 
    if (48 <= num && num <= 57) 
    then raise VarNameInvalid 
    else () in
  (* Check that every character in the string is a valid name character*)
  let _ = String.map (fun x -> let num = Char.code x in 
                       if (48 <= num && num <= 57) || (65 <= num && num <= 90) 
                          || (97 <= num && num <= 122) || (num = 95) 
                       then x else raise VarNameInvalid) s in 
  (* Check that var is not a reserved keyword *)
  if List.mem s reserved_keywords then raise ReservedVarName else ()

let parse_line (line : string) = 
=======
>>>>>>> 539124c14996c201221356d5e689c83f34e079ec
