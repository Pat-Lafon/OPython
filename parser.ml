open State

exception ReservedVarName
exception VarNameInvalid

type op = Plus | Minus | Divide | Multiply
type expr = Binary of (expr * op * expr) 
          | Unary of (op * expr) 
          | Value of State.value 
          | Variable of string
let operators = "+-/*"

let reserved_keywords = [
  "False"; "def"; "if"; "raise"; "None"; "del"; "import"; "return"; "True";	
  "elif";	"in";	"try"; "and";	"else";	"is";	"while"; "as"; "except"; "lambda";	
  "with"; "assert";	"finally"; "nonlocal"; "yield"; "break"; "for"; "not"; 
  "class"; "from"; "or"; "continue"; "global"; "pass"]

let is_var_name (s:string) : string = 
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
  if List.mem s reserved_keywords then raise ReservedVarName else s

let get_idx str char =
  try Some (String.index str char) with
  | Not_found -> None

let is_assignment line =
  match get_idx line '=' with
  | Some idx -> not ((String.get line (idx+1)) = '=')
  | None -> false

(* let digits s = Str.string_match (Str.regexp "[0-9]+$") s 0 *)

let digits = String.map (fun x -> let num = Char.code x in 
                          if (48 <= num && num <= 57)
                          then x else raise Not_found) 
let rec digits s idx =
  if String.length s = idx then true else 
    let code = (Char.code (String.get s idx)) in 48 <= code && code <= 57 && digits s (idx + 1)

let rec parse_expr_helper str : expr = 
  match get_idx str '+' with
  | None -> if digits str 0
    then Value(Int(int_of_string (String.trim str)))
    else Variable(str)
  | Some idx -> 
    let left = String.trim (String.sub str 0 idx) in
    let right = String.trim (String.sub str (idx + 1) ((String.length str) - idx - 1)) in
    Binary(parse_expr_helper left, Plus, parse_expr_helper right)

let parse_expr line = parse_expr_helper line

let parse_assignment line = 
  let eq_idx = String.index line '=' in
  let left = is_var_name (String.trim (String.sub line 0 eq_idx)) in
  let right = String.trim (String.sub line (eq_idx + 1) ((String.length line) - eq_idx - 1)) in
  (Some left, parse_expr right)

let parse_line (line : string) = 
  if is_assignment line 
  then parse_assignment line
  else (None, parse_expr line)
