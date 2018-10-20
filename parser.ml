open State
open Error

type op = Plus | Minus | Divide | Multiply | Modular | Exponent | Floor_Divide 
        | Equal | Not_Equal | And | Or | Not | Complement
type expr = Binary of (expr * op * expr) 
          | Unary of (op * expr) 
          | Value of State.value 
          | Variable of string
let operators = [("+", Plus);("-", Minus);("/", Divide);("*", Multiply)]
let reserved_keywords = [
  "False"; "def"; "if"; "raise"; "None"; "del"; "import"; "return"; "True";	
  "elif";	"in";	"try"; "and";	"else";	"is";	"while"; "as"; "except"; "lambda";	
  "with"; "assert";	"finally"; "nonlocal"; "yield"; "break"; "for"; "not"; 
  "class"; "from"; "or"; "continue"; "global"; "pass"]

let is_var_name (s:string) : string = 
  (* Check that the first letter is not an integer*)
  let _ = let num = Char.code s.[0] in 
    if (48 <= num && num <= 57) 
    then raise (SyntaxError "invalid syntax")
    else () in
  (* Check that every character in the string is a valid name character*)
  let _ = String.map (fun x -> let num = Char.code x in 
                       if (48 <= num && num <= 57) || (65 <= num && num <= 90) 
                          || (97 <= num && num <= 122) || (num = 95) 
                       then x 
                       else raise (SyntaxError "invalid syntax")) s in 
  (* Check that var is not a reserved keyword *)
  if List.mem s reserved_keywords 
  then raise (Error.SyntaxError "can't assign to keyword") 
  else s

let rec get_idx (str:string) (op:string) : int =
  if String.length str = 0 then -1
  else if String.sub str 0 (String.length op) = op then 0
  (* Add code here to ignore stuff in quotes and parenthesis and anything else that should be ignored *)
  (*else if  then*)
  else let acc = get_idx (String.sub str 1 (String.length str - 1)) op in 
    if acc = -1 then -1 else 1 + acc

let expr_contains (line:string) (op:string) : bool = get_idx line op <> -1

(* Will need some kind of trim function to improve upon String.trim. 
   Handle stuff like parenthesis, example: (3 + 2) -> 3 + 2*)

let is_assignment (line:string) : bool =
  let idx = get_idx line "=" in
  if idx <> -1 && not ((String.get line (idx+1)) = '=') then true
  else false

let rec 
  parse_expr_helper str op: expr = 
  let idx = get_idx str (fst op) in
  let oplen = String.length (fst op) in
  let left = String.trim (String.sub str 0 idx) in
  let right = String.trim (String.sub str (idx + oplen) (String.length str - idx - oplen)) in
  Binary(parse_expr left operators, snd op, parse_expr right operators) 
and
  parse_expr (line:string) (oplist:(string*op)list) : expr = match oplist with
  | [] -> let line = String.trim line in
    if line.[0] = '"' || line.[0] = '\'' 
    then Value(String(String.sub line 1 (String.length line-2)))
    else if int_of_string_opt line <> None then Value(Int(int_of_string line))
    else if float_of_string_opt line <> None then Value(Float(float_of_string line))
    else if bool_of_string_opt line <> None then Value(Bool(bool_of_string line))
    else Variable(line)
  | h :: t -> if expr_contains line (fst h) 
    then parse_expr_helper line h 
    else parse_expr line t

(* Will have to revisit this to deal with +=, -= %=, ect. *)
let parse_assignment (line:string) : string option * expr = 
  let eq_idx = String.index line '=' in
  let left = is_var_name (String.trim (String.sub line 0 eq_idx)) in
  let right = String.trim (String.sub line (eq_idx + 1) ((String.length line) - eq_idx - 1)) in
  (Some left, parse_expr right operators)

(* Will become some helper that raises a Syntax error if not valid
   For example, catch cases like: 'hello  *)
let valid_line line = ()

let parse_line (line : string) : string option * expr = 
  valid_line line;
  if String.length line = 0 then raise EmptyInput
  else if is_assignment line 
  then parse_assignment line
  else (None, parse_expr line operators)
