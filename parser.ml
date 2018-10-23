open State

type op = Plus | Minus | Divide | Floor_Divide | Multiply | Modular | Exponent 
        | Equal | Not_Equal | And | Or | Not | Complement

type line_type = Assignment | Expression | If | Empty
(* type line_type = Assignment | Expression | If | While | Elif | Else | Empty *)

type expr = Binary of (expr * op * expr) | Unary of (op * expr) | Value of State.value | Variable of string | List of expr list

exception SyntaxError of string
exception TypeError of string
exception NameError of string
exception OverflowError of string
exception IndentationError of string
exception ZeroDivisionError of string
exception EmptyInput
exception Multiline of (expr * string)

let operators = [[("==", Equal);("!=", Not_Equal);("and", And);("or", Or)];
                 [("+", Plus);("-", Minus);];
                 [("%", Modular);];
                 [("/", Divide);("//", Floor_Divide);("*", Multiply);];
                 [("**", Exponent);];
                 [("not", Not)]]

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
  then raise (SyntaxError "can't assign to keyword") 
  else s

let rec get_idx (str:string) (op:string) : int =
  if String.length str = 0 then -1
  else if String.length str < String.length op then -1
  else if String.sub str 0 (String.length op) = op then 0
  else if str.[0] = '(' then
    (match get_idx (String.sub str 1 (String.length str - 1)) ")" with 
     | -1 -> raise (SyntaxError "Missing closing paren") 
     | x -> get_idx_acc str (x+2) op)
  else if str.[0] = ')' then raise (SyntaxError "Missing opening paren") 
  else if str.[0] = '[' then
    (match get_idx (String.sub str 1 (String.length str - 1)) "]" with 
     | -1 -> raise (SyntaxError "Missing closing bracket") 
     | x -> get_idx_acc str (x+2) op)
  else if str.[0] = ']' then raise (SyntaxError "Missing opening bracket") 
  else if str.[0] = '"' then
    (match String.index (String.sub str 1 (String.length str -1)) '"' with 
     | exception Not_found -> raise (SyntaxError "Missing closing quote") 
     | x -> get_idx_acc str (x+2) op)
  else if str.[0] = '\'' then
    (match String.index (String.sub str 1 (String.length str -1)) '\'' with 
     | exception Not_found -> raise (SyntaxError "Missing closing quote") 
     | x -> get_idx_acc str (x+2) op)
  else get_idx_acc str 1 op
and 
  get_idx_acc (str:string) (num:int) (op:string) : int = 
  let acc = get_idx (String.sub str num (String.length str - num)) op in 
  if acc = -1 then -1 else num + acc

let rec expr_contains (line:string) (op:(string*op) list) : (string*op) option * int = 
  match op with
  | [] -> None, max_int
  | h :: t -> let next = expr_contains line t in 
    let current = get_idx line (fst h) in
    if current <> -1 && current < snd next then Some h, current else next

let valid_paren str = match get_idx str ")" with 
  | exception (SyntaxError x) -> false
  | x -> if x = -1 then true else false

let rec trim str : string =
  let newstr = String.trim str in
  if newstr <> str then trim newstr
  else if String.length newstr = 0 then str
  else if str.[0] = '(' && str.[String.length str - 1] = ')' then 
    if valid_paren (String.sub str 1 (String.length str - 2))
    then trim (String.sub str 1 (String.length str - 2))
    else str
  else str

let is_assignment (line:string) : bool =
  let idx = get_idx line "=" in
  if idx <> -1 then 
    let prev = String.get line (idx-1) in
    let next = String.get line (idx+1) in
    prev <> '>' && prev <> '<' && prev <> '!' && next <> '='
  else false

let rec parse_expr_helper (str:string) (op:string*op) : expr = 
  let idx = get_idx str (fst op) in
  let oplen = String.length (fst op) in
  let left = String.sub str 0 idx in
  let right = String.sub str (idx + oplen) (String.length str - idx - oplen) in
  if trim left = "" then Unary (snd op, parse_expr right operators) 
  else Binary(parse_expr left operators, snd op, parse_expr right operators) 
and
  parse_expr (line:string) (oplist:(string*op) list list) : expr = 
  let line = trim line in
  match oplist with
  | [] -> 
    if line.[0] = '"' || line.[0] = '\'' 
    then Value(String(String.sub line 1 (String.length line-2)))
    else if line.[0] = '[' || line.[String.length line -1] = ']' 
    then if String.length line = 2 then List([])
      else List(List.map (fun x -> parse_expr x operators) 
                  (String.split_on_char ',' (String.sub line 1 (String.length line - 2))))
    else if int_of_string_opt line <> None then Value(Int(int_of_string line))
    else if float_of_string_opt line <> None then Value(Float(float_of_string line))
    else if "True" = line || "False" = line then Value(Bool(bool_of_string (String.lowercase_ascii line)))
    else Variable(line)
  | h :: t -> match expr_contains line h with
    | Some x, _ -> parse_expr_helper line x
    | None, _ -> parse_expr line t

(* Will have to revisit this to deal with +=, -= %=, ect. *)
let parse_assignment (line:string) : string option * expr = 
  let eq_idx = String.index line '=' in
  let left = is_var_name (String.trim (String.sub line 0 eq_idx)) in
  let right = trim (String.sub line (eq_idx + 1) ((String.length line) - eq_idx - 1)) in
  (Some left, parse_expr right operators)

(** [paren_check str idx acc] returns true if parentheses are valid and false otherwise *)
let rec paren_check (str: string) idx acc =
  if idx = String.length str then acc = 0 
  else if acc < 0 then false 
  else if String.get str idx = '(' then paren_check str (idx+1) (acc+1)
  else if String.get str idx = ')' then paren_check str (idx+1) (acc-1)
  else if String.get str idx = '"' then paren_check str (idx+2+(get_idx (String.sub str (idx+1) (String.length str -idx-1)) "\"")) acc
  else paren_check str (idx+1) acc

(* Will become some helper that raises a Syntax error if not valid
   For example, catch cases like: 'hello  *)
let valid_line line = 
  if not (paren_check line 0 0) then raise (SyntaxError "Invalid parenthesis")
  else ()

(** Matches if statement *)
let if_regex = Str.regexp "^if \(.*\):\(.*\)"

(** Check if line is an if statement *)
let is_if line = Str.string_match if_regex line 0

let line_type (line : string) : line_type =
  if String.length line = 0 then Empty
  else if is_assignment line then Assignment
  else if is_if line then If
  else Expression

let parse_if (line: string) : (expr * string) =
  let condition = Str.matched_group 1 line in
  let body = String.trim (Str.matched_group 2 line) in
  (parse_expr condition operators, body)

let parse_line (line : string) : string option * expr = 
  (*valid_line line; Testing whether get_idx is working, report to Patrick if failure*)
  match line_type line with
  | Empty -> raise EmptyInput
  | Assignment -> parse_assignment line
  | Expression -> (None, parse_expr line operators)
  | If -> raise (Multiline (parse_if line))

let parse_multiline (line: string) =
  match line_type line with
  | Empty -> raise EmptyInput
  | _ -> line
