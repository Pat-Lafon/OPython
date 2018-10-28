open State

type op = Plus | Minus | Divide | Floor_Divide | Multiply | Modular | Exponent 
        | Equal | Not_Equal | Greater_Than | Less_Than | Greater_Equal 
        | Less_Equal | And | Or | Not | Complement

type expr = Binary of (expr * op * expr) | Unary of (op * expr) 
          | Value of State.value | Variable of string | List of expr list 
          | Function of (string * expr list)

type line_type = Assignment | Expression | If of (expr * string) 
               | Empty | Else | Line of string | Elif of (expr * string) 
               | While of (expr * string) | Def of (string * string list * string)
               | Return of (expr) 

exception SyntaxError of string
exception TypeError of string
exception NameError of string
exception ValueError of string
exception OverflowError of string
exception IndentationError of string
exception ZeroDivisionError of string
exception EmptyInput
exception IfMultiline of (expr * string)
exception WhileMultiline of (expr * string)
exception DefMultiline of (string * string list * string)
exception ReturnExpr of expr

let operators = [[("or", Or)];
                 [("and", And);];
                 [("==", Equal);("!=", Not_Equal)];
                 [("<", Less_Than);("<=", Less_Equal);(">", Greater_Than);(">=", Greater_Equal);];
                 [("not", Not)];(* Not sure if not should be higher, can't prove yet *)
                 [("+", Plus);("-", Minus);];
                 [("%", Modular);("/", Divide);("//", Floor_Divide);("*", Multiply);];
                 [("**", Exponent);]]

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

let not_mistaken str op = 
  let oplen = String.length op in
  if String.length str = oplen then true
  else str.[oplen] <> '*' && str.[oplen] <> '=' && str.[oplen] <> '/'

let rec get_idx (str:string) (op:string) : int =
  let strlen = String.length str in
  let oplen = String.length op in 
  if strlen = 0 then -1
  else if strlen < oplen then  -1
  else if String.sub str 0 oplen = op then 
    if not_mistaken str op then 0
    else get_idx_acc str 2 op
  else if str.[0] = '(' then
    (match get_idx (String.sub str 1 (strlen - 1)) ")" with 
     | -1 -> raise (SyntaxError "Missing closing paren") 
     | x -> get_idx_acc str (x+2) op)
  else if str.[0] = ')' then raise (SyntaxError "Missing opening paren") 
  else if str.[0] = '[' then
    (match get_idx (String.sub str 1 (strlen - 1)) "]" with 
     | -1 -> raise (SyntaxError "Missing closing bracket") 
     | x -> get_idx_acc str (x+2) op)
  else if str.[0] = ']' then raise (SyntaxError "Missing opening bracket") 
  else if str.[0] = '"' then
    (match String.index (String.sub str 1 (strlen -1)) '"' with 
     | exception Not_found -> raise (SyntaxError "Missing closing quote") 
     | x -> get_idx_acc str (x+2) op)
  else if str.[0] = '\'' then
    (match String.index (String.sub str 1 (strlen -1)) '\'' with 
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

let rec split_on_char (chr:char) (line:string) : string list = 
  match get_idx line (Char.escaped chr) with
  | -1 -> line::[]
  | num -> (String.sub line 0 num)::(split_on_char chr (String.sub line (num+1) (String.length line - num -1))) 

let is_assignment (line:string) : bool =
  let idx = get_idx line "=" in
  if idx <> -1 then 
    let prev = if idx - 1 = -1 then raise (SyntaxError "invalid syntax")
      else String.get line (idx-1) in
    let next = if idx + 1 = String.length line then raise (SyntaxError "invalid syntax")
      else String.get line (idx+1) in
    prev <> '>' && prev <> '<' && prev <> '!' && next <> '='
  else false

let rec exprlst (line:string): expr list =
  if line = "" then []
  else 
    List.map (fun x -> parse_expr x operators) (split_on_char ',' line)
and parse_expr_helper (str:string) (op:string*op) : expr = 
  let idx = get_idx str (fst op) in
  let oplen = String.length (fst op) in
  let left = String.sub str 0 idx in
  let right = String.sub str (idx + oplen) (String.length str - idx - oplen) in
  if trim left = "" then Unary (snd op, parse_expr right operators) 
  else Binary(parse_expr left operators, snd op, parse_expr right operators) 
and
  parse_expr (line:string) (oplist:(string*op) list list) : expr = 
  let line = trim line in let args = get_idx line "(" in let fstarg =  get_idx line "." in 
  match oplist with
  | [] -> 
    if line.[0] = '"' || line.[0] = '\'' 
    then Value(String(String.sub line 1 (String.length line-2)))
    else if line.[0] = '[' && line.[String.length line-1] = ']'
    then if String.length line = 2 then List([])
      else List(List.map (fun x -> parse_expr x operators) 
                  (split_on_char ',' (String.sub line 1 (String.length line - 2))))
    else if int_of_string_opt line <> None then Value(Int(int_of_string line))
    else if float_of_string_opt line <> None then Value(Float(float_of_string line))
    else if "True" = line || "False" = line then Value(Bool(bool_of_string (String.lowercase_ascii line)))
    else if args <> -1 && fstarg <> -1 
    then Function(String.sub line (fstarg+1) (args-fstarg-1), 
                  exprlst(String.sub line 0 (fstarg) ^","^ String.sub line (args+1) (String.length line - args - 2)))
    else if args <> -1 
    then Function(String.sub line 0 (args), exprlst (String.sub line (args+1) (String.length line - (args + 2))))
    else if line.[String.length line -1] = ']' then let args = get_idx line "[" in
      Function("splice", exprlst (String.sub line (args+1) (String.length line - (args + 2))))
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

(** Matches if statement *)
let if_regex = Str.regexp "^if \\(.*\\):\\(.*\\)"
let elif_regex = Str.regexp "^elif \\(.*\\):\\(.*\\)"
let else_regex = Str.regexp "^else *: *"
let while_regex = Str.regexp "^while \\(.*\\):\\(.*\\)"
let def_regex = Str.regexp "^def \\(.*\\)(\\(.*\\)) *:\\(.*\\)$"
let return_regex = Str.regexp "^return \\(.*\\)"

(** Check if line is an if statement *)
let is_if line = Str.string_match if_regex line 0
let is_else line = Str.string_match else_regex line 0
let is_elif line = Str.string_match elif_regex line 0
let is_while line = Str.string_match while_regex line 0
let is_def line = Str.string_match def_regex line 0
let is_return line = Str.string_match return_regex line 0

let parse_if (line: string) : (expr * string) =
  let condition = Str.matched_group 1 line in
  let body = String.trim (Str.matched_group 2 line) in
  (parse_expr condition operators, body)

let parse_return (line: string) : (expr) =
  let return_expr = Str.matched_group 1 line in
  (parse_expr return_expr operators)

let parse_def (line: string) : (string * string list * string) =
  let fn_name = Str.matched_group 1 line in
  let args = List.map String.trim (String.split_on_char ',' (Str.matched_group 2 line)) in
  let body = String.trim (Str.matched_group 3 line) in
  (fn_name, args, body)

let line_type (line : string) : line_type =
  if String.length line = 0 then Empty
  else if is_assignment line then Assignment
  else if is_if line then If (parse_if line)
  (* calling parse_if is not a typo *)
  else if is_elif line then Elif (parse_if line)
  else if is_else line then Else
  else if is_while line then While (parse_if line)
  else if is_def line then Def (parse_def line)
  else if is_return line then Return (parse_return line)
  else Expression

let parse_line (line : string) : string option * expr = 
  match line_type line with
  | Empty -> raise EmptyInput
  | Assignment -> parse_assignment line
  | Expression -> (None, parse_expr line operators)
  | If (cond, body) -> raise (IfMultiline (cond, body))
  | Def (name, args, body) -> raise (DefMultiline (name, args, body))
  | Elif (cond, body) -> raise (SyntaxError "Elif statement with no if")
  | Else -> raise (SyntaxError "Else statement with no if")
  (* line type is helpful for later *)
  | Line l -> (None, parse_expr line operators)
  | While (cond, body) -> raise (WhileMultiline (cond, body)) 
  | Return (expr) -> raise (ReturnExpr (expr))

let parse_multiline (line: string) : line_type =
  match line_type line with
  | Empty -> Empty
  | Assignment -> Line line
  | Expression -> Line line
  | Line line -> Line line
  | Return (expr) -> Line line
  | If (cond, body) -> If (cond, body)
  | Elif (cond, body) -> Elif (cond, body)
  | While (cond, body) -> While (cond, body)
  | Def (name, args, body) -> Def (name, args, body)
  | Else -> Else 
