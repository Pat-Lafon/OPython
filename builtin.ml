open State
open Parser
open Error
open Arithmetic

(**[to_string] returns the string of a value*)
let rec to_string (value:State.value) : string = 
  match value with
  | VList x -> List.fold_left (fun x y -> x^(to_string y)^", ") "[" !x |> 
               (fun x -> if String.length x = 1 then x ^ "]" 
                 else String.sub x 0 (String.length x -2) ^ "]")
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Bool x -> string_of_bool x |> String.capitalize_ascii
  | Function f -> 
    let (name, args, body) = f in
    let address = 2*(Obj.magic (ref f)) in
    "<function " ^ name ^ " at " ^ Printf.sprintf "0x%08x" address ^ ">"
  | String x -> "'" ^ x ^ "'"
  | NoneVal -> "None"

(** [index lst] returns the index of the second element of lst in the first 
    element of lst in an integer value, returns Int(-1) if not found.
    The first element of lst must be either a VList or a String **)
let index (lst : value list): State.value  = let func = function
    | Int x, Int y -> (x = y)
    | Int x, Float y -> (float_of_int x = y)
    | Int x, Bool y -> if y then (x=1) else (x=0)
    | Float x, Int y -> (x = float_of_int y)
    | Float x, Float y -> (x = y)
    | Float x, Bool y -> if y then (x=1.0) else (x=0.0)
    | Bool x, Int y -> if x then (y=1) else (y=0)
    | Bool x, Float y -> if x then (y=1.0) else (y=0.0)
    | Bool x, Bool y -> (x = y)
    | String x, _ | _, String x -> false
    | VList x, _ | _, VList x -> false
    | Function (name1, args1, body1), Function (name2, args2, body2) -> 
      (name1 = name2)
    | _, _ -> false
  in let idx value l = List.find (fun x -> func (x,value)) l 
  in match lst with
  | VList(t)::h::[] -> idx h !t
  | String(s)::String(s1)::[] -> 
    let rec search sub str = if (String.length sub > String.length str) then 
        String.length sub else 
      if String.sub str 0 (String.length sub) = sub then
        0 else (1 + search sub (String.sub str 1 (String.length sub)))
    in if (search s1 s >= String.length s) then Int(-1) else Int(search s1 s)
  | _ -> raise (TypeError ("Operation not supported"))

let rec splice_string (item:string) start stop step = 
  if start >= stop then ""
  else if step > 0 then Char.escaped (String.get item start) 
                        ^ splice_string item (start+step) stop step
  else Char.escaped (String.get item (stop-1)) 
       ^ splice_string item start (stop+step) step

let rec splice_list (item: value list) start stop step =
  if start >= stop then []
  else if step > 0 then List.nth item start
                        :: splice_list item (start+step) stop step
  else List.nth item (stop-1) :: splice_list item start (stop+step) step

(** The splice function as in Python; the first element of lst is the 
    string/list to splice, the optional second to fourth elements of lst consist 
    of the start, end, and stepping of the index **)
let splice (lst : value list) : State.value = 
  let item, start, stop, step = match lst with
    | VList a1 :: a2 :: a3 :: a4 :: [] -> 
      let a4 = if a4 = Int 0 then raise (ValueError "Third argument must not be zero") 
        else if a4 = String "" then Int 1 else a4 in 
      let a3 = if a3 = String "" then Int (List.length !a1) else a3 in
      let a2 = if a2 = String "" then Int 0 else a2
      in VList a1, a2, a3, a4
    | String a1 :: a2 :: a3 :: a4 :: [] -> 
      let a4 = if a4 = Int 0 then raise (ValueError "Third argument must not be zero") 
        else if a4 = String "" then Int 1 else a4 in 
      let a3 = if a3 = String "" then Int (String.length a1) else a3 in
      let a2 = if a2 = String "" then Int 0 else a2
      in String a1, a2, a3, a4
    | VList a1 :: a2 :: a3 :: [] ->       
      let a4 =  Int 1 in 
      let a3 = if a3 = String "" then Int (List.length !a1) else a3 in
      let a2 = if a2 = String "" then Int 0 else a2
      in VList a1, a2, a3, a4
    | String a1 :: a2 :: a3 :: [] ->       
      let a4 =  Int 1 in 
      let a3 = if a3 = String "" then Int (String.length a1) else a3 in
      let a2 = if a2 = String "" then Int 0 else a2
      in String a1, a2, a3, a4
    | VList a1 :: a2 :: [] -> 
      let length = List.length !a1 in
      let idx = (match a2 with 
          | Int x -> if x > length || x < -length 
            then raise (IndexError ("list index out of range"))
            else (x+length) mod length
          | _ -> raise (SyntaxError "invalid syntax")) in
      let a1 = ref(List.nth !a1 idx::[])
      in VList a1, NoneVal, NoneVal, NoneVal
    | String a1 :: a2 :: [] ->
      let length = String.length a1 in
      let idx = (match a2 with 
          | Int x -> if x > length || x < -length 
            then raise (IndexError ("list index out of range"))
            else (x+String.length a1) mod (String.length a1)
          | _ -> raise (SyntaxError "invalid syntax")) in
      let a1 = Char.escaped (String.get a1 idx) in
      String a1, NoneVal, NoneVal, NoneVal
    | a1 :: [] -> raise (SyntaxError "invalid syntax")
    | a1 -> raise (SyntaxError "invalid syntax") in
  match item, start, stop, step with 
  | VList a1, NoneVal, NoneVal, NoneVal -> 
    (match !a1 with 
     | a::[] -> a
     | _ -> failwith "something above messed up")
  | String a1, NoneVal, NoneVal, NoneVal -> String a1
  | VList a1, Int a2, Int a3, Int a4 -> 
    let length = List.length !a1 in
    let a2, a3 = 
      if a2 > length || a2 < -length || a3 > length || a3 < -length
      then raise (IndexError "list index out of range")
      else (a2+length) mod length, (a3+length+1) mod (length+1) in
    VList(ref(splice_list !a1 a2 a3 a4))
  | String a1, Int a2, Int a3, Int a4 -> 
    let length = String.length a1 in
    let a2, a3 = 
      if a2 > length || a2 < -length || a3 > length || a3 < -length
      then raise (IndexError "list index out of range")
      else (a2+length) mod length, (a3+length+1) mod (length+1) in
    String(splice_string a1 a2 a3 a4)
  | _ -> failwith "Something went wrong in splice evaluation"

(** [append lst] appends the second element of lst into the first element, 
    which must either be a list**)
let append (val_list : value list)= 
  match val_list with
  | lst::value::[] -> 
    (match lst with
     | VList x -> x := !x@value::[]; NoneVal
     | _ -> failwith("not a list")
    )
  | _ -> raise (TypeError("requires two arguments"))


let print (val_list : value list) =
  print_endline(List.fold_left (fun acc value -> acc^(to_string value)) "" 
                  val_list);
  NoneVal 

let if_decider = function
  | Int(0) -> false
  | String("") -> false
  | Bool(false) -> false
  | NoneVal -> false
  | Float(0.0)  -> false
  | VList(a) -> if !a = [] then false else true
  | _ -> true

let rec assertt (val_list : value list) =
  match val_list with
  | [] -> NoneVal 
  | h::t -> if if_decider h then assertt t else raise AssertionError

let len (lst : value list) : State.value = match lst with
  | VList(l)::[] -> Int(List.length !l)
  | String(s)::[] -> Int (String.length s)
  | _::[] -> raise (TypeError ("Object of that type has no len()"))
  | _ -> raise (TypeError("Length takes exactly one argument"))

let rec helper_range s f i = 
  if i = 0 then raise (ValueError "range() arg 3 must not be zero") 
  else if i > 0 then (if s >= f then [] 
                      else Int(s) :: helper_range (s+i) f i) 
  else (if s <= f then [] else Int(s) :: helper_range (s+i) f i)


let range (lst : value list) : State.value = 
  match lst with
  | Int(a)::[] -> VList (ref(helper_range 0 a 1))
  | _::[] -> raise (TypeError ("Unsupported type for this operation"))

  | Int(a)::Int(b)::[] -> VList (ref(helper_range a b 1))
  | _ ::_::[]-> raise (TypeError ("Unsupported type for this operation"))

  | Int(a)::Int(b)::Int(c)::[] -> VList (ref(helper_range a b c))
  | _::_::_::[]-> raise (TypeError ("Unsupported type for this operation"))

  | _ -> raise (TypeError("Range takes at most three arguments"))

let dictionary (lst : value list) : State.value = 
  let hashtbl = Hashtbl.create (List.length lst) in
  let rec helper (assoc_lst : value list) : (value,value) Hashtbl.t = 
    begin match assoc_lst with
      | [] -> Hashtbl.copy hashtbl
      | h1::h2::t -> Hashtbl.add hashtbl h1 h2; helper t
      | _ -> raise (TypeError("Operation unsupported"))
    end 
  in Hash(helper lst)

let put (lst : value list) : State.value =
  match lst with
  | Hash(h)::key::value::[] -> Hashtbl.remove h key; 
    Hashtbl.add h key value; Hash(h)
  | _ -> raise (TypeError("Operation unsupported"))

let get (lst : value list) : State.value =
  match lst with
  | Hash(h)::key::[] -> Hashtbl.find h key
  | _ -> raise (TypeError("Operation unsupported"))

(** Type casts *)
let chr (val_list : value list) =
  match val_list with
  | Int(x)::[] ->if (x>=0) && (x<=1114111) 
    then String(Char.escaped((Uchar.to_char(Uchar.of_int(x))))) 
    else raise (ValueError("chr() arg is not in range"))
  | _ ::[]-> raise (TypeError("an integer is required"))
  | _ -> raise (TypeError("chr() takes exactly 1 argument"))

let bool (val_list: value list) = 
  match val_list with
  | Bool x::[] -> Bool x
  | Int x::[] ->  Bool (x<>0)
  | Float x::[] -> Bool (x <> 0.)
  | String x::[] -> Bool (x<>"")
  | VList x::[] -> Bool (!x <> [])
  | Function x::[] -> Bool true
  | NoneVal :: [] -> Bool false
  | [] -> Bool false
  | _ -> raise (TypeError("bool() takes at most 1 argument"))

let float (val_list: value list) =
  match val_list with
  | Int x::[] -> Float(float_of_int(x))
  | Float x::[] -> Float x
  | String x::[] -> if float_of_string_opt(x) <> None 
    then Float(float_of_string(x)) 
    else raise (ValueError "could not convert input to float")
  | _::[] -> raise (TypeError("float() argument must be a string or a number"))
  | [] -> Float(0.0)
  | _ -> raise (TypeError("float() takes at most 1 argument"))

let int (val_list: value list) = 
  match val_list with
  | Int(x)::[] -> Int(x)
  | Float(x)::[] -> Int(int_of_float(x))
  | String(x)::[] -> if int_of_string_opt(x) <> None then Int(int_of_string(x)) 
    else raise (ValueError("could not convert input to int"))
  | Bool x ::[]-> if x then Int 1 else Int 0
  | [] -> Int(0)
  | _ -> raise (TypeError "int() can't convert more than one argument")

let rec list (v : value list) = match v with
  | [] -> VList(ref[])
  | VList(l)::[]-> VList(l)
  | String(s)::[] -> let rec help_list str = if str = "" then [] else 
                       if String.length str = 1 then [String(str)] else 
                         String(String.sub str 0 1) :: (help_list (String.sub str 1 (String.length str - 1)))
    in VList(ref(help_list s))
  | _ :: [] -> raise (TypeError ("Input type is not iterable"))
  | x -> raise (TypeError ("list() takes at most 1 argument (" 
                           ^ string_of_int (List.length x) ^ " given)"))

let rec to_list (lst : value list) = 
  let vlist = list lst in
  match vlist with
  | VList(l) -> !l
  | _ -> raise (TypeError ("Input type is not iterable"))

(** Quit in actual python can take an arg, it ignores it.*)
let quit arg = exit 0

let rec replace (v : value list) = match v with
  | VList(l):: Int(idx):: x :: []-> let
    rec replace_help l idx x = begin match l with 
      | [] -> []
      | h::t -> if idx = 0 then x :: t else h :: replace_help t (idx-1) x 
    end
    in VList(ref (replace_help !l idx x))
  | _ -> raise (TypeError (""))

let match_bool = function
  |Bool x -> x|_->failwith("not possble")

(**List Functions*)
let max (v:value list) = match v with
  | VList l :: []-> let get_first l = begin match l with
      | h::_ -> h 
      | [] -> NoneVal
    end in let
      rec max_help l acc = begin match l with
      | [] -> acc
      | h::t -> if h >= acc then max_help t h else max_help t acc
    end 
    in let frst =  get_first(!l)
    in (max_help !l frst)
  | x -> let get_first l = begin match l with
      | f::_ -> f
      | [] -> NoneVal
    end in let rec max_assist x acc = begin match x with
      | [] -> acc
      | h::t -> if match_bool(helper_greater_equal (h,acc)) then max_assist t h else max_assist t acc
    end in let frst =  get_first(x) in (max_assist x frst)

let min (v:value list) = match v with
  | VList l :: []-> let get_first l = begin match l with
      | h::_ -> h 
      | [] -> NoneVal
    end in let
      rec min_help l acc = begin match l with
      | [] -> acc
      | h::t -> if h <= acc then min_help t h else min_help t acc
    end 
    in let frst =  get_first(!l)
    in (min_help !l frst)
  | x -> let get_first l = begin match l with
      | f::_ -> f
      | [] -> NoneVal
    end in let rec max_assist x acc = begin match x with
      | [] -> acc
      | h::t -> if match_bool(helper_less_equal (h,acc)) then max_assist t h else max_assist t acc
    end in let frst =  get_first(x) in (max_assist x frst)

let built_in_functions = [("append", append); ("len", len); ("print", print); 
                          ("chr", chr); ("bool", bool); ("float", float); 
                          ("int",int); ("range", range); ("splice", splice); 
                          ("index", index); ("assert", assertt); ("list", list);
                          ("put", put); ("get", get); 
                          ("dictionary", dictionary); ("replace", replace);
                          ("max", max); ("min", min)]
