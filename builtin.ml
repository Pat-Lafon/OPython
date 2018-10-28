open Parser
open State
open Evaluate

let append (explist : expr list) (st : State.t) = 
  let vallist = List.map (fun x -> eval x st) explist in
  match vallist with
  | lst::valu::[] -> 
    (match lst, valu with
     | VList x, Function y -> failwith("functions are not values")
     | VList x, y -> List.rev(valu::List.rev(x))
     | _ -> failwith("no lists to append to")
    )
  | _ -> failwith("not enough args")


let length (lst : expr list) (st : State.t) : State.value = match lst with
  | h::[] -> begin match Evaluate.eval h st with 
      | VList(l) -> Int(List.length l)
      | _ -> raise (TypeError ("Object of that type has no len()"))
    end
  | _ -> raise (TypeError("Length takes exactly one argument"))

let rec helper_range s f i = if i = 0 then 
    raise (ValueError ("Third argument must not be zero")) else 
  if s >= f then [] else 
    Int(s) :: helper_range (s+i) f i 

let range (lst : expr list) (st : State.t) : State.value = match lst with
  | h::[] -> begin match Evaluate.eval h st with 
      | Int(a) -> VList (helper_range 0 a 1)
      | _ -> raise (TypeError ("Unsupported type for this operation"))
    end
  | h1::h2::[] -> begin match (Evaluate.eval h1 st,Evaluate.eval h2 st) with 
      | (Int(a),Int(b)) -> VList (helper_range a b 1)
      | _ -> raise (TypeError ("Unsupported type for this operation"))
    end
  | h1::h2::h3::[] -> begin 
      match (Evaluate.eval h1 st,Evaluate.eval h2 st, Evaluate.eval h3 st) with 
      | (Int(a),Int(b),Int(c)) -> VList (helper_range a b c)
      | _ -> raise (TypeError ("Unsupported type for this operation"))
    end
  | _ -> raise (TypeError("Range takes at most three arguments"))

let built_in_function_names = ["append", "length", "range"]

let built_in_functions = [("append", append), ("length", length), ("range", length)]




