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



let rec helper_range s f i = match
    let range (lst : expr list) (st : State.t) : State.value = match lst with
      | h::[] -> begin match Evaluate.eval h st with 
          | Int(l) -> VList (helper_range 0 l 1)
          | _ -> raise (TypeError ("Object of that type has no len()"))
        end] 
