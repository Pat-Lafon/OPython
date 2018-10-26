open Parser
open State
open Evaluate

let append (explist : expr list) (st : State.t) = 
  let vallist = List.map (fun x -> eval x st) explist in
  match vallist with
  | lst::valu::[] -> 
    (match lst with
     | VList x -> List.rev(valu::List.rev(x))
     | _ -> failwith("not a list")
    )
  | _ -> failwith("not enough args")

