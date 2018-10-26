open Parser
open State
open Evaluate

let append (exp : expr) = 
  match exp with
  | Function (s,explist) -> 
  | _ -> failwith("wrong data type")