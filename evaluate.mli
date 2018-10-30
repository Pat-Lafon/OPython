open Parser
open State

(** [evaluate input st] returns the new state with the expression part of 
    input evaluated to a value and added to the state if input has a string. 
    Otherwise, it prints out the value of the expression part of input **)
val evaluate : string option * Parser.expr -> State.t -> State.t

val eval : expr -> State.t -> State.value

val if_decider : State.value -> bool

val to_bool : expr -> State.t -> bool

(* Built-in stuff *)

val append : expr list -> State.t -> State.value

val len : expr list -> State.t -> State.value

val range : expr list -> State.t -> State.value

val built_in_function_names : string list

val built_in_functions : (string * (expr list -> State.t -> State.value)) list
