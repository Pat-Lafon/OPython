open Parser
open State

val evaluate : string option * Parser.expr -> State.t -> State.t

val eval : expr -> State.t -> State.value

val if_decider : State.value -> bool

val to_bool : expr -> State.t -> bool

(* Built-in stuff *)

val append : expr list -> State.t -> State.value

val length : expr list -> State.t -> State.value

val range : expr list -> State.t -> State.value

val built_in_function_names : string list

val built_in_functions : (string * (expr list -> State.t -> State.value)) list
