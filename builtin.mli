open Parser
open State
open Evaluate

val append : expr list -> State.t -> State.value

val length : expr list -> State.t -> State.value

val range : expr list -> State.t -> State.value

val built_in_function_names : string list

val built_in_functions : (string * (expr list -> State.t -> State.value)) list

