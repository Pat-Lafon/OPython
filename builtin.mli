open Parser
open State
open Evaluate

val append : expr list -> State.t -> State.value

val length : expr list -> State.t -> State.value

val range : expr list -> State.t -> State.value
