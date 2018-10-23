open Parser
open State

val evaluate : string option * Parser.expr -> State.t -> State.t

val eval : expr -> State.t -> State.value

val if_decider : State.value -> bool