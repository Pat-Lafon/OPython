open Parser
open State

val evaluate : string option * Parser.expr -> State.t -> State.t