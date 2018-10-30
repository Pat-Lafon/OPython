open Parser
open State
(**[evaluate] takes the line input and either stores a variable or
   evaluates the given expression.*)
val evaluate : string option * Parser.expr -> State.t -> State.t

(**[eval] directly evaluates expressions.*)
val eval : expr -> State.t -> State.value

(**[if_decider] determines if a non-boolean-value is false or true.*)
val if_decider : State.value -> bool

(**[to_bool] evaluates expression to value and determines if it is true
   or false.*)
val to_bool : expr -> State.t -> bool

