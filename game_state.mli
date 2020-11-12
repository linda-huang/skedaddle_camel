type state = Welcome | GameOver | Won | InPlay

type game_state =  {
  score : Scorer.t;
  current_state : state;
  round_state : Round_state.t
}

(** [new_level gs] is a new game_state with the appropriate
    next round_state level and updated score. *)
val new_level : game_state -> game_state 

(** [update_game_state gs st] is an updated game_state with 
    round_state [st] and score updated appropriately, given how 
    many enemies were shot since the last update *)
val update_game_state : game_state -> Round_state.t -> game_state 

(** [init st] is a fresh game_state with round_state [st], 
    empty score, and Welcome current_state. *)
val init : Round_state.t -> game_state

(** [string_of_game_state gs] is [gs], 
    nicely formatted in a string*)
val string_of_game_state : game_state -> string 