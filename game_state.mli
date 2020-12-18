(** Interactions across levels of the game *)

(** The variant type of how a game can end, if not won *)
type game_end = Time | Health 

(** The variant type of the game state *)
type state = 
  | Welcome 
  | GameOver of game_end 
  | Won 
  | InPlay
  | Transition of int 
  | Instructions of float  

(** The variant type of difficulty *)
type difficulty = Easy | Hard 

(** The type of a game *)
type game_state = {
  score : Scorer.t;
  current_state : state;
  round_state : Round_state.t;
  game_difficulty : difficulty;
}

(** [int_of_difficulty diff] is the number corresponding 
    to [diff] chosen by the player *)
val int_of_difficulty : difficulty -> int 

(** [new_level gs] is a new game_state with the appropriate
    next round_state level and updated score. *)
val new_level : game_state -> game_state 

(** [update_game_state gs st] is an updated game_state with 
    round_state [st] and score updated appropriately, given how 
    many enemies were shot since the last update *)
val update_game_state : game_state -> Timer.timer -> game_state 

(** [update_difficulty gs diff] is [gs] with game difficulty
    updated according to user input *)
val update_difficulty : game_state -> difficulty -> game_state 

(** [init st] is a fresh game_state with round_state [st], 
    empty score, and Welcome current_state. *)
val init : Round_state.t -> game_state

(** [string_of_game_state gs] is [gs], 
    nicely formatted in a string*)
val string_of_game_state : game_state -> string 