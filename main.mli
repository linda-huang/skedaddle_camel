(** Run the entire game *)

(** [input st] updates [st] in response to user key presses *)
val input : Game_state.game_state -> Timer.timer -> Game_state.game_state

(** [run st] runs game responding to key presses *)
val run : Game_state.game_state -> Timer.timer -> unit

(** [init ()] creates a new game round_state and then runs the game *)
val init : unit -> unit

(** [main ()] starts the game on a user key press *)
val main : unit -> unit