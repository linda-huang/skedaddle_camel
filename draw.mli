(** [main m n] generates a new maze dimensions [m] x [n]. 
    Requires: [m] and [n] to be positive and odd. *)
val draw_maze : Round_state.t -> unit

(** [draw_camel camel] draws a rectangle representing the player's camel.*)
val draw_camel : Camel.t -> unit

(** [draw_enemy enemy] draws a rectangle representing an Enemy camel. *)
val draw_enemy : Enemy.t -> unit

(** [draw_coin coin] draws a rectangle representing an Enemy camel. *)
val draw_coin : Coin.t -> unit

val draw_projectile : Projectile.t -> unit

(** [draw_round_state st] is the Graphics representation of [st]. *)
val draw_round_state : Round_state.t -> unit

val draw_game_state : Game_state.game_state -> unit 
