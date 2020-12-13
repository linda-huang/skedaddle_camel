(** Graphically represent game elements *)

(** [draw_maze st] draws the maze corresponding to [st] *)
val draw_maze : Round_state.t -> unit

(** [draw_camel camel] draws a rectangle representing the player's camel.*)
val draw_camel : Camel.t -> unit

(** [draw_enemy enemy] draws a rectangle representing an Enemy camel. *)
val draw_enemy : Enemy.t -> unit

(** [draw_coin coin] draws a rectangle representing a coin. *)
val draw_coin : Coin.t -> unit

(** [draw_projectile proj] draws a rectangle representing a Projectile *)
val draw_projectile : Projectile.t -> unit

(** [draw_round_state st] is the Graphics representation of [st]. *)
val draw_round_state : Round_state.t -> unit

(** [draw_game_state gs] is the Graphics representation of [gs]. *)
val draw_game_state : Game_state.game_state -> Timer.timer -> unit 
