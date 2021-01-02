(** Graphically represent game elements *)

(** [add_heart_img (x,y) lives] redraws the heart gained.*) 
val add_heart_img: int * int -> int -> unit

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

(** [draw_words height pos str_lst] draws a list of strings out onto the screen
    with each line of the array [height] apart, starting at [pos] *)
val draw_words : int -> Position.t -> string list -> unit

(** [draw_initial_round_state gs] draws components that will mainly stay the
    same. *)
val draw_initial_round_state : Round_state.t -> int -> unit