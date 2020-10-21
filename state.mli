type t = {
  camel : Camel.t;
  maze : Maze.maze;
  x_size : int;
  y_size : int;
  enemies : Enemy.t array;
  coins : Coin.t array;
  projectiles : Projectile.t list;
}

val camel_width : float
val tile_width : float
val near : float

val tile_to_pixel : int -> int -> float * float

val init_enemy_lst : int -> Maze.maze -> Enemy.t array

(*val valid_spawn_pos : int -> Maze.maze -> Position.t list (* TODO *)*)

val curr_tile : Position.t -> int * int

val hit_wall : Position.t -> Maze.maze -> bool

val near_enemy : Camel.t -> t -> bool

val on_coin : Camel.t -> t -> bool

val shoot : Camel.t -> t -> t

val move_proj : t -> t 

val move_enemies : t -> t 

val init : Camel.t -> int -> int -> int -> t

val string_of_state : t -> string 