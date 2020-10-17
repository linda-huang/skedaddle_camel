type t (* = {
          camel : Camel.t;
          maze : Maze.maze;
          enemies : Enemy.t array;
          coins : Coin.t array;
          } *)

val camel_widt : float
val tile_width : float
val near : float

val tile_to_pixel : float -> float -> float * float

val enemy_lst : int -> 'a array

val valid_spawn_pos : int -> Position.t list (* TODO *)

val curr_tile : Position.t -> float * float

val hit_wall : Position.t -> Maze.t -> bool

val near_enemy : Camel.t -> t -> bool

val on_coin : Camel.t -> t -> bool
