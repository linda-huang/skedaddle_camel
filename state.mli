(** The type representing the state of the game *)
type t = {
  camel : Camel.t;
  maze : Maze.maze;
  x_size : int;
  y_size : int;
  enemies : Enemy.t array;
  coins : Coin.t array;
  projectiles : Projectile.t list;
}

val camel_width_int : int 
(** [camel_width] is the width of the Camel character, in pixels *)
val camel_width : float

(** [tile_width] is the width of a single tile, in pixels *)
val tile_width : float

(** [width] is the number of pixels that defines close contact *)
val near : float

(* [tile_to_pixel tx ty] is the (center) pixel location from [tx]th, [ty]th tile *)
val tile_to_pixel : int -> int -> float * float

(* [curr_tile pos] is the 0-indexed tile number corresponding to [pos] *)
val curr_tile : Position.t -> int * int

(* [hit_wall pos maze] detect if the position is a valid
   move in [maze] *)
val hit_wall : Position.t -> Maze.maze -> bool

(* [near_enemy camel maze] detects if [camel]'s position is near 
   an enemy camel *)
val near_enemy : Camel.t -> t -> bool

(* [on_coin camel maze] detects if [camel]'s position is on a coin *)
val on_coin : Camel.t -> t -> bool

(** [find_coin p st] is the coin at [p] in [st].
    Requires: there must be a coin at [p]. *)
val find_coin : Position.t -> t -> Coin.t 

(* [rem_coin c st] is [st] with [c] removed *)
val rem_coin : Coin.t -> t -> t

(* [shoot camel] shoots a projectile in the direction of [camel]
   instantiates a new projectile in the state?? do we keep a list of all
   active projectiles as a field in the state *)
val shoot : Camel.t -> t -> t

(* [move_proj st] is the state with all active projectiles moved one step 
   (e.g. in a straight line according to their direction). If a projectile runs
   into a wall, it stops and is removed from the game. *)
val move_proj : t -> t 

(** [move_enemies st] is the state after updating the position of all enemy
    camels. *)
val move_enemies : t -> t 

(** [init camel x y numenemy] is a fresh state with [camel] at
    the beginning of an [x] x [y] maze with [numenemy] enemies *)
val init : Camel.t -> int -> int -> int -> t

(** [string_of_state st] is [st], nicely formatted. *)
val string_of_state : t -> string 