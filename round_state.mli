(** Inter-agent interactions within a single round of the game  *)

(** The type of a single round (or level) of the game *)
type t = {
  camel : Camel.t;
  maze : Maze.maze;
  cols : int;
  rows : int;
  enemies : Enemy.t array;
  coins : Coin.t array;
  potions : Potion.potion array;
  projectiles : Projectile.t list;
  genie : Genie.genie option;
  hourglass : Hourglass.hourglass option;
  top_left_corner: int * int;
  portals : Position.t list
}

(** [at_exit state] detects if camel's position is on the Exit tile of maze. *)
val at_exit : t -> bool

(** [hit_wall pos maze rad] detects if the position [pos] is a valid
    move in [maze] for a specific character, depending on the [rad] of the 
    character in question*)
val hit_wall : t -> Position.t -> int -> int -> bool

(** [hit_power_tile pos maze] detect if camel has hit a power tile and if 
    so updates camel in response to power tile effect *)
val hit_power_tile : t ->  Position.t -> Camel.t

(** [near_enemy camel maze] detects if [camel]'s position is near 
    an enemy camel *)
val near_enemy : Camel.t -> t -> bool

(** [near_genie camel maze] detects if [camel]'s position is near 
    a genie *)
(* val near_genie : Camel.t -> t -> bool  *)

(** [on_coin st] detects if the position of [camel] in [st] 
    is on a coin. *)
val on_coin : t -> bool

(** [rem_coin c st] is [st] with [c] removed *)
val remove_coin : Coin.t -> t -> t

(** [on_potion st] detects if the position of [camel] in [st] 
    is on a potion.*)
(* val on_potion : t -> bool  *)

(** [remove_potion pot st] is [st] with [pot] removed *)
(* val remove_potion : Potion.potion -> t -> t *)

(** [shoot camel] shoots a projectile in the direction of [camel]
    instantiates a new projectile in the state?? do we keep a list of all
    active projectiles as a field in the state *)
val shoot : Camel.t -> t -> t

(** [move_proj st] is the state with all active projectiles moved one step 
    (e.g. in a straight line according to their direction). If a projectile runs
    into a wall, it stops and is removed from the game. *)
(* val move_proj : t -> t  *)

(** [hit_enemy st] checks if any projectiles in [st] have hit an enemy. 
    If a projectile has hit an enemy, both the projectile and enemy 
    are removed from [st] *)
(* val hit_enemy : t -> t  *)

val move_camel_ice : t -> Camel.t -> Camel.t

(** [update_round_state st] is [st] with all agents updated one move. 
    All enemies moved one step; projectiles moved one unit; 
    any applicable coins and potions picked up; 
    camel score and health adjusted *)
val update_round_state : t -> t 

(** [init camel x y numenemy diff nportals] is a fresh round_state with [camel]
    at the beginning of an [x] x [y] maze with [numenemy] enemies,
    difficulty corresponding to [diff], and [nportals] number of portals *)
val init : int -> int -> int -> int -> int -> t

(** [string_of_round_state st] is [st], nicely formatted. *)
val string_of_round_state : t -> string 