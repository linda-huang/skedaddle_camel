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

(** [shoot camel] shoots a projectile in the direction of [camel]
    instantiates a new projectile in the state?? do we keep a list of all
    active projectiles as a field in the state *)
val shoot : Camel.t -> t -> t

(** [move_camel_ice st camel] is [camel] moved accordingly 
    if it has hit an ice tile  *)
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