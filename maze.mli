(** The abstract type representing the value of a tile in a maze. *)
type t

(** The type of maze. *)
type maze = t array array

(** [populate maze m n (x, y)] is a randomly populated maze with m rows and 
    n columns. [(x, y)] is the index of the starting position.
    Requires:
    [m] >= 1
    [n] >= 1
*)
val populate : int -> int -> (int*int) -> maze

(** [is_wall maze x y] is true the tile ([x],[y]) is a Wall. 
    Requires:
    0 <= [x] < number of columns of maze
    0 <= [y] < number of rows of maze. 
*)
val is_wall : maze -> int -> int -> bool

(** [is_exit maze x y] is true when the tile ([x],[y]) is an exit. *)
val is_exit : maze -> int -> int -> bool



