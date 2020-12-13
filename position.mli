(** Computing distances and conversion between pixels and tiles *)

(** The abstract type representing a pixel location. *)
type t = {
  x : int; 
  y : int
}

(** The abstract type representing whether a pixel 
    is within bounds for the maze *)
type v = 
  | Valid of int * int 
  | Out_of_bounds

(** [dist p1 p2] is the euclidian distance between [p1] and [p2] *)
val dist : t -> t -> int

(** [init_pos p] creates the position with coordinates from [p] *)
val init_pos : int * int -> t

(** [string_of_pos t] is the string formatting of [t] *)
val string_of_pos : t -> string 

(** [tile_to_pixel] is the center pixel corresponding to the current 
    maze.[row].[col] tile.
    Requires:
    [col] >= 0
    [row] >= 0
*)
val tile_to_pixel : int * int -> int * int -> int * int

(** [pixel_to_tile px py] is the index of maze tile at pixel ([px],[py]) 
    Requires:
    [px] >= 0
    [py] >= 0*)
val pixel_to_tile : t -> int * int -> v

