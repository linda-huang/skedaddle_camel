(** The type representing a pixel location. *)
type t = {x : float; y : float}

(* [dist p1 p2] is the euclidian distance between [p1] and [p2] *)
val dist : t -> t -> float 

(** [make_pos x y] creates the position with coordinates (x,y) *)
val make_pos : float -> float -> t 

(** [make_pos_2 p] creates the position with coordinates from [p] *)
val make_pos_2 : float * float -> t

(** [string_of_pos t] is the string formatting of [t] *)
val string_of_pos : t -> string 