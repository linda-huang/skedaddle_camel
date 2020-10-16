(* the type [t] represents a camel *)
type t 

val speed : float 
val rot : int   

val init : float -> float -> t 

val turn_right : t -> t 

val turn_left : t -> t 

(* position after moving horizontally *)
val move_horiz : t -> float -> t 

(* position after moving vertically *)
val move_vert : t -> float -> t

val string_of_camel : t -> string 
