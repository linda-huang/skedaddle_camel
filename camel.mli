(* the type [t] represents a camel *)
type t 

val speed : float 
val rot : int   

val turn_right : t -> t 

val turn_left : t -> t 

(* position after moving horizontally *)
val move_horiz : Position.t -> Position.t 

(* position after moving vertically *)
val move_vert : Position.t -> Position.t 

val string_of_camel : t -> string 
