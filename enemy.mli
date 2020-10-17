type t

val turn_around : t -> t

val move_left : Position.t -> Position.t

val move_right : Position.t -> Position.t

val speed : float

val init : int -> Position.t -> int