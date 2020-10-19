type t

val speed : float

val init : int -> int -> Position.t -> t

val move_horiz : Position.t -> t

val move_vert : Position.t -> t

val move_proj : t -> t