type t = {
  dir : int;
  pos : Position.t;
}

val turn_around : t -> t

val move : t -> t

val speed : float

val init : int -> Position.t -> t

val string_of_enemy : t -> string  