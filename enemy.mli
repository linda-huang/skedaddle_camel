type t = {
  id : int;
  dir : int;
  pos : Position.t;
}

val turn_around : t -> t

val move : t -> t

val speed : float

val init : int -> int -> Position.t -> t