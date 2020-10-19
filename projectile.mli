type t = {
  pos : Position.t;
  (* direction in degrees *)
  dir : int;
  id : int;
}

val speed : float

val init : int -> int -> Position.t -> t

val move_horiz : Position.t -> Position.t

val move_vert : Position.t -> Position.t

val move_proj : t -> t