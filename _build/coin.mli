type t = {
  id : int;
  pos : Position.t;
  value : int;
}

(** [init i p v] is a new coin at position [p] with value [v] *)
val init : int -> Position.t -> int -> t