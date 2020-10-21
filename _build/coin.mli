type t = {
  pos : Position.t;
  value : int;
}

(** [init p v] is a new coin at position [p] with value [v] *)
val init : Position.t -> int -> t

(** [string_of_coin c] is the string representation of [c] *)
val string_of_coin : t -> string 