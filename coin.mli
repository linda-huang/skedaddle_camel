(** Coin generation and finding *)

(** The type of a coin *)
type t = {
  pos : Position.t;
  value : int;
}

(** [init p v] is a new coin at position [p] with value [v] *)
val init : Position.t -> int -> t

(** [find_coin p coins] is the coin at [p] in [coins].
    Requires: there must be a coin at [p]. *)
val find_coin : Position.t -> t array -> t 

(** [string_of_coin c] is the string representation of [c] *)
val string_of_coin : t -> string 