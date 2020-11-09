type t = {
  pos : Position.t;
  value : int;
}

(** [init p v] is a new coin at position [p] with value [v] *)
val init : Position.t -> int -> t

(* [on_coin camel maze] detects if [camel]'s position is on a coin *)
(* val on_coin : Camel.t -> t array -> bool *)

(** [find_coin p coins] is the coin at [p] in [coins].
    Requires: there must be a coin at [p]. *)
val find_coin : Position.t -> t array -> t 

(** [string_of_coin c] is the string representation of [c] *)
val string_of_coin : t -> string 