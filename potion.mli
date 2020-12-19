(** Potion generation and finding *)

(** The type of a coin *)
type potion = {
  pos : Position.t
}

(** [init p] is a new potion at position [p] *)
val init : Position.t -> potion

(** [find_potion p potions] is the potion at [p] in [potions].
    Requires: there must be a potion at [p]. *)
val find_potion : Position.t -> potion array -> potion 

(** [string_of_coin c] is the string representation of [c] *)
val string_of_potion : potion -> string 