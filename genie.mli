(** Automated movement of the genie *)

(** The type of a genie *)
type genie = {
  dir : int;
  pos : Position.t;
  lastteleport : float;
}

(** [change_dir genie dir] is [genie] going in direction [dir].
    Requires: [dir] must be a multiple of 90 *)
val change_dir : genie -> int -> genie

(** [move genie] is [genie] moved one step *)
val move : genie -> genie

(** [init d p] is a new Genie with direction [d] and position [p] *)
val init : int -> Position.t -> genie

(** [string_of_genie g] is the string representation of [g] *)
val string_of_genie : genie -> string  