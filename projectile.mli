(** [Projectile] generation and movement *)

(** The type of a projectile
    [dir] is direction in degrees *)
type t = {
  pos : Position.t;
  dir : int;
}

(** [init i d pos] is a projectile with id [i], dir [d]; and pos [pos] *)
val init : int -> Position.t -> t

(**[move_proj p] is [p] moved one step, according to its direction *)
val move_proj : t -> t

(** [string_of_proj p] is the string representation of [p] *)
val string_of_proj : t -> string 