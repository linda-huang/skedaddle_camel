(** Hourglass generation *)

(** The type of the power of an hourglass.*)
type hourglass_power = Add | Pause 

(** The type of an hourglass *)
type hourglass = {
  pos : Position.t;
  power : hourglass_power 
}

(** [init p] is a new hourglass at position [p] 
    It has [Pause] power 1/4 of the time. Otherwise, it has [Add] power *)
val init : Position.t -> hourglass

(** [string_of_hourglass hg] is the string representation of [hg] *)
val string_of_hourglass : hourglass -> string 