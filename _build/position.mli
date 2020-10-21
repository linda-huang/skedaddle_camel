type t = {x : float; y : float}

val dist : t -> t -> float 

val make_pos : float -> float -> t 

val make_pos_2 : float * float -> t

val string_of_pos : t -> string 