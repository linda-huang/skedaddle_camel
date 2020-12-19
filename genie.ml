open Position
open Constant

type genie = {
  dir : int;
  pos : Position.t;
  lastteleport : float;
}

let change_dir genie dir = 
  {genie with dir = dir} 

let turn_around genie = 
  Random.self_init ();
  {genie with dir = (genie.dir + (Random.int 4) * 90) mod 360}

let move_horiz pos sign = 
  {pos with x = pos.x + sign * Constant.genie_speed}

let move_vert pos sign = 
  {pos with y = pos.y + sign * Constant.genie_speed}

let move genie =
  let dir = genie.dir mod 360 in 
  if dir = 0 then {genie with pos = (move_horiz genie.pos 1)}
  else if dir = 180 then {genie with pos = (move_horiz genie.pos ~-1)}
  else if dir = 90 then {genie with pos = (move_vert genie.pos 1)}
  else {genie with pos = (move_vert genie.pos ~-1)}

let init d p = {
  dir = d;
  pos = p;
  lastteleport = 0.
}

let string_of_genie e = 
  "Genie position: "  ^ Position.string_of_pos e.pos ^ 
  " Dir: " ^ string_of_int e.dir 