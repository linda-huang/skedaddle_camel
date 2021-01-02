open Position
open Constant

type t = {
  dir : int;
  pos : Position.t;
}

let change_dir camel dir = {camel with dir = dir} 

(** [turn_around camel] is [camel] facing a new random direction
    that is a multiple of 90 *)
let turn_around camel = 
  Random.self_init ();
  {camel with dir = (camel.dir + (Random.int 4) * 90) mod 360}

(** [move_horiz pos sign] is a new postion stepped one step left or right, 
    corresponding to [sign] *)
let move_horiz pos sign = 
  {pos with x = pos.x + sign * Constant.enemy_speed}

(** [move_vert pos sign] is a new postion stepped one step up or down, 
    corresponding to [sign] *)
let move_vert pos sign = 
  {pos with y = pos.y + sign * Constant.enemy_speed}

let move enemy =
  let dir = enemy.dir mod 360 in 
  if dir = 0 then {enemy with pos = (move_horiz enemy.pos 1)}
  else if dir = 180 then {enemy with pos = (move_horiz enemy.pos ~-1)}
  else if dir = 90 then {enemy with pos = (move_vert enemy.pos 1)}
  else {enemy with pos = (move_vert enemy.pos ~-1)}

let init d p = {
  dir = d;
  pos = p;
}

let string_of_enemy e = 
  "Enemy position: "  ^ Position.string_of_pos e.pos ^ 
  " Dir: " ^ string_of_int e.dir 