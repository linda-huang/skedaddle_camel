open Position
open Constant

type t = {
  pos : Position.t;
  dir : int;  
}

let init d p = {
  dir = d;
  pos = p;
}

(** [move_vert proj sign] is [proj] moved one step left or right, 
    corresponding to [sign] *)
let move_horiz (proj : t) (sign : int) : t = 
  {proj with pos = {proj.pos with 
                    x = proj.pos.x + sign * Constant.projectile_speed}}

(** [move_vert proj sign] is [proj] stepped one step up or down, 
    corresponding to [sign] *)
let move_vert (proj : t) (sign : int) : t = 
  {proj with pos = {proj.pos with 
                    y = proj.pos.y + sign * Constant.projectile_speed}}

let move_proj (p : t) =
  let sign = if p.dir = 180 || p.dir = 270 then ~-1 else 1 in 
  if p.dir = 0 || p.dir = 180 
  then move_horiz p sign 
  else move_vert p sign

let string_of_proj p = 
  "Pos: " ^ Position.string_of_pos p.pos ^ 
  "Dir: " ^ string_of_int p.dir