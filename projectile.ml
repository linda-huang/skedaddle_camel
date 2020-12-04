open Position

type t = {
  pos : Position.t;
  dir : int;  (* direction in degrees *)
}

let speed = 1

let init d p = {
  dir = d;
  pos = p;
}

let move_horiz (proj : t) (sign : int) : t = 
  {proj with pos = {proj.pos with x = proj.pos.x + sign * speed}}

let move_vert (proj : t) (sign : int) : t = 
  {proj with pos = {proj.pos with y = proj.pos.y + sign * speed}}

(* let rad_of_deg d = (float_of_int d) *. ((acos (-1.)) /. 180.) *)

let move_proj (p : t) =
  let sign = if p.dir = 180 || p.dir = 270 then ~-1 else 1 in 
  if p.dir = 0 || p.dir = 180 
  then move_horiz p sign else move_vert p sign

let string_of_proj p = 
  "Pos: " ^ Position.string_of_pos p.pos ^ 
  "Dir: " ^ string_of_int p.dir