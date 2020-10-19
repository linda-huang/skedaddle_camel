open Position

type t = {
  pos : Position.t;
  (* direction in degrees *)
  dir : int;
  id : int;
}

let speed = 15.

let init i d p = {
  id = i;
  dir = d;
  pos = p;
}

let move_horiz pos = 
  {pos with x = pos.x +. speed}

let move_vert pos = 
  {pos with y = pos.y +. speed}

let rad_of_deg d = (float_of_int d) *. ((acos (-1.)) /. 180.)

let move_proj p =
  let newx = p.x = speed *. (sin (rad_of_deg p.dir)) in
  let newy = p.y = speed *. (cos (rad_of_deg p.dir)) in
  {p with pos = {newx, newy}}