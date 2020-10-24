open Position

type t = {
  pos : Position.t;
  dir : int;  (* direction in degrees *)
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

let move_proj (p : t) =
  let newx = speed *. (sin (rad_of_deg p.dir)) in
  let newy = speed *. (cos (rad_of_deg p.dir)) in
  {p with pos = Position.make_pos newx newy}

let string_of_proj p = 
  "Pos: " ^ Position.string_of_pos p.pos ^ 
  "Dir: " ^ string_of_int p.dir