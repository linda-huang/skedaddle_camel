open Position

type t = {
  pos : Position.t;
  dir : int;  (* direction in degrees *)
}

let speed = 15

let init d p = {
  dir = d;
  pos = p;
}

let move_horiz pos = 
  {pos with x = pos.x + speed}

let move_vert pos = 
  {pos with y = pos.y + speed}

(* let rad_of_deg d = (float_of_int d) *. ((acos (-1.)) /. 180.) *)

let move_proj (p : t) =
  let origx = p.pos.x in 
  let origy = p.pos.y in 
  let sign = if p.dir = 180 || p.dir = 270 then ~-1 else 1 in 
  if p.dir = 0 || p.dir = 180 then 
    {p with pos = Position.init_pos ((origx + (sign * speed)), origy)} 
  else {p with pos = Position.init_pos (origx, origy + (sign * speed))}

let string_of_proj p = 
  "Pos: " ^ Position.string_of_pos p.pos ^ 
  "Dir: " ^ string_of_int p.dir