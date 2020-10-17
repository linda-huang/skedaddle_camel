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

