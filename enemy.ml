open Position

type enemy = {
  id : int;
  dir : int;
  pos : Position.t;
}

type t = enemy

let speed = 1.5

let turn_around camel = {
  id = camel.id;
  dir = (camel.dir + 180) mod 360;
  pos = camel.pos
}

(* let move_left camel = {
   id = camel.id;
   dir = camel.dir;
   speed = camel.speed;
   pos = camel.pos
   } *)

let move_horiz pos = 
  {pos with x = pos.x +. speed}

let move_vert pos = 
  {pos with y = pos.y +. speed}

let init i d p = {
  id = i;
  dir = d;
  pos = p;
}