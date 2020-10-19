open Position

type t = {
  id : int;
  dir : int;
  pos : Position.t;
}

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

let move enemy =
  if enemy.dir mod 180 = 0 then {enemy with pos = (move_horiz enemy.pos)}
  else {enemy with pos = (move_vert enemy.pos)}

let init i d p = {
  id = i;
  dir = d;
  pos = p;
}