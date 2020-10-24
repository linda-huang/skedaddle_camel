open Position

type t = {
  pos : Position.t;
  value : int;
}

let init p v = {
  pos = p;
  value = v;
}

let string_of_coin c = Position.string_of_pos c.pos 