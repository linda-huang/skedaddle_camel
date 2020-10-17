open Position

type t = {
  id : int;
  pos : Position.t;
  value : int;
}

let init i p v = {
  pos = p;
  value = v;
  id = i;
}