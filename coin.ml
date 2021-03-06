open Position
open Camel 
open Constant 

type t = {
  pos : Position.t;
  value : int;
}

let init p v = {
  pos = p;
  value = v;
}

let find_coin (p : Position.t) (coins : t array) = 
  let lst = Array.fold_left (fun acc (coin : t) -> 
      if (Position.dist p coin.pos < 
          (Constant.coin_radius + Constant.camel_radius))
      then coin ::acc else acc) [] coins in 
  if List.length lst = 0 then raise (Invalid_argument "No coin here") 
  else (List.hd lst) 

let string_of_coin c = Position.string_of_pos c.pos 