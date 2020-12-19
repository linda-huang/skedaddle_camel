open Position
open Constant 

type potion = {
  pos : Position.t
}

let init p = {
  pos = p
}

let find_potion (p : Position.t) (potions : potion array) = 
  let lst = Array.fold_left (fun acc (potion : potion) -> 
      if (Position.dist p potion.pos < 
          (Constant.potion_radius + Constant.camel_radius))
      then potion :: acc else acc) [] potions in 
  if List.length lst = 0 then raise (Invalid_argument "No potion here") 
  else (List.hd lst) 

let string_of_potion c = Position.string_of_pos c.pos 