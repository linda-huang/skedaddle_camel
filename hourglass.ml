open Position
open Constant 

type hourglass_power = Add | Pause 

type hourglass = {
  pos : Position.t;
  power : hourglass_power;
}

let init p = 
  Random.self_init (); 
  let choosepower = Random.int Constant.hourglass_freq in 
  {pos = p; power = 
              if choosepower = 0 
              then Pause else Add;}

let string_of_hourglass hg = 
  "Hourglass: " ^ Position.string_of_pos hg.pos