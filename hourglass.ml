open Position

type hourglass_power = Add | Pause 

type hourglass = {
  pos : Position.t;
  power : hourglass_power;
}

let init p = 
  Random.self_init (); 
  let choosepower = Random.int 4 in 
  {pos = p; power = 
              if choosepower = 0 
              then Pause else Add;}

let string_of_hourglass hg = 
  "Hourglass: " ^ Position.string_of_pos hg.pos