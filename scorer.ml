open Camel

type t = {
  mazes : int;
  time : float list; 
  hit : int 
}

let time_mult = 100.
let hit_bonus = 10
let health_bonus = 50

let update_time scr time = 
  let newtimes = 
    match scr.time with 
    | [] -> [time] 
    | h :: t -> (time -. h) :: scr.time 
  in {scr with mazes = scr.mazes + 1; time = newtimes} 

let score scr camel = 
  let health = if camel.health < 0 then 0 else camel.health in 
  let timecalc = List.fold_left 
      (fun acc x -> let x' = if x > time_mult then 0. else time_mult -. x in 
        acc +. x') 0. scr.time in 
  scr.mazes * int_of_float timecalc + scr.hit * hit_bonus + 
  health * health_bonus

let string_of_score scr camel = 
  let num = score scr camel in 
  "Score: " ^ string_of_int num ^ 
  "       Lives Remaining: " ^ string_of_int camel.health 

let init () = 
  {mazes = 0; time = []; hit = 0}