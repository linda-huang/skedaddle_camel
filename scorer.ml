open Camel

type t = {
  mazes : int;
  time : float list; 
  hit : int; 
  coins : int
}

let time_mult = 100.
let hit_bonus = 10
let health_bonus = 50

let update_score scr time (camel : Camel.t) =
  let newtimes = 
    match scr.time with 
    | [] -> [time] 
    | h :: t -> (time -. h) :: scr.time 
  in {scr with mazes = scr.mazes + 1; 
               time = newtimes; 
               coins = scr.coins + camel.coins} 

let score scr camel timed = 
  let health = if camel.health < 0 then 0 else camel.health in 
  let timecalc = 
    if timed && health <> 0 then List.fold_left 
        (fun acc x -> 
           let x' = if x > time_mult then 0. else time_mult -. x in 
           acc +. x') 
        0. scr.time 
    else 0. in 
  scr.mazes * (1 + int_of_float timecalc) + 
  scr.hit * hit_bonus + 
  health * health_bonus + scr.coins + camel.coins 

let string_of_score scr camel timed = 
  let num = score scr camel timed in 
  "Score: " ^ string_of_int num ^ 
  "       Lives Remaining: " ^ string_of_int camel.health 

let init () = 
  {mazes = 0; time = []; hit = 0; coins = 0}