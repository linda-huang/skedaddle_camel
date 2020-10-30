open Camel

type t = {
  mazes : int;
  time : float list; 
}

let time_mult = 100.

let update_time scr time = 
  let newtimes = 
    match scr.time with 
    | [] -> [time] 
    | h :: t -> (time -. h) :: scr.time 
  in {mazes = scr.mazes + 1; time = newtimes} 

let score scr camel = 
  let health = camel.health in 
  (* let enemies = camel.bodycount in *)
  let timecalc = List.fold_left 
      (fun acc x -> let x' = if x > time_mult then 0. else time_mult -. x in 
        acc +. x') 0. scr.time in 
  scr.mazes * health * int_of_float timecalc 

let init () = 
  {mazes = 0; time = []}