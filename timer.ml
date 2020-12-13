open Unix 

type timer = {
  starttime : float;
  elapsedtime : int 
} 

let time_left (curr_round : Constant.round_info) (timer : timer) : int option = 
  let timelim = curr_round.timelim in 
  if timelim = max_int then None 
  else Some (timelim - timer.elapsedtime)

let out_of_time (timer : timer) timelim : bool = 
  if timer.elapsedtime > timelim then true else false 

let update_timer (timer : timer) = 
  let time = Unix.gettimeofday () in 
  {timer with elapsedtime = 
                time -. timer.starttime |> int_of_float}

let init_timer () = {
  starttime = Unix.gettimeofday ();
  elapsedtime = 0
}