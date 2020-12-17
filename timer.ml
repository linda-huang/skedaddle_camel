open Unix 

type timer = {
  starttime : float;
  elapsedtime : int 
} 

let time_left (curr_round : Constant.round_info)
    (st : Round_state.t) (timer : timer) : int option = 
  let bonustime = 
    match st.camel.hourglasses with 
    | None | Some Pause -> 0 
    | Some Add -> Constant.hourglass_add in  
  let timelim = bonustime + curr_round.timelim in 
  if timelim = max_int then None 
  else Some (timelim - timer.elapsedtime)

let out_of_time (curr_round : Constant.round_info)
    (st : Round_state.t) (timer : timer) : bool = 
  let time_left = 
    match time_left curr_round st timer with 
    | Some i -> i 
    | None -> max_int in 
  if time_left <= 0 then true else false 

let update_timer (timer : timer) (paused : float) = 
  let time = Unix.gettimeofday () -. paused in 
  {timer with elapsedtime = 
                time -. timer.starttime |> int_of_float}

let init_timer () = {
  starttime = Unix.gettimeofday ();
  elapsedtime = 0
}

let string_of_timer timer = 
  "Starttime: " ^ string_of_float timer.starttime 
  ^ " Elapsed: " ^ string_of_int timer.elapsedtime