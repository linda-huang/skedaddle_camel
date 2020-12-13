open Unix 

type timer = {
  starttime : float;
  elapsedtime : int 
} 

let update_timer (timer : timer) = 
  let time = Unix.gettimeofday () in 
  {timer with elapsedtime = 
                time -. timer.starttime |> int_of_float}

let init_timer () = {
  starttime = Unix.gettimeofday ();
  elapsedtime = 0
}