open Round_state

type state = Welcome | GameOver | InPlay

type game_state = {
  score : Scorer.t;
  current_state : state;
  round_state : Round_state.t
}

type round_info = {
  dimx : int;
  dimy : int;
  enemies : int
}

let round1 = {dimx = 15; dimy = 15; enemies = 2}
let round2 = {dimx = 33; dimy = 33; enemies = 5}
let round3 = {dimx = 57; dimy = 57; enemies = 10}

let set_game_state g s = 
  { g with current_state = s }

let get_game_state g = g.current_state

let new_level (gs : game_state) : game_state = 
  let newscr = Scorer.update_time gs.score (Sys.time ()) in
  let round = if gs.score.mazes = 1 then round1 else 
    if gs.score.mazes = 2 then round2 else round3 in 
  let newstate = Round_state.init round.dimx round.dimy round.enemies in 
  {gs with score = newscr; round_state = newstate}

let init (st : Round_state.t) : game_state = 
  {score = Scorer.init (); current_state = Welcome; round_state = st}

let string_of_game_state (gs : game_state) : string = 
  let msg = match get_game_state gs with 
    | Welcome -> "Welcome!"
    | InPlay -> "Game in progress"
    | GameOver -> "Game over" 
  in
  msg ^ " \ " ^ (Scorer.string_of_score gs.score gs.round_state.camel) ^ 
  " \ " ^ Round_state.string_of_round_state gs.round_state  