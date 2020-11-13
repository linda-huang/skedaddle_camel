open Round_state

type state = Welcome | GameOver | Won | InPlay

type game_state = {
  score : Scorer.t;
  current_state : state;
  round_state : Round_state.t
}

(* [round_info] stores constants about each round
   e.g. the size of maze and number of enemies *)
type round_info = {
  dimx : int;
  dimy : int;
  enemies : int
}

let round1 = {dimx = 11; dimy = 11; enemies = 1}
let round2 = {dimx = 15; dimy = 15; enemies = 5}
let round3 = {dimx = 21; dimy = 21; enemies = 10}

let totrounds = 3

let set_game_state g s = 
  { g with current_state = s }

let get_game_state g = g.current_state

let new_level (gs : game_state) : game_state = 
  if gs.current_state = Welcome then 
    {gs with current_state = InPlay; 
             round_state = Round_state.init 
                 round1.dimx round1.dimy round1.enemies} 
  else 
    let newscr = Scorer.update_time gs.score (Sys.time ()) in
    if gs.score.mazes = totrounds-1 then {gs with current_state = Won} 
    else if Camel.is_dead gs.round_state.camel then 
      {gs with current_state = GameOver} 
    else 
      let round = if gs.score.mazes = 0 then round2 else round3 in 
      let newstate = Round_state.init round.dimx round.dimy round.enemies in 
      {gs with score = newscr; round_state = newstate}

let update_game_state (gs : game_state) : game_state = 
  let st = Round_state.update_round_state gs.round_state in 
  if Round_state.at_exit st then new_level gs else 
  if Camel.is_dead st.camel then 
    {gs with current_state = GameOver; round_state = st} 
  else 
    let enemies_hit = 
      (Array.length gs.round_state.enemies) - (Array.length st.enemies) in 
    {gs with round_state = st; 
             score = {gs.score with hit = gs.score.hit + enemies_hit}} 

let init (st : Round_state.t) : game_state = 
  {score = Scorer.init (); current_state = Welcome; round_state = st}

let string_of_game_state (gs : game_state) : string = 
  let msg = match get_game_state gs with 
    | Welcome -> "Welcome!"
    | InPlay -> "Game in progress"
    | GameOver -> "Game over" 
    | Won -> "You've won!"
  in
  msg ^ " \ " ^ (Scorer.string_of_score gs.score gs.round_state.camel) ^ 
  " \ " ^ Round_state.string_of_round_state gs.round_state  