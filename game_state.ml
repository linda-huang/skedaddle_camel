open Round_state

type state = Welcome | GameOver | InPlay

type game_state = {
  score : Scorer.t;
  current_state : state;
  round_state : Round_state.t
}

let set_game_state g s = 
  { g with current_state = s }

let get_game_state g = g.current_state