type state = Welcome | GameOver | InPlay

type game_state =  {
  score : Scorer.t;
  current_state : state;
  round_state : Round_state.t
}