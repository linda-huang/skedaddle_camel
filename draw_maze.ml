open Graphics;;
open Maze;;
open Camel
(** [draw_element x y color] draws a rectangle of size [x] by [y] in [color].*)
let draw_element x y color size= 
  set_color color;
  fill_poly [|(x,y); (x+size,y); (x+size, y-size); 
              (x, y-size)|]

(** [draw_walls gen_maze start_pos maze_row maze_col] draws a maze of *)
let draw_walls gen_maze start_pos maze_row maze_col = 
  let curr_pos = ref start_pos in
  for i = 0 to maze_row - 1 do begin
    curr_pos := ((fst !curr_pos), (snd start_pos) - i*path_width);
    for j = 0 to maze_col - 1 do begin  
      curr_pos := ((fst start_pos) + (j)*path_width , snd !curr_pos);
      if tile_type gen_maze j i = Wall then begin
        draw_element (fst !curr_pos) (snd !curr_pos) Graphics.black path_width;
      end
      else
        draw_element (fst !curr_pos) (snd !curr_pos) Graphics.green path_width;
    end
    done
  end
  done 


let is_dead camel = camel.health = 0

(* [update_camel st] is the state with the camel's 
   health and coin total updated *)
let update_camel (st : State.t) : State.t = 
  let camel = st.camel in 
  let camel' = if (State.near_enemy camel st) then 
      {camel with health = camel.health - 1} else camel in 
  let camel'' = if (State.on_coin camel st) then 
      {camel with coins = camel.coins + 1} else camel' in 
  if (is_dead camel'') then failwith " game over " else 
    {st with camel = camel''}  

let draw_state state = failwith "todo" 

let input (st : State.t) (k : char) : State.t =  
  (* Graphics.moveto 50 500; *)
  let camel = st.camel in 
  let st' = 
    match k with 
    | '0' -> exit 0 
    | 'w' -> {st with camel = (Camel.move_vert camel 1.)}
    | 'a' -> {st with camel = (Camel.move_horiz camel ~-.1.)}
    | 's' -> {st with camel = (Camel.move_vert camel ~-.1.)}
    | 'd' -> {st with camel = (Camel.move_horiz camel 1.)}
    | 'e' -> {st with camel = (Camel.turn_right camel)}
    | 'q' -> {st with camel = (Camel.turn_left camel)}
    | ' ' -> State.shoot camel st
    | _ -> {st with camel = camel} 
  in 
  let st'' = if State.hit_wall st'.camel.pos st'.maze then st else st' in 
  st'' |> State.move_proj |> State.move_enemies 



let rec run (st : State.t) gen_maze start_pos maze_row maze_col = 
  draw_walls gen_maze start_pos maze_row maze_col;
  draw_element (int_of_float st.camel.pos.x + (fst start_pos + 2)) (int_of_float st.camel.pos.y + (snd start_pos - 2)) Graphics.blue 8;
  Graphics.draw_string "press a key to move";
  let s = wait_next_event[Key_pressed] in 
  if s.keypressed then 
    (
      let newst = input st s.key in 

      Graphics.synchronize ();
      run newst gen_maze start_pos maze_row maze_col;)

(*draw_state State.init_state; run State.init_state*)
let init k gen_maze start_pos maze_row maze_col= 
  let camel = Camel.init 0. 0. in 
  let st = State.init camel 10 10 1 in 

  run st gen_maze start_pos maze_row maze_col

(** [main m n] generates a new maze dimensions [m] x [n]. 
    Requires: [m] and [n] to be positive and odd. *)
let main m n = 
  let maze_row = m in
  let maze_col = n in
  let window_height = maze_row * path_width + 200 in
  let window_width = maze_col * path_width + 200 in
  Graphics.open_graph (" " ^ (string_of_int window_width) ^ "x" ^ 
                       (string_of_int window_height));
  set_window_title "Camel Maze";
  moveto (window_width / 2 - 55) (window_height - 50);
  Graphics.set_text_size 300; 
  Graphics.draw_string "WELCOME TO CAMEL MAZE";
  Graphics.auto_synchronize false;
  let gen_maze = populate maze_row maze_col (0,0) in
  let start_y = window_height - ((window_height- maze_row * path_width) / 2) in
  let start_x = ((window_width - maze_col * path_width) / 2) in
  let start_pos = (start_x, start_y) in

  (* let s = wait_next_event[Key_pressed] in
     if s.keypressed then Graphics.close_graph () *)
  match Graphics.read_key () with 
  | k -> init k gen_maze start_pos maze_row maze_col 

let () = main 45 45
