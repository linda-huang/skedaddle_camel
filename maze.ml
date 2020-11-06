(* open Graphics;;  *)
open Graphics

(* denotes whether the position is a wall, a path, or an exit, or start*)
type t = 
  | Wall
  | Path 
  | Exit
  | Start

(* maze as a 2d array*)
type maze = t array array

let fightingring : maze = 
  let arr = Array.init 30 (fun i -> Array.init 30 (fun i -> Path)) in 
  (arr.(0)).(0) <- Start;
  (arr.(29)).(29) <- Exit;
  arr


let path_width = 12

let in_limit maze posx posy = (posx >= 0) && (posx < Array.length maze) && 
                              (posy >= 0) && (posy < Array.length maze.(0))

let visited maze newx newy = 
  if maze.(newx).(newy) = Wall then false else true

let exchange arr i j = 
  let temp = arr.(i) in 
  arr.(i) <- arr.(j);
  arr.(j) <- temp

let shuffle arr = 
  let rec helper arr counter =
    if counter <> 0 then 
      let rand_i = Random.int (Array.length arr) in
      let rand_j = Random.int (Array.length arr) in
      exchange arr rand_i rand_j;
      helper arr (counter - 1)
    else ()
  in helper arr 4

let clear_path maze posx posy newx newy = 
  maze.(newx).(newy) <- Path;
  let diffx = newx - posx in
  let diffy = newy - posy in
  if diffx = -2 then maze.(posx-1).(posy) <- Path
  else if diffx = 2 then maze.(posx+1).(posy) <- Path
  else if diffy = -2 then maze.(posx).(posy - 1) <- Path
  else if diffy = 2 then maze.(posx).(posy + 1) <- Path

let rec dfs maze posx posy =
  let direction = [|(posx - 2, posy); (posx + 2, posy); 
                    (posx, posy - 2); (posx, posy + 2)|] in 
  shuffle direction;
  for i = 0 to Array.length direction - 1 do begin
    let newx, newy = direction.(i) in
    if in_limit maze newx newy && not (visited maze newx newy) then begin
      clear_path maze posx posy newx newy;
      dfs maze newx newy
    end
    else ()
  end
  done

let populate n m start_pos = 
  let maze = Array.make_matrix n m Wall in
  let startx, starty = start_pos in
  dfs maze startx starty;
  maze.(startx).(starty) <- Path;
  maze.(0).(0) <- Start;
  maze.(n-1).(m-1) <- Exit;
  maze

(* let maze x y = if maze.(y).(x) = Wall then true else false *)

let tile_type maze x y = 
  match maze.(y).(x) with
  | Exit -> Exit
  | Start -> Start
  | Wall -> Wall
  | Path -> Path