open Constant 

type t = {x : int; y : int}

type v = Valid of int * int | Out_of_bounds

let dist p1 p2 = 
  let sqr n = n * n in 
  int_of_float ((float_of_int (sqr (p1.x - p2.x)) +. 
                 float_of_int (sqr (p1.y - p2.y))
                 |> sqrt ))


let init_pos tuple = 
  {x = fst tuple; y = snd tuple}

let string_of_pos p = 
  "("  ^ string_of_int p.x ^ ", " ^ string_of_int p.y ^ ")"

let tile_to_pixel start_pos (col, row)  = 
  if col < 0 || row < 0 then raise (Invalid_argument "negative") else 
    let f p = Constant.tile_width * p + Constant.tile_radius in 
    (fst start_pos + f col, snd start_pos - f row)

let pixel_to_tile (pos : t) (start_pos : int * int) =
  let x_diff = pos.x - fst start_pos in
  let y_diff = snd start_pos - pos.y in
  if x_diff < 0 || y_diff < 0 then Out_of_bounds else
    Valid (x_diff / tile_width, y_diff / tile_width) 