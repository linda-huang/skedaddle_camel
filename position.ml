open Constant 

type t = {x : int; y : int}

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
  ((pos.x - fst start_pos ) / tile_width, 
   (snd start_pos - pos.y) / tile_width) 