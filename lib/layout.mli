type config = {
  rows        : int;
  cols        : int;
  brick_width : float;
  brick_height: float;
  gap_x       : float;
  gap_y       : float;
  start_x     : float;
  start_y     : float;
  value_fn    : int -> int -> int;           (* row -> col -> value *)
  color_fn    : int -> int -> Graphics.color;
}

val default_config : config
val make_grid : config -> Brick.bricks
val classic : unit -> Brick.bricks
