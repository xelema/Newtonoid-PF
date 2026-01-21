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

let default_config = {
  rows = 5;
  cols = 10;
  brick_width = 70.0;
  brick_height = 20.0;
  gap_x = 5.0;
  gap_y = 5.0;
  start_x = 20.0;
  start_y = 400.0;
  value_fn = (fun row _col -> (row + 1) * 10);
  color_fn = (fun row _col ->
    match row mod 5 with
    | 0 -> Graphics.red
    | 1 -> Graphics.magenta
    | 2 -> Graphics.yellow
    | 3 -> Graphics.green
    | 4 -> Graphics.cyan
    | _ -> Graphics.white
  );
}

let make_grid config =
  let list_bricks =
    List.init config.rows (fun row ->
      List.init config.cols (fun col ->
        let x = config.start_x +. float_of_int col *. (config.brick_width +. config.gap_x) in
        let y = config.start_y +. float_of_int row *. (config.brick_height +. config.gap_y) in
        Brick.{ x; y; width = config.brick_width; height = config.brick_height;
              value = config.value_fn row col; color = config.color_fn row col }
      )
    ) |> List.flatten
  in
  let global_box = { 
    Brick.xmin = 0.0; 
    Brick.xmax = 800.0; 
    Brick.ymin = 0.0; 
    Brick.ymax = 600.0 
  } in
  Brick.build_tree global_box list_bricks

let classic () = make_grid default_config
