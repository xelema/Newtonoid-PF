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

(* Niveau 2 : Pyramide inversee *)
let level2_config = {
  rows = 5;
  cols = 10;
  brick_width = 70.0;
  brick_height = 18.0;
  gap_x = 5.0;
  gap_y = 4.0;
  start_x = 20.0;
  start_y = 420.0;
  value_fn = (fun row _col -> (row + 1) * 15);  (* x1.5 points *)
  color_fn = (fun row _col ->
    match row mod 5 with
    | 0 -> Graphics.blue
    | 1 -> Graphics.green
    | 2 -> Graphics.yellow
    | 3 -> Graphics.red
    | 4 -> Graphics.magenta
    | _ -> Graphics.white
  );
}

let make_pyramid config =
  let list_bricks =
    List.init config.rows (fun row ->
      (* Nombre de briques decroissant : 10, 8, 6, 4, 2 *)
      let cols_this_row = config.cols - (row * 2) in
      let cols_this_row = max 2 cols_this_row in
      let offset = float_of_int (config.cols - cols_this_row) *. (config.brick_width +. config.gap_x) /. 2.0 in
      List.init cols_this_row (fun col ->
        let x = config.start_x +. offset +. float_of_int col *. (config.brick_width +. config.gap_x) in
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

(* Niveau 3 : Damier - une brique sur deux *)
let level3_config = {
  rows = 6;
  cols = 10;
  brick_width = 70.0;
  brick_height = 18.0;
  gap_x = 5.0;
  gap_y = 4.0;
  start_x = 20.0;
  start_y = 400.0;
  value_fn = (fun row _col -> (row + 1) * 20);  (* x2 points *)
  color_fn = (fun row col ->
    if (row + col) mod 2 = 0 then Graphics.red
    else Graphics.cyan
  );
}

let make_checkerboard config =
  let list_bricks =
    List.init config.rows (fun row ->
      List.init config.cols (fun col ->
        (* Seulement les cases ou (row + col) est pair *)
        if (row + col) mod 2 = 0 then
          let x = config.start_x +. float_of_int col *. (config.brick_width +. config.gap_x) in
          let y = config.start_y +. float_of_int row *. (config.brick_height +. config.gap_y) in
          Some Brick.{ x; y; width = config.brick_width; height = config.brick_height;
                value = config.value_fn row col; color = config.color_fn row col }
        else
          None
      ) |> List.filter_map Fun.id
    ) |> List.flatten
  in
  let global_box = { 
    Brick.xmin = 0.0; 
    Brick.xmax = 800.0; 
    Brick.ymin = 0.0; 
    Brick.ymax = 600.0 
  } in
  Brick.build_tree global_box list_bricks

let get_level n =
  match n mod 3 with
  | 0 -> make_grid default_config      (* Niveau 1: grille classique *)
  | 1 -> make_pyramid level2_config    (* Niveau 2: pyramide inversee *)
  | 2 -> make_checkerboard level3_config (* Niveau 3: damier *)
  | _ -> make_grid default_config
