type brick = {
  x      : float;          (* coin gauche *)
  y      : float;          (* coin bas *)
  width  : float;
  height : float;
  value  : int;            (* points *)
  color  : Graphics.color;
}

type bricks = brick list

let remove_brick brick bricks =
  List.filter (fun b -> b != brick) bricks
