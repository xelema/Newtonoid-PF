type brick = {
  x      : float;          (* coin gauche *)
  y      : float;          (* coin bas *)
  width  : float;
  height : float;
  value  : int;            (* points *)
  color  : Graphics.color;
}

type bricks = brick list

val remove_brick : brick -> bricks -> bricks
