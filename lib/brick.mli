type brick = {
  x      : float;
  y      : float;
  width  : float;
  height : float;
  value  : int;
  color  : Graphics.color;
}

type box = { 
  xmin : float; 
  ymin : float; 
  xmax : float; 
  ymax : float 
}

type bricks =
  | Empty
  | Leaf of brick list
  | Node of box * bricks * bricks * bricks * bricks


val in_box : brick -> box -> bool


val remove_brick : brick -> bricks -> bricks

val build_tree : box -> brick list -> bricks
