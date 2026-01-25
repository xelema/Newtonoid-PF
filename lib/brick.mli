(* Module des brick du jeu *)

(* Type de la brick avec tous ses parametres *)

type brick = {
  x      : float;
  y      : float;
  width  : float;
  height : float;
  value  : int;
  color  : Graphics.color;
}

(* Type des boites pour le quadtree *)

type box = { 
  xmin : float; 
  ymin : float; 
  xmax : float; 
  ymax : float 
}

(* Type des noeuds pour le quadtree *)

type bricks =
  | Empty
  | Leaf of brick list
  | Node of box * bricks * bricks * bricks * bricks

(*Regarde si une brique est dans une boite*)

val in_box : brick -> box -> bool

(*Enleve une brique d'un arbre de briques*)

val remove_brick : brick -> bricks -> bricks

(*Construit un quadtree a partir d'une liste de briques*)

val build_tree : box -> brick list -> bricks

(*Teste si un arbre de briques est vide*)

val is_empty : bricks -> bool
