type brick = {
  x : float; y : float;
  width : float; height : float;
  value : int; color : Graphics.color;
}

type box = { xmin: float; ymin: float; xmax: float; ymax: float }

type bricks =
  | Empty
  | Leaf of brick list
  | Node of box * bricks * bricks * bricks * bricks

(* Fonction utilitaire pour savoir si une brique est dans une zone *)
let in_box b box =
  b.x < box.xmax && b.x +. b.width > box.xmin &&
  b.y < box.ymax && b.y +. b.height > box.ymin

(* Suppression d'une brique dans l'arbre *)
let rec remove_brick target = function
  | Empty -> Empty
  | Leaf l -> Leaf (List.filter (fun b -> b != target) l)
  | Node (box, nw, ne, sw, se) ->
      Node (box, remove_brick target nw, remove_brick target ne, 
                 remove_brick target sw, remove_brick target se)

(* Verifie si toutes les briques ont ete cassees *)
let rec is_empty = function
  | Empty -> true
  | Leaf [] -> true
  | Leaf _ -> false
  | Node (_, nw, ne, sw, se) -> 
      is_empty nw && is_empty ne && is_empty sw && is_empty se

let rec build_tree box list_bricks =
  (* Condition d'arrêt : si peu de briques, on fait une feuille *)
  if List.length list_bricks <= 4 then
    if list_bricks = [] then Empty else Leaf list_bricks
  else
    (* Calcul du centre de la zone pour diviser en 4 *)
    let mid_x = (box.xmin +. box.xmax) /. 2.0 in
    let mid_y = (box.ymin +. box.ymax) /. 2.0 in

    (* Définition des 4 zones *)
    let box_nw = { xmin = box.xmin; xmax = mid_x;   ymin = mid_y;    ymax = box.ymax } in
    let box_ne = { xmin = mid_x;    xmax = box.xmax; ymin = mid_y;    ymax = box.ymax } in
    let box_sw = { xmin = box.xmin; xmax = mid_x;   ymin = box.ymin; ymax = mid_y } in
    let box_se = { xmin = mid_x;    xmax = box.xmax; ymin = box.ymin; ymax = mid_y } in

    (* Répartition des briques dans chaque zone *)
    (* Une brique peut être dans plusieurs zones si elle chevauche une limite *)
    let nw_bricks = List.filter (fun b -> in_box b box_nw) list_bricks in
    let ne_bricks = List.filter (fun b -> in_box b box_ne) list_bricks in
    let sw_bricks = List.filter (fun b -> in_box b box_sw) list_bricks in
    let se_bricks = List.filter (fun b -> in_box b box_se) list_bricks in

    Node (box, 
          build_tree box_nw nw_bricks,
          build_tree box_ne ne_bricks,
          build_tree box_sw sw_bricks,
          build_tree box_se se_bricks)