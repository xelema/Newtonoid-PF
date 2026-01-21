open Iterator

(*Contour de la fenêtre*)

let x_min, x_max = 10.0, 790.0
let y_min, y_max = 10.0, 590.0

let contact_boite ((x,y), (vx, vy)) =
  (x < x_min && vx < 0.0)|| (x > x_max && vx > 0.0)||
  (y < y_min && vy < 0.0)|| (y > y_max && vy > 0.0)

let rebond_boite ((x, y), (vx, vy)) =
  let nv_x = if x < x_min then x_min else if x > x_max then x_max else x in
  let nv_y = if y < y_min then y_min else if y > y_max then y_max else y in
  let nv_vx = if (x < x_min && vx < 0.0) || (x > x_max && vx > 0.0) then -.vx else vx in
  let nv_vy = if (y > y_max && vy > 0.0) then -.vy else vy in
  ((nv_x, nv_y), (nv_vx, nv_vy))

let contact((x,y), (vx, vy)) rayon (xmin, xmax, ymin, ymax) =
  ((x +. rayon >= xmin && x -. rayon <= xmax) && (y +. rayon >= ymin && y -. rayon <= ymax))
  
let rebond ((x, y), (vx, vy)) (xmin, xmax, ymin, ymax) =
  let nv_vx = if x < xmin || x > xmax then -.vx else vx in
  let nv_vy = if y < ymin || y > ymax then -.vy else vy in
  ((x, y), (nv_vx, nv_vy))



(* Collision cercle (balle) avec AABB (brique) *)
let circle_aabb_contact (cx, cy) r box =
  let closest_x = max box.Brick.xmin (min cx box.xmax) in
  let closest_y = max box.ymin (min cy box.ymax) in
  let dx = cx -. closest_x in
  let dy = cy -. closest_y in
  (dx *. dx) +. (dy *. dy) <= (r *. r)

(* Trouver la première brique en collision *)
let rec find_colliding_brick (pos, vel) tree radius =
  match tree with
  | Brick.Empty -> None
  | Brick.Leaf l ->
    let (cx, cy) = pos in
    List.find_opt (fun (b : Brick.brick) ->
      (* On crée une box à la volée pour correspondre à la brique *)
      let b_box = { Brick.xmin = b.x; Brick.xmax = b.x +. b.width; 
                    Brick.ymin = b.y; Brick.ymax = b.y +. b.height } in
      circle_aabb_contact (cx, cy) radius b_box
    ) l
  | Brick.Node (box, nw, ne, sw, se) ->
      if circle_aabb_contact pos radius box then
        match find_colliding_brick (pos, vel) nw radius with
        | Some b -> Some b
        | None -> match find_colliding_brick (pos, vel) ne radius with
        | Some b -> Some b
        | None -> match find_colliding_brick (pos, vel) sw radius with
        | Some b -> Some b
        | None -> find_colliding_brick (pos, vel) se radius
      else None

(* Calculer le rebond sur une brique *)
let rebond_brick ((cx, cy), (vx, vy)) (brick : Brick.brick) =
  (* Point le plus proche sur la brique *)
  let closest_x = max brick.x (min cx (brick.x +. brick.width)) in
  let closest_y = max brick.y (min cy (brick.y +. brick.height)) in

  (* Vecteur de collision *)
  let dx = cx -. closest_x in
  let dy = cy -. closest_y in

  (* Déterminer le côté de collision *)
  let (nv_vx, nv_vy) =
    if abs_float dx > abs_float dy then
      (* Collision horizontale *)
      (-.vx, vy)
    else
      (* Collision verticale *)
      (vx, -.vy)
  in
  ((cx, cy), (nv_vx, nv_vy))

(*rebond de la barre différent pour l'angle x de rebond*)
let rebond_barre ((x, y), (vx, vy)) (xmin, xmax, ymin, ymax, centre) =
  let largeur = xmax -. xmin in
  let diff = x -. centre in 


  let demi_largeur = largeur /. 2. in
  (*pour pas de pb de collision*)
  let diff = max (-.demi_largeur) (min demi_largeur diff) in
  
  let proportion = diff /. demi_largeur in 
  
  (*pas d'angle plat*)
  let max_angle = 1.2 in 
  let angle = proportion *. max_angle in
  
  let vitesse = sqrt (vx *. vx +. vy *. vy) in
  
  let nv_vx = vitesse *. sin angle in
  let nv_vy = vitesse *. cos angle in
  
  ((x, y), (nv_vx, abs_float nv_vy))
