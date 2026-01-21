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
  let nv_vy = if (y < y_min && vy < 0.0) || (y > y_max && vy > 0.0) then -.vy else vy in
  ((nv_x, nv_y), (nv_vx, nv_vy))

let contact((x,y), (vx, vy)) rayon (xmin, xmax, ymin, ymax) =
  ((x +. rayon >= xmin && x -. rayon <= xmax) && (y +. rayon >= ymin && y -. rayon <= ymax))
  
let rebond ((x, y), (vx, vy)) (xmin, xmax, ymin, ymax) =
  let nv_vx = if x < xmin || x > xmax then -.vx else vx in
  let nv_vy = if y < ymin || y > ymax then -.vy else vy in
  ((x, y), (nv_vx, nv_vy))



(* Collision cercle (balle) avec AABB (brique) *)
let circle_aabb_contact (cx, cy) radius bx by width height =
  (* Point le plus proche sur l'AABB au centre du cercle *)
  let closest_x = max bx (min cx (bx +. width)) in
  let closest_y = max by (min cy (by +. height)) in
  (* Distance au carré *)
  let dx = cx -. closest_x in
  let dy = cy -. closest_y in
  dx *. dx +. dy *. dy < radius *. radius

(* Trouver la première brique en collision *)
let find_colliding_brick (pos, _vel) bricks radius =
  let (cx, cy) = pos in
  List.find_opt (fun (b : Brick.brick) ->
    circle_aabb_contact (cx, cy) radius b.x b.y b.width b.height
  ) bricks

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
