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

  (* perturbation aléatoire pour ne pas avoir de trajectoires monotones *)
  let perturbation = 0.1 in
  let rand_factor = (Random.float (2.0 *. perturbation)) -. perturbation in
  let speed = sqrt (nv_vx *. nv_vx +. nv_vy *. nv_vy) in
  let nv_vx_final = nv_vx +. (speed *. rand_factor) in
  ((cx, cy), (nv_vx_final, nv_vy))

(*rebond sur la barre *)
let rebond_barre ((x, y), (vx, vy)) barre_vel =
  (* Rebond simple: inverser la vitesse verticale *)
  let nv_vy = abs_float vy in

  (* Ajouter l'impulsion horizontale de la raquette *)
  let coefficient_impulsion = 0.5 in
  let nv_vx = vx +. (barre_vel *. coefficient_impulsion) in

  (* vitesse horizontale max pour garder un angle raisonnable *)
  let vx_max = 700.0 in
  let nv_vx = max (-.vx_max) (min vx_max nv_vx) in

  (* accélération progressive de 5% à chaque rebond sur la barre *)
  let acceleration_factor = 1.05 in
  let nv_vx_accel = nv_vx *. acceleration_factor in
  let nv_vy_accel = nv_vy *. acceleration_factor in

  (* vitesse max pour garder le jeu jouable *)
  let vitesse_max = 1200.0 in
  let speed = sqrt (nv_vx_accel *. nv_vx_accel +. nv_vy_accel *. nv_vy_accel) in
  let (final_vx, final_vy) = 
    if speed > vitesse_max then
      let ratio = vitesse_max /. speed in
      (nv_vx_accel *. ratio, nv_vy_accel *. ratio)
    else
      (nv_vx_accel, nv_vy_accel)
  in
  ((x, y), (final_vx, final_vy))
