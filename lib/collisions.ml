(* Contour de la fenetre *)
let x_min = Config.default_bounds.x_min
let x_max = Config.default_bounds.x_max
let y_min = Config.default_bounds.y_min
let y_max = Config.default_bounds.y_max

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
      (* On crée une box directement ici pour correspondre à la brique *)
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

  (* Déterminer le coté de collision *)
  let (nv_vx, nv_vy) =
    if abs_float dx > abs_float dy then
      (* Collision horizontale *)
      (-.vx, vy)
    else
      (* Collision verticale *)
      (vx, -.vy)
  in

  (* perturbation aleatoire pour eviter les trajectoires monotones *)
  let perturbation = Config.default_physics.perturbation in
  let rand_factor = (Random.float (2.0 *. perturbation)) -. perturbation in
  let speed = sqrt (nv_vx *. nv_vx +. nv_vy *. nv_vy) in
  let nv_vx_final = nv_vx +. (speed *. rand_factor) in
  ((cx, cy), (nv_vx_final, nv_vy))

(*rebond sur la barre *)
let rebond_barre ((x, y), (vx, vy)) barre_vel =
  (* Rebond simple: inverser la vitesse verticale *)
  let nv_vy = abs_float vy in

  (* Ajouter l'impulsion horizontale de la raquette *)
  let coefficient_impulsion = Config.default_physics.impulse_coefficient in
  let nv_vx = vx +. (barre_vel *. coefficient_impulsion) in

  (* vitesse horizontale max pour garder un angle raisonnable *)
  let vx_max = Config.default_physics.vx_max in
  let nv_vx = max (-.vx_max) (min vx_max nv_vx) in

  (* acceleration progressive a chaque rebond sur la barre *)
  let acceleration_factor = Config.default_physics.acceleration_factor in
  let nv_vx_accel = nv_vx *. acceleration_factor in
  let nv_vy_accel = nv_vy *. acceleration_factor in

  (* vitesse max pour garder le jeu jouable *)
  let vitesse_max = Config.default_physics.max_speed in
  let speed = sqrt (nv_vx_accel *. nv_vx_accel +. nv_vy_accel *. nv_vy_accel) in
  let (final_vx, final_vy) = 
    if speed > vitesse_max then
      let ratio = vitesse_max /. speed in
      (nv_vx_accel *. ratio, nv_vy_accel *. ratio)
    else
      (nv_vx_accel, nv_vy_accel)
  in
  ((x, y), (final_vx, final_vy))

(* Tests contact_boite *)
let%test "contact_boite pas de contact interne" =
  not (contact_boite ((400., 300.), (100., 100.)))

let%test "contact_boite mur gauche" =
  contact_boite ((5., 300.), (-100., 0.))

let%test "contact_boite mur gauche 2" =
  not (contact_boite ((5., 300.), (100., 0.)))

let%test "contact_boite mur droit" =
  contact_boite ((795., 300.), (100., 0.))

let%test "contact_boite mur haut" =
  contact_boite ((400., 595.), (0., 100.))

(* Tests rebond_boite *)
let%test "rebond_boite mur gauche" =
  let ((nx, _), (nvx, _)) = rebond_boite ((5., 300.), (-100., 50.)) in
  nx >= x_min && nvx > 0.

let%test "rebond_boite mur droit" =
  let ((nx, _), (nvx, _)) = rebond_boite ((795., 300.), (100., 50.)) in
  nx <= x_max && nvx < 0.

let%test "rebond_boite mur haut" =
  let ((_, ny), (_, nvy)) = rebond_boite ((400., 595.), (50., 100.)) in
  ny <= y_max && nvy < 0.

let%test "rebond_boite pas de changement à l'intérieur" =
  let ((x, y), (vx, vy)) = rebond_boite ((400., 300.), (100., 100.)) in
  x = 400. && y = 300. && vx = 100. && vy = 100.

(* Tests contact *)
let%test "contact dedans rectangle" =
  contact ((50., 50.), (0., 0.)) 10. (40., 60., 40., 60.)

let%test "contact dehors rectangle" =
  not (contact ((100., 100.), (0., 0.)) 10. (40., 60., 40., 60.))

let%test "contact bordure rectangle" =
  contact ((35., 50.), (0., 0.)) 10. (40., 60., 40., 60.)

(* Tests circle_aabb_contact *)
let%test "circle_aabb dedans" =
  let box = { Brick.xmin = 40.; xmax = 60.; ymin = 40.; ymax = 60. } in
  circle_aabb_contact (50., 50.) 5. box

let%test "circle_aabb dehors" =
  let box = { Brick.xmin = 40.; xmax = 60.; ymin = 40.; ymax = 60. } in
  not (circle_aabb_contact (100., 100.) 5. box)

let%test "circle_aabb bordure" =
  let box = { Brick.xmin = 40.; xmax = 60.; ymin = 40.; ymax = 60. } in
  circle_aabb_contact (35., 50.) 6. box

let%test "circle_aabb corner" =
  let box = { Brick.xmin = 40.; xmax = 60.; ymin = 40.; ymax = 60. } in
  circle_aabb_contact (35., 35.) 8. box

(* Tests rebond_barre *)
let%test "rebond_barre inverse vy" =
  let ((_, _), (_, nvy)) = rebond_barre ((400., 50.), (100., -200.)) 0. in
  nvy > 0.

let%test "rebond_barre ajoute impulsion" =
  let ((_, _), (nvx1, _)) = rebond_barre ((400., 50.), (100., -200.)) 0. in
  let ((_, _), (nvx2, _)) = rebond_barre ((400., 50.), (100., -200.)) 500. in
  nvx2 > nvx1

let%test "rebond_barre ne dépasse pas la vitesse max" =
  let ((_, _), (vx, vy)) = rebond_barre ((400., 50.), (1000., -1000.)) 1000. in
  let speed = sqrt (vx *. vx +. vy *. vy) in
  speed <= 1200.0 +. 0.001
