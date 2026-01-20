open Iterator

(*Contour de la fenêtre*)

let x_min, x_max = 10.0, 790.0
let y_min, y_max = 10.0, 590.0

(* Contact avec les bords (haut, gauche, droite) - PAS le bas *)
let contact_boite ((x,y), (vx, vy)) =
  (x < x_min && vx < 0.0) || (x > x_max && vx > 0.0) ||
  (y > y_max && vy > 0.0)

(* Detecte si la balle est tombee sous le bord bas *)
let ball_fallen ((_x, y), _vel) = y < y_min

(* Rebond sur les bords (haut, gauche, droite) - PAS le bas *)
let rebond_boite ((x, y), (vx, vy)) =
  let nv_vx = if (x < x_min && vx < 0.0) || (x > x_max && vx > 0.0) then -.vx else vx in
  let nv_vy = if (y > y_max && vy > 0.0) then -.vy else vy in
  ((x, y), (nv_vx, nv_vy))

let contact ((x, y), (_vx, _vy)) (x_gauche, y_bas) (x_droite, y_haut) =
  x > x_gauche && x < x_droite && y < y_haut && y > y_bas

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



