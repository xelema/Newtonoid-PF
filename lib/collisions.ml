open Iterator

(*Contour de la fenêtre*)

let x_min, x_max = 10.0, 790.0
let y_min, y_max = 10.0, 590.0

let contact_boite ((x,y), (vx, vy)) =
  (x < x_min && vx < 0.0)|| (x > x_max && vx > 0.0)||
  (y < y_min && vy < 0.0)|| (y > y_max && vy > 0.0)

let rebond_boite ((x, y), (vx, vy)) =
  let nv_vx = if (x < x_min && vx < 0.0) || (x > x_max && vx > 0.0) then -.vx else vx in
  let nv_vy = if (y < y_min && vy < 0.0) || (y > y_max && vy > 0.0) then -.vy else vy in
  ((x, y), (nv_vx, nv_vy))

let contact((x,y), (vx, vy)) (x_gauche,y_bas) (x_droite, y_haut) =
  (x > x_gauche && x < x_droite && y < y_haut && y > y_bas)











