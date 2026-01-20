(* ouvre la bibliotheque de modules definis dans lib/ *)
open Libnewtonoid
open Iterator

(* exemple d'ouvertue d'un tel module de la bibliotheque : *)
open Game

module Init = struct
  let dt = 1. /. 60. (* 60 Hz *)
end

module Box = struct
  let marge = 10.
  let infx = 10.
  let infy = 10.
  let supx = 790.
  let supy = 590.
end

let graphic_format =
  Format.sprintf
    " %dx%d+50+50"
    (int_of_float ((2. *. Box.marge) +. Box.supx -. Box.infx))
    (int_of_float ((2. *. Box.marge) +. Box.supy -. Box.infy))

(* Dessiner une brique *)
let draw_brick (brick : Brick.brick) =
  Graphics.set_color brick.color;
  Graphics.fill_rect
    (int_of_float brick.x)
    (int_of_float brick.y)
    (int_of_float brick.width)
    (int_of_float brick.height);
  Graphics.set_color Graphics.black;
  Graphics.draw_rect
    (int_of_float brick.x)
    (int_of_float brick.y)
    (int_of_float brick.width)
    (int_of_float brick.height)

(* Dessiner toutes les briques *)
let draw_bricks bricks =
  List.iter draw_brick bricks

let draw_state etat =
  (* Dessiner les briques *)
  draw_bricks etat.bricks;
  (* Dessiner la balle *)
  let (x, y) = etat.ball_pos in
  let int_x = int_of_float x in
  let int_y = int_of_float y in
  Graphics.set_color Graphics.black;
  Graphics.fill_circle int_x int_y 10;
  (* Afficher le score *)
  Graphics.moveto 10 5;
  Graphics.draw_string (Format.sprintf "Score: %d" etat.score)

let draw flux_etat =
  let rec loop flux_etat last_score =
    match Flux.(uncons flux_etat) with
    | None -> last_score
    | Some (etat, flux_etat') ->
      Graphics.clear_graph ();
      draw_state etat;
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      loop flux_etat' etat.score
  in
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  let score = loop flux_etat 0 in
  Format.printf "Score final : %d@\n" score;
  Graphics.close_graph ()

let () =
  let dt = Init.dt in
  let init : Game.etat = {
    ball_pos = (400., 300.);
    ball_vel = (250., 200.);
    bricks = Layout.classic ();
    score = 0;
  } in
  let flux_jeu = Game.run dt init in
  draw flux_jeu
