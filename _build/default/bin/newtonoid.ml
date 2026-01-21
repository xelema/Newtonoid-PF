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
let rec draw_bricks = function
  | Brick.Empty -> ()
  | Brick.Leaf l -> List.iter draw_brick l
  | Brick.Node (_, nw, ne, sw, se) ->
      draw_bricks nw; draw_bricks ne; draw_bricks sw; draw_bricks se

let draw_state (etat, barre) =
  (* Dessiner les briques *)
  draw_bricks etat.bricks;
  (* Dessiner la barre *)
  let (xmin, xmax, ymin, ymax, c) = barre in
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect 
    (int_of_float xmin) 
    (int_of_float ymin) 
    (int_of_float (xmax -. xmin)) 
    (int_of_float (ymax -. ymin));
  (* Dessiner la balle *)
  let (x, y) = etat.ball_pos in
  let int_x = int_of_float x in
  let int_y = int_of_float y in
  Graphics.set_color Graphics.black;
  Graphics.fill_circle int_x int_y 10;
  (* Afficher le score *)
  Graphics.moveto 10 5;
  Graphics.draw_string (Format.sprintf "Score: %d" etat.score);
  Graphics.moveto 10 20;
  Graphics.draw_string (Format.sprintf "Vies: %d" etat.vies)


let draw flux_etat =
  let rec loop flux_etat =
    match Flux.(uncons flux_etat) with
    | None -> 
        Graphics.set_color Graphics.red;
        Graphics.moveto 350 300;
        Graphics.draw_string "GAME OVER";
        Graphics.synchronize ();
        Unix.sleep 2
    | Some ((etat, barre), flux_etat') ->
        Graphics.clear_graph ();
        draw_state (etat, barre);
        Graphics.synchronize ();
        Unix.sleepf Init.dt;
        loop flux_etat'
  in
  loop flux_etat

let () = 
  Graphics.open_graph " 800x600";

  let dt = Init.dt in
  let game_box = { 
    Brick.xmin = Box.infx; 
    Brick.xmax = Box.supx; 
    Brick.ymin = 0.0; (* On part du bas pour inclure toute la zone de vol *)
    Brick.ymax = Box.supy 
  } in
  let liste_briques_initiale = Layout.classic () in
  let init : Game.etat = {
    ball_pos = (400., 300.);
    ball_vel = (0., 500.);
    bricks = liste_briques_initiale;
    score = 0;
    vies = 3;
  } in
  let limites_cadre = (Box.infx, Box.supx) in
  let flux_barre = Barreau.flux_barre (Input.mouse ()) limites_cadre in

  let flux_jeu = Game.run dt init flux_barre in
  draw flux_jeu
