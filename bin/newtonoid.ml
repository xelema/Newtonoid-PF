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
  Graphics.moveto 10 5;
  Graphics.draw_string (Format.sprintf "Score: %d  Vies: %d" etat.score etat.lives)

(* Attendre un clic souris *)
let wait_for_click () =
  (* Attendre que le bouton soit appuye *)
  while not (Graphics.button_down ()) do
    Unix.sleepf 0.01
  done;
  (* Attendre que le bouton soit relache *)
  while Graphics.button_down () do
    Unix.sleepf 0.01
  done

(* Afficher l'ecran Game Over *)
let draw_game_over score =
  Graphics.clear_graph ();
  Graphics.set_color Graphics.black;
  Graphics.moveto 320 320;
  Graphics.draw_string "GAME OVER";
  Graphics.moveto 300 280;
  Graphics.draw_string (Format.sprintf "Score final: %d" score);
  Graphics.moveto 260 220;
  Graphics.draw_string "R = Recommencer | Q = Quitter";
  Graphics.synchronize ()

let draw_start_message etat message =
  Graphics.clear_graph ();
  draw_state etat;
  Graphics.set_color Graphics.blue;
  Graphics.moveto 280 300;
  Graphics.draw_string message;
  Graphics.synchronize ()

let make_init_state () : Game.etat = {
  ball_pos = (400., 100.);
  ball_vel = (0., 0.);
  bricks = Layout.classic ();
  score = 0;
  lives = 3;
}

let rec main_loop () =
  let init = make_init_state () in
  
  draw_start_message init "Cliquez pour commencer";
  wait_for_click ();
  
  let init_moving = { init with ball_vel = (250., 200.) } in
  
  let rec game_loop etat flux =
    match Flux.uncons flux with
    | None -> (* Flux termine = vie perdue *)
      etat
    | Some (e, flux') ->
      Graphics.clear_graph ();
      draw_state e;
      Graphics.synchronize ();
      Unix.sleepf Init.dt;
      game_loop e flux'
  in
  
  let rec lives_loop etat =
    let flux = Game.run Init.dt etat in
    let final_etat = game_loop etat flux in
    
    if final_etat.lives <= 0 then
      final_etat.score
    else begin
      draw_start_message final_etat "Cliquez pour relancer";
      wait_for_click ();
      let respawned = { final_etat with ball_vel = (250., 200.) } in
      lives_loop respawned
    end
  in
  
  let final_score = lives_loop init_moving in
  
  draw_game_over final_score;
  
  let rec wait_key () =
    let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
    match ev.Graphics.key with
    | 'r' | 'R' -> main_loop ()
    | 'q' | 'Q' -> ()
    | _ -> wait_key ()
  in
  wait_key ()

let () =
  Graphics.open_graph graphic_format;
  Graphics.auto_synchronize false;
  main_loop ();
  Graphics.close_graph ()
