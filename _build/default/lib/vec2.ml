(* Vec2 - Vecteur 2D pour positions et vitesses *)

type t = { x : float; y : float }

(* Constructeurs *)
let create x y = { x; y }
let zero = { x = 0.; y = 0. }
let of_tuple (x, y) = { x; y }
let to_tuple v = (v.x, v.y)

(* Operations arithmetiques *)
let add v1 v2 = { x = v1.x +. v2.x; y = v1.y +. v2.y }
let sub v1 v2 = { x = v1.x -. v2.x; y = v1.y -. v2.y }
let scale s v = { x = s *. v.x; y = s *. v.y }
let neg v = { x = -.v.x; y = -.v.y }

(* Operations geometriques *)
let dot v1 v2 = v1.x *. v2.x +. v1.y *. v2.y
let magnitude_squared v = dot v v
let magnitude v = sqrt (magnitude_squared v)

let normalize v =
  let m = magnitude v in
  if m = 0. then zero else scale (1. /. m) v

let distance v1 v2 = magnitude (sub v1 v2)

(* Utilitaires *)
let clamp_magnitude max_mag v =
  let m = magnitude v in
  if m > max_mag then scale (max_mag /. m) v else v

let lerp t v1 v2 = add (scale (1. -. t) v1) (scale t v2)

(* Tests unitaires *)
let%test "create" = 
  let v = create 3. 4. in 
  v.x = 3. && v.y = 4.

let%test "zero" = 
  zero.x = 0. && zero.y = 0.

let%test "of_tuple" = 
  let v = of_tuple (1., 2.) in 
  v.x = 1. && v.y = 2.

let%test "to_tuple" = 
  to_tuple (create 1. 2.) = (1., 2.)

let%test "add" = 
  let v = add (create 1. 2.) (create 3. 4.) in 
  v.x = 4. && v.y = 6.

let%test "sub" = 
  let v = sub (create 5. 7.) (create 2. 3.) in 
  v.x = 3. && v.y = 4.

let%test "scale" = 
  let v = scale 2. (create 3. 4.) in 
  v.x = 6. && v.y = 8.

let%test "neg" = 
  let v = neg (create 3. (-4.)) in 
  v.x = (-3.) && v.y = 4.

let%test "dot" = 
  dot (create 1. 2.) (create 3. 4.) = 11.

let%test "magnitude 3-4-5 triangle" = 
  Float.abs (magnitude (create 3. 4.) -. 5.) < 0.0001

let%test "magnitude_squared" = 
  magnitude_squared (create 3. 4.) = 25.

let%test "normalize" = 
  let v = normalize (create 3. 4.) in
  Float.abs (v.x -. 0.6) < 0.0001 && Float.abs (v.y -. 0.8) < 0.0001

let%test "normalize zero" = 
  let v = normalize zero in 
  v.x = 0. && v.y = 0.

let%test "distance" = 
  Float.abs (distance (create 0. 0.) (create 3. 4.) -. 5.) < 0.0001

let%test "clamp_magnitude under" = 
  let v = clamp_magnitude 10. (create 3. 4.) in
  v.x = 3. && v.y = 4.

let%test "clamp_magnitude over" = 
  let v = clamp_magnitude 5. (create 6. 8.) in
  Float.abs (magnitude v -. 5.) < 0.0001

let%test "lerp 0" = 
  let v = lerp 0. (create 0. 0.) (create 10. 10.) in
  v.x = 0. && v.y = 0.

let%test "lerp 1" = 
  let v = lerp 1. (create 0. 0.) (create 10. 10.) in
  v.x = 10. && v.y = 10.

let%test "lerp 0.5" = 
  let v = lerp 0.5 (create 0. 0.) (create 10. 10.) in
  v.x = 5. && v.y = 5.
