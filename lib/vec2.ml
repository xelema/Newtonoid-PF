(* vecteur 2d pour positions et vitesses *)

type t = { x : float; y : float }

(* Constructeurs *)
let create x y = { x; y }
let zero = { x = 0.; y = 0. }
let of_tuple (x, y) = { x; y }
let to_tuple v = (v.x, v.y)


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
