(* Interface des iterateurs/flux *)
module type Intf = sig
  type 'a t

  val vide : 'a t
  val cons : 'a -> 'a t -> 'a t
  val unfold : ('s -> ('a * 's) option) -> 's -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val constant : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val uncons : 'a t -> ('a * 'a t) option
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val unless : 'a t -> ('a -> bool) -> ('a -> 'a t) -> 'a t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
end

type 'a flux = Tick of ('a * 'a flux) option Lazy.t

module Flux : Intf with type 'a t = 'a flux
