
(* In order to display different data in the same place, we need some way of hierarchying
  all kinds of information.  We could do it with a monoid, but I feel that directed sets
  provide a better way to think these kinds of relations. *)
module type DirectedSet = sig

  type t

  val join : t -> t -> t

end

(* To deal with information, we introduce a lattice structure. *)
module type Lattice = sig

  type t

  val join : t -> t -> t
  val merge : t -> t -> t

end

