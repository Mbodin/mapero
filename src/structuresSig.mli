
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

  (* Union and intersection. *)
  val union : t -> t -> t
  val inter : t -> t -> t

  (* Bottom element. *)
  val bot : t

end

(* A lattice with more information. *)
module type ExtLattice = sig

  include Lattice

  (* (Partial) order relation. *)
  val le : t -> t -> bool

  (* Equality test. *)
  val eq : t -> t -> bool

  (* Compute the smallest value whose union with the second argument is greater
    than the first argument. *)
  val diff : t -> t -> t

end

