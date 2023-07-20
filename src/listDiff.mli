
module Make (O : Set.OrderedType) : sig
(* A space storing the difference between the elements of lists storing O.t.
  Its implementation is imperative. *)

(* An identifier for each stored list. *)
type id

(* As the convention goes, we also name it t. *)
type t = id

(* Equality between two identifiers.
  It is exactly the usual [=] equality, but we include it as a compatibility to ExtLattice. *)
val eq : id -> id -> bool

(* Add a list to the store (if not already in it), and return its id. *)
val add : O.t list -> id

(* Get the elements of a stored list.
  The order is unspecified. *)
val to_list : id -> O.t list

module S : Set.S with type elt = O.t

(* Get the elements of a stored list as a set. *)
val to_set : id -> S.t

(* Return the union of two lists. *)
val union : id -> id -> id

(* Return the intersection of two lists. *)
val inter : id -> id -> id

(* All the elements that are in the first list that are not in the second list,
  in other words, the difference between two lists. *)
val diff : id -> id -> id

(* Whether the first list is fully included in the second.
  In other words, whether its difference is empty. *)
val le : id -> id -> bool

end

