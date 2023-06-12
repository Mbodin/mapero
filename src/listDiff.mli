
module Make (O : Set.OrderedType) : sig
(* A space storing the difference between the elements of lists storing O.t.
  Its implementation is imperative. *)

(* An identifier for each stored list. *)
type id

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

(* The difference between two lists. *)
val diff : id -> id -> id

end

