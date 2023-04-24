
module Make (O : Set.OrderedType) : sig
(* A space storing the difference between the elements of lists storing O.t.
  Its implementation is imperative. *)

(* An identifier for each stored list. *)
type id

(* Add a list to the store (if not already in it), and return its id. *)
val add : O.t list -> id

(* Given two id of lists, return two lists:
  - the list of elements that have been removed.
  - the list of elements that have been added.
  The order is unspecified. *)
val diff : id -> id -> O.t list * 'a list

(* Return the union of two lists. *)
val union : id -> id -> id

(* Return the intersection of two lists. *)
val inter : id -> id -> id

end

