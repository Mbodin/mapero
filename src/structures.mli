
open StructuresSig


(* Integers are ordered. *)
module IntOrder : Set.OrderedType with type t = int

(* Combine two ordered type using a product. *)
module ProductOrderedType (S1 : Set.OrderedType) (S2 : Set.OrderedType)
  : Set.OrderedType with type t = S1.t * S2.t


(* Some general levels of priority. *)
type priority =
  | VeryLow
  | Low
  | Medium
  | High
  | VeryHigh

(* Priorities are ordered. *)
module Priority : Set.OrderedType with type t = priority

(* Add a type of priorities to a given directed set: higher priorities always take precedence. *)
module AddPriority (P : Set.OrderedType) (S : DirectedSet)
  : DirectedSet with type t = P.t * S.t

(* If a type is equiped with an order, there is a natural direction to it. *)
module DirectedSetFromOrder (S : Set.OrderedType)
  : DirectedSet with type t = S.t

(* We can combine two directions with the product. *)
module DirectedSetProduct (S1 : DirectedSet) (S2 : DirectedSet)
  : DirectedSet with type t = S1.t * S2.t

(* Adding a top element to a set as a unique ordering. *)
module TopDirectedSet (S : sig type t val top : t end)
  : DirectedSet with type t = S.t

(* Adding a None element to a directed set. *)
module OptionDirectedSet (S : DirectedSet)
  : DirectedSet with type t = S.t option

