
include StructuresSig


type priority =
  | VeryLow
  | Low
  | Medium
  | High
  | VeryHigh

module Priority : Set.OrderedType with type t = priority

(* Add a type of priorities to a given directed set: higher priorities always take precedence. *)
module AddPriority (P : Set.OrderedType) (S : DirectedSet)
  : DirectedSet with type t = P.t * S.t

module DirectedSetFromOrder (S : Set.OrderedType)
  : DirectedSet with type t = S.t

module DirectedSetProduct (S1 : DirectedSet) (S2 : DirectedSet)
  : DirectedSet with type t = S1.t * S2.t

module TopDirectedSet (S : sig type t val top : t end)
  : DirectedSet with type t = S.t

