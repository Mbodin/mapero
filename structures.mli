
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
  : DirectedSet with type t = S.t * P.t

module DirectedSetFromOrder (S : Set.OrderedType) : DirectedSet

