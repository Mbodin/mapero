
open StructuresSig


type priority =
  | Low
  | Medium
  | High

val priority_lt : priority -> priority -> bool


module addPriority (S : DirectedSet)
  : DirectedSet with type t = S.t * priority

