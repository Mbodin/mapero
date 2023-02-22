
include StructuresSig


type priority =
  | VeryLow
  | Low
  | Medium
  | High
  | VeryHigh

module Priority = struct

  let to_number = function
    | VeryLow -> -2
    | Low -> -1
    | Medium -> 0
    | High -> 1
    | VeryHigh -> 2

  let compare p1 p2 = to_number p1 - to_number p2

end


module AddPriority (P : Set.OrderedType) (S : DirectedSet) = struct

  type t = S.t * priority

  let join (e1, p1) (e2, p2) =
    let c = P.compare p1 p2 in
    if c < 0 then (e2, p2)
    else if c > 0 then (e1, p1)
    else (* c = 0 *) (S.join e1 e2, p1)

end


module DirectedSetFromOrder (S : Set.OrderedType) = struct

  include S

  let join e1 e2 =
    let c = P.compare e1 e2 in
    if c < 0 then e2
    else if c > 0 then e1
    else (* c = 0 *) e1

end

