
open StructuresSig


module IntOrder = struct
  type t = int
  let compare : t -> t -> int = compare
end

module ProductOrderedType (S1 : Set.OrderedType) (S2 : Set.OrderedType) = struct
  type t = S1.t * S2.t
  let compare (a1, a2) (b1, b2) =
    match S1.compare a1 b1 with
    | 0 -> S2.compare a2 b2
    | v -> v
end

type priority =
  | VeryLow
  | Low
  | Medium
  | High
  | VeryHigh

module Priority = struct

  type t = priority

  let to_number = function
    | VeryLow -> -2
    | Low -> -1
    | Medium -> 0
    | High -> 1
    | VeryHigh -> 2

  let compare p1 p2 = to_number p1 - to_number p2

end


module AddPriority (P : Set.OrderedType) (S : DirectedSet) = struct

  type t = P.t * S.t

  let join (p1, e1) (p2, e2) =
    let c = P.compare p1 p2 in
    if c < 0 then (p2, e2)
    else if c > 0 then (p1, e1)
    else (* c = 0, p1 = p2 *) (p1, S.join e1 e2)

end


module DirectedSetFromOrder (S : Set.OrderedType) = struct

  include S

  let join e1 e2 =
    let c = S.compare e1 e2 in
    if c < 0 then e2
    else if c > 0 then e1
    else (* c = 0, e1 = e2 *) e1

end

module DirectedSetProduct (S1 : DirectedSet) (S2 : DirectedSet) = struct

  type t = S1.t * S2.t

  let join (a1, a2) (b1, b2) = (S1.join a1 b1, S2.join a2 b2)

end

module TopDirectedSet (S : sig type t val top : t end) = struct

  include S

  let join a b =
    if a = b then a
    else top

end

module OptionDirectedSet (S : DirectedSet) = struct
  
  type t = S.t option

  let join a b =
    match a, b with
    | None, o | o, None -> o
    | Some a, Some b -> Some (S.join a b)

end


