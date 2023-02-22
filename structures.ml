
open StructuresSig


type priority =
  | Low
  | Medium
  | High

let priority_lt p1 p2 =
  match p1, p2 with
  | Low, (Medium | High) -> true
  | Medium, High -> true
  | _, _ -> false


module addPriority (S : DirectedSet) = struct

  type t = S.t * priority

  let join (e1, p1) (e2, p2) =
    if priority_lt p1 p2 then (e2, p2)
    else if priority_lt p2 p1 then (e1, p1)
    else (
      assert (p1 = p2) ;
      (S.join e1 e2, p1)
    )

end

