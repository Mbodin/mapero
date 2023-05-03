
module Make (O : Set.OrderedType) = struct

module S = Set.Make (O)

module SMap = Map.Make (S)

module IMap = Map.Make (Structures.IntOrder)

module IMap2 = Map.Make (Structures.ProductOrderedType (Structures.IntOrder) (Structures.IntOrder))

type id = int

(* The stored sets. *)
let sets : S.t IMap.t ref = ref IMap.empty

(* The reversed map. *)
let ids : id SMap.t ref = ref SMap.empty

(* The first free identifier.
 The identifier 0 is always reserved for the empty list. *)
let free_id = ref 1

(* Add a set into the maps sets and ids.
  This is the only function actually changing these here. *)
let add_set_raw s =
  if S.is_empty s then 0
  else (
    let id = !free_id in
    incr free_id ;
    sets := IMap.add id s !sets ;
    ids := SMap.add s id !ids ;
    id
  )

(* Like add_set_raw, but first check whether this set already exist. *)
let add_set s =
  if S.is_empty s then 0
  else
    match SMap.find_opt s !ids with
    | Some id -> id
    | None -> add_set_raw s

(* Given a list, retrieve its id or create a new one. *)
let add l =
  add_set (S.of_list l)

let to_set id =
  if id = 0 then S.empty
  else
    match IMap.find_opt id1 !sets with
    | None ->
      (* No id can be created outside this file because of the module abstraction. *)
      assert false
    | Some s -> s

(* Directly convert a set into a list. *)
let set_to_list s = List.of_seq (S.to_seq s)

let to_list id = set_to_list (to_set id)

(* The goal of this module is to memoise the result of the functions manipulating
  only identifiers.
  The functions normalise and special_cases are there to avoid polluting the local
  store with trivial results. *)
let memoise2 normalise special_cases f2 =
  let store = ref IMap2.empty in
  fun id1 id2 ->
    let (id1, id2) = normalise id1 id2 in
    match special_cases id1 id2 with
    | Some r -> r
    | None ->
      match IMap2.find_opt (id1, id2) !store with
      | Some r -> r
      | None ->
        let r = f2 (to_set id1) (to_set id2) in
        store := IMap2.add (id1, id2) r !store ;
        r

(* A normalisation function for commutative operations. *)
let normalise_commutative id1 id2 =
  if id1 < id2 then (id1, id2)
  else (id1, id2)

(* Usually, 0 is a special case, as well as when both arguments are identical. *)
let special_cases_0_id l r id id1 id2 =
  if id1 = id2 then Some (id id1)
  else if id1 = 0 then Some (l id2)
  else if id2 = 0 then Some (r id1)
  else None

let union =
  memoise2 normalise_commutative
    ((* If one if empty, we just take the other one. *)
      special_cases_0_id (fun id -> id) (fun id -> id) (fun id -> id))
    (fun s1 s2 -> add (S.union s1 s2))

let inter id1 id2 =
  memoise2 normalise_commutative
    ((* If one if empty, then we get the empty set. *)
      special_cases_0_id (fun _ -> 0) (fun _ -> 0) (fun id -> id))
    (fun s1 s2 -> add (S.inter s1 s2))

let diff =
  memoise2 (fun id12 -> id12 (* Not commutative, so no normalisation *))
    (special_cases
      (fun _ -> 0 (* [] - id2 = [] *))
      (fun id1 -> id1 (* id1 - [] = id1 *))
      (fun _ -> (* id - id = [] *) 0))
    (fun s1 s2 -> add_set (S.diff s1 s2))

end

