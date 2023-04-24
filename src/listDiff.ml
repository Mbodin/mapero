
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

(* The first free identifier. *)
let free_id = ref 0

(* Add a set into the maps sets and ids.
  This is the only function actually changing these here. *)
let add_set s =
  let id = !free_id in
  incr free_id ;
  sets := IMap.add id s !sets ;
  ids := SMap.add s id !ids ;
  id

(* Given a list, retrieve its id or create a new one. *)
let add l =
  let s = S.of_list l in
  match SMap.find_opt s !ids with
  | Some id -> id
  | None -> add_set s

(* Return the set associated to an id. *)
let get id =
  match IMap.find_opt id1 !sets with
  | None ->
    (* No id can be created outside this file because of the module abstraction. *)
    assert false
  | Some s -> s

let union id1 id2 =
    add (S.union (get id1) (get id2))

let inter id1 id2 =
    add (S.inter (get id1) (get id2))

(* The goal of this module is to memoise the result of the diff function:
  here comes the associated store. *)
let store = ref IMap2.empty

let diff id1 id2 =
  match IMap2.find_opt (id1, id2) !store with
  | Some r -> r
  | None ->
    let r =
      let s1, s2 = get id1, get id2 in
      let removed = S.diff s1 s2 in
      let added = S.diff s2 s1 in
      let to_list s = List.of_seq (S.to_seq s) in
      (to_list removed, to_list added) in
    store := IMap2.add (id1, id2) r !store ;
    r

end

