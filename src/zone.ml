
(* A type to store spacially localised information.
  Information can be stored at the level of nodes (typically for non punctual data) and leaves.
  The choice of name is criticable: I associated horizontal with a x-division and vertical with
  an y-division (that is horizontal and vertical layouts, not a horizontal and vertical separation
  bar. *)
type ('node, 'leaf) tree =
  | Leaf of 'leaf
  | Horizontal of 'node * ('node, 'leaf) tree * float * ('node, 'leaf) tree (* Horizontal division (that is vertically splitted), with the middle x coordinate *)
  | Vertical of 'node * ('node, 'leaf) tree * float * ('node, 'leaf) tree (* Vertical division (that is, horizontally), with the middle y coordinate *)

(* We associate this spatial information with the external bounding box to get a precise zone. *)
type ('node, 'leaf) bbtree = Bbox.t * ('node, 'leaf) tree

type zone = (unit, bool) bbtree


(* A default bounded box, for initialisation. *)
let default_bbox = Bbox.from_points (0., 0.) (0., 0.)

let empty : zone = (default_bbox, Leaf false)

(* Get a zone from a bbox, filling up the zone. *)
let zone_of_bbox b : zone = (b, Leaf true)

(* Recognise a leaf. *)
let is_leaf = function
  | Leaf _ -> true
  | _ -> false

(* Recognise a node. *)
let is_node t = not (is_leaf t)


(* Restriction functions of a tree along a semi-line. *)

let rec restrict_gen fx fy = function
  | Leaf data -> Leaf data
  | Horizontal (data, t1, x, t2) ->
    let t1 = restrict_gen fx fy t1 in
    let t2 = restrict_gen fx fy t2 in
    fx data t1 x t2
  | Vertical (data, t1, y, t2) ->
    let t1 = restrict_gen fx fy t1 in
    let t2 = restrict_gen fx fy t2 in
    fy data t1 y t2

let restrict_before_x x0 =
  restrict_gen
    (fun data t1 x t2 -> if x >= x0 then t1 else Horizontal (data, t1, x, t2))
    (fun data t1 y t2 -> Vertical (data, t1, y, t2))

let restrict_after_x x0 =
  restrict_gen
    (fun data t1 x t2 -> if x <= x0 then t2 else Horizontal (data, t1, x, t2))
    (fun data t1 y t2 -> Vertical (data, t1, y, t2))

let restrict_before_y y0 =
  restrict_gen
    (fun data t1 x t2 -> Horizontal (data, t1, x, t2))
    (fun data t1 y t2 -> if y >= y0 then t1 else Horizontal (data, t1, y, t2))

let restrict_after_y y0 =
  restrict_gen
    (fun data t1 x t2 -> Horizontal (data, t1, x, t2))
    (fun data t1 y t2 -> if y <= y0 then t2 else Horizontal (data, t1, y, t2))


(* Negation of a zone (boolean) tree. *)
let rec negation = function
  | Leaf covered -> Leaf (not covered)
  | Horizontal ((), t1, x, t2) -> Horizontal ((), negation t1, x, negation t2)
  | Vertical ((), t1, y, t2) -> Vertical ((), negation t1, y, negation t2)

(* Intersection of two zone trees. *)
let rec intersection t = function
  | Leaf false -> Leaf false
  | Leaf true -> t
  | Horizontal ((), t1, x, t2) ->
    let t1 = intersection (restrict_before_x x t) t1 in
    let t2 = intersection (restrict_after_x x t) t2 in
    Horizontal ((), t1, x, t2)
  | Vertical ((), t1, y, t2) ->
    let t1 = intersection (restrict_before_y y t) t1 in
    let t2 = intersection (restrict_after_y y t) t2 in
    Vertical ((), t1, y, t2)

(* Union of two zone trees. *)
let rec union t = function
  | Leaf false -> t
  | Leaf true -> Leaf true
  | Horizontal ((), t1, x, t2) ->
    let t1 = union (restrict_before_x x t) t1 in
    let t2 = union (restrict_after_x x t) t2 in
    Horizontal ((), t1, x, t2)
  | Vertical ((), t1, y, t2) ->
    let t1 = union (restrict_before_y y t) t1 in
    let t2 = union (restrict_after_y y t) t2 in
    Vertical ((), t1, y, t2)

(* Split a list in two at the given index. *)
let split_list =
  let rec aux acc i = function
    | l when i = 0 -> (List.rev acc, l)
    | x :: l -> aux (x :: acc) (i - 1) l
    | [] ->
      assert (i > 0) ;
      assert false in
  aux []

(* A (costly) function to simplify bounded trees.
  Information at nodes may be applied to a larger area, loosing some information.
  In order to deal with the node reorganisation, a function to merge nodes and a neutral element
  are required (e.g., a monoid), as well a neutral element for leaves.
  Neutral elements are assumed to carry no information. *)
let simplify merge_node neutral_node neutral_leaf (bbox, t) =
  (* Create a balanced tree from a list.
    Each element of the list is a pair of its start coordinate and its associated tree.
    The list is assumed non-empty. *)
  let make_tree constr l =
    let rec aux s l =
      (* Invariant: List.length l = s > 0. *)
      assert (s > 0) ;
      if s = 1 then
        (match l with
         | [(x, t)] -> (x, t)
         | _ -> assert false)
      else
        let s' = s / 2 in
        let (l1, l2) = split_list s' l in
        constr (aux s' l1) (aux (s - s') l2)
      in
    snd (aux (List.length l) l) in
  (* CHeck whether two trees are identical, ignoring the local node data. *)
  let compare_trees t1 t2 =
    match t1, t2 with
    | Leaf _, Leaf _ -> t1 = t2
    | Horizontal (_, t1a, x1, t1b), Horizontal (_, t2a, x2, t2b) -> t1a = t2a && x1 = x2 && t1b = t2b
    | Vertical (_, t1a, y1, t1b), Vertical (_, t2a, y2, t2b) -> t1a = t2a && y1 = y2 && t1b = t2b
    | _, _ -> false in
  (* Assuming [compare_trees t1 t2], merge both trees with its local node data. *)
  let merge_trees t1 t2 =
    assert (compare_trees t1 t2) ;
    match t1, t2 with
    | Leaf _, Leaf _ -> t1
    | Horizontal (data1, t1a, x1, t1b), Horizontal (data2, _, _, _) ->
      Horizontal (merge_node data1 data2, t1a, x1, t1b)
    | Vertical (data1, t1a, y1, t1b), Vertical (data2, _, _, _) ->
      Vertical (merge_node data1 data2, t1a, y1, t1b)
    | _, _ -> assert false in
  (* Incorporate the provided data to a tree.
    The optional argument indicates a prefered mode (true for horizontal and false for vertical). *)
  let add_data ?(mode=true) data = function
    | Leaf d ->
      if data = neutral_node then
        (* If the provided data is neutral, we assume it's fine not to store it. *)
        Leaf d
      else
        (* The data is important: we add a dummy division. *)
        let (d, t1, x, t2) = (data, Leaf neutral_leaf, neg_infinity, Leaf d) in
        if mode then Horizontal (d, t1, x, t2) else Vertical (d, t1, x, t2)
    | Horizontal (data', t1, x, t2) -> Horizontal (merge_node data data', t1, x, t2)
    | Vertical (data', t1, y, t2) -> Vertical (merge_node data data', t1, y, t2) in
  (* Merge the duplicates in a list of pointed trees. *)
  let rec merge_duplicates = function
    | [] -> []
    | (x, t1) :: (_, t2) :: l when compare_trees t1 t2 ->
      merge_duplicates ((x, merge_trees t1 t2) :: l)
    | (x, t) :: l -> (x, t) :: merge_duplicates l in
  (* Return a horizontal list of subtrees, each associated with the x coordinate they start with.
    Also return the merge of all node data removed during the construction of the list. *)
  let rec list_horizontal bbox = function
    | Horizontal (data, t1, x, t2) ->
      (* We build a first tree, ignoring the inner data. *)
      let (l, data') =
      (* First, we check whether this horizontal division is meaningful. *)
        if x <= bbox.Bbox.min_x then list_horizontal bbox t2
        else if x >= bbox.Bbox.max_x then list_horizontal bbox t1
        else
          let (l1, data1) = list_horizontal {bbox with Bbox.max_x = x} t1 in
          let (l2, data2) = list_horizontal {bbox with Bbox.min_x = x} t2 in
          (merge_duplicates (l1 @ l2), merge_node data1 data2) in
      (l, merge_node data data')
    | t -> ([(bbox.Bbox.min_x, optimise_vertical bbox t)], neutral_node)
  (* Group all the horizontal elements into a balanced tree. *)
  and optimise_horizontal bbox t =
    let (l, data) = list_horizontal bbox t in
    let t =
      make_tree (fun (x1, t1) (x2, t2) ->
        (x1, Horizontal (neutral_node, t1, x2, t2))) l in
    (* We reverse the mode to help the information being caught at the next level if needed. *)
    add_data ~mode:false data t
  (* Same than list_horizontal, but vertically. *)
  and list_vertical bbox = function
    | Vertical (data, t1, y, t2) ->
      let (l, data') =
        if y <= bbox.Bbox.min_y then list_vertical bbox t2
        else if y >= bbox.Bbox.max_y then list_vertical bbox t1
        else
          let (l1, data1) = list_vertical {bbox with Bbox.max_y = y} t1 in
          let (l2, data2) = list_vertical {bbox with Bbox.min_y = y} t2 in
          (merge_duplicates (l1 @ l2), merge_node data1 data2) in
      (l, merge_node data data')
    | t -> ([(bbox.Bbox.min_y, optimise_horizontal bbox t)], neutral_node)
  (* Same than optimise_horizontal, but vertically. *)
  and optimise_vertical bbox t =
    let (l, data) = list_vertical bbox t in
    let t =
      make_tree (fun (y1, t1) (y2, t2) ->
        (y1, Vertical (neutral_node, t1, y2, t2))) l in
    add_data ~mode:true data t in
  optimise_horizontal bbox t

(* The simplify function can make node information less precise.
  This function aims at counter-balacing this effect.
  It can be applied when the node information includes a list of objects that
  can be identified spatially.
  It takes a function stating whether an object is within a bbox or not. *)
let rec specialise is_in_bbox (bbox, t) =
  (* Assuming that t is not a leaf, incorporate the provided data to t's data. *)
  let incorporate data = function
    | Leaf _ -> assert false
    | Horizontal ((state, data'), t1, x, t2) -> Horizontal ((state, data @ data'), t1, x, t2)
    | Vertical ((state, data'), t1, y, t2) -> Vertical ((state, data @ data'), t1, y, t2) in
  (* Extract from data the objects within the provided bbox and incorporate it into t. *)
  let aux data t bbox =
    let (bbox, t) = specialise is_in_bbox (bbox, t) in
    if is_leaf t then (data, t)
    else
      let (data_inner, data, data_size) =
        (* Similar to List.partition, but also computes the size of the second list.
          It also reverses the list, but we assume that the order is irrelevant here. *)
        let partition_size p l =
          List.fold_left (fun (yes, no, no_size) e ->
            if p e then
              (e :: yes, no, no_size)
            else (yes, e :: no, 1 + no_size)) ([], [], 0) l in
        partition_size (is_in_bbox bbox) data in
      ignore data_size (* TODO: if data_size is greater than a constant, then create new nodes
                         by finding a meaningful separation. *) ;
      (data, incorporate data_inner t) in
  match t with
  | Leaf _ -> (bbox, t)
  | Horizontal ((state, data), t1, x, t2) ->
    let (data, t1) = aux data t1 {bbox with Bbox.max_x = x} in
    let (data, t2) = aux data t2 {bbox with Bbox.min_x = x} in
    (bbox, Horizontal ((state, data), t1, x, t2))
  | Vertical ((state, data), t1, y, t2) ->
    let (data, t1) = aux data t1 {bbox with Bbox.max_y = y} in
    let (data, t2) = aux data t2 {bbox with Bbox.min_y = y} in
    (bbox, Vertical ((state, data), t1, y, t2))

(* Extend a zone to a larger bbox, without changing its meaning.
  It takes a neutral element for nodes and leaves. *)
let extend neutral_node neutral_leaf (b1, t) b2 =
  let open Bbox in
  let b2 = outer b1 b2 in
  let empty = Leaf neutral_leaf in
  let t =
    Horizontal (neutral_node, empty, b1.Bbox.min_x,
      Horizontal (neutral_node, t, b1.Bbox.max_x, empty)) in
  let t =
    Vertical (neutral_node, empty, b1.Bbox.min_y,
      Vertical (neutral_node, t, b1.Bbox.max_y, empty)) in
  (b2, t)

(* A specialised version of extend for zones. *)
let extend_zone : zone -> Bbox.t -> zone = extend () false

(* Add a bbox to a zone. *)
let add_bbox (z : zone) b2 =
  let (b, t) = extend_zone z b2 in
  let (b', t') = extend_zone (zone_of_bbox b2) b in
  assert (b = b') ;
  (b, union t t')

(* Convert a zone into a list of rectangles. *)
let to_bboxes ((bbox, t) : zone) =
  let rec aux acc bbox = function
    | Leaf false -> acc
    | Leaf true -> bbox :: acc
    | Horizontal ((), t1, x, t2) ->
      let acc = aux acc {bbox with Bbox.max_x = min bbox.Bbox.max_x x} t1 in
      let acc = aux acc {bbox with Bbox.min_x = max bbox.Bbox.min_x x} t2 in
      acc
    | Vertical ((), t1, y, t2) ->
      let acc = aux acc {bbox with Bbox.max_y = min bbox.Bbox.max_y y} t1 in
      let acc = aux acc {bbox with Bbox.min_y = max bbox.Bbox.min_y y} t2 in
      acc in
  aux [] bbox t

(* Compute the square of the distance between two rectangles rect1 and rect2.
  It is faster to compute and equivalent to the distance for sorting. *)
let distance rect1 rect2 =
  let (cx1, cy1) = Bbox.center rect1 in
  let (cx2, cy2) = Bbox.center rect2 in
  let square x = x *. x in
  square (cx1 -. cx2) +. square (cy1 -. cy2)

(* Insert an element in a sorted list. *)
let rec insert compare e = function
  | [] -> [e]
  | e1 :: l ->
    if compare e1 e < 0 then e1 :: insert compare e l
    else e :: e1 :: l

(* Given a list of rectangles (associated to a notion of knowledge), try to return a
  slightly different list covering the same area, and following the min and max dimensions.
  It needs a way to merge knowledge (on too small areas). *)
let fit_to_dimensions le_k merge_k maxwidth maxheight minwidth minheight rectangles =
  (* First, splitting the larger ones. *)
  let rectangles =
    List.concat_map (fun (rectangle, k) ->
      List.map (fun rect -> (rect, k))
        (Bbox.split rectangle maxwidth maxheight)) rectangles in
  (* At this point, rectangles are at most maxwidth by maxheight in dimension. *)
  (* Splitting all rectangles on whether their dimensions are fine or not. *)
  let (fine, too_small) =
    List.partition (fun (rectangle, _k) ->
      let (width, height) = Bbox.dimensions rectangle in
      width >= minwidth && height >= minheight) rectangles in
  (* What follows is more efficient with in-place replacments. *)
  let fine = Array.of_list fine in
  let can_be_merged rect1 rect2 =
    let rect' = Bbox.outer rect1 rect2 in
    let (x, y) = Bbox.dimensions rect' in
    x <= maxwidth && y <= maxheight in
  let (_, still_too_small) =
    (* Merging too small rectangles if possible.
      Note that the function has a side effect on the [fine] array. *)
    List.partition (fun (rect, k) ->
      (* Fetching the closest fine rectangle that can be merged with. *)
      let best = ref (-1) in
      Array.iteri (fun i (rect', _k') ->
        if can_be_merged rect rect' then (
          if !best = -1 then
            best := i
          else (
            let (rect_best, _k_best) = fine.(!best) in
            if distance rect rect' < distance rect rect_best then
              best := i
          )
        )) fine ;
      if !best = -1 then
        (* Returning false adds (rect, k) to the list still_too_small. *)
        false
      else (
        (* Merging the two rectangles. *)
        let (rect', k') = fine.(!best) in
        assert (can_be_merged rect rect') ;
        let rect' = Bbox.outer rect rect' in
        fine.(!best) <- (rect', merge_k k k') ;
        true
      )) too_small in
  let merging_too_small =
    (* The rectangles that are yet in still_too_small can't be merged with any rectangle
      from the fine group, but they might be merged together. *)
    let rec aux = function
      | [] -> []
      | (rect, k) :: l ->
        (* We just fetch any other rectangle in l that we could merge with. *)
        let rec fetch acc = function
          | [] -> (None, [])
          | (rect', k') :: l ->
            if can_be_merged rect rect' then (Some (rect', k'), acc @ l)
            else fetch ((rect', k') :: acc) l in
        let (o, l) = fetch [] l in
        match o with
        | None ->
          (* This rectangle has to be left unmerged. *)
          (rect, k) :: aux l
        | Some (rect', k') ->
          let rect' = Bbox.outer rect rect' in
          let k' = merge_k k k' in
          let (width, height) = Bbox.dimensions rect' in
          if width >= minwidth && height >= minheight then
            (* We just merge our rectangle into something fine! *)
            (rect', k') :: aux l
          else
            (* We keep looking for potential merges. *)
            aux ((rect', k') :: l) in
    aux still_too_small in
  let merging_too_small =
    (* At this point, there might be rectangles within merging_too_small that overlap and that
      would be useless. *)
    let order (rect1, k1) (rect2, k2) =
      let c = compare (Bbox.dimensions rect1) (Bbox.dimensions rect2) in
      if c = 0 then compare k1 k2 else c in
    let merging_too_small = List.sort order merging_too_small in
    let rec aux = function
      | [] -> []
      | (rect, k) :: l ->
        if List.exists (fun (rect', k') -> Bbox.included rect rect' && le_k k k') l then
          (* This rectangle is actually useless. *)
          aux l
        else
          match List.find_opt (fun (rect', _k') -> Bbox.overlap rect rect') l with
          | None -> (rect, k) :: aux l
          | Some (rect', k') ->
            if can_be_merged rect rect' then
              (* We found an overlapping bbox that can be merged. *)
              let rect' = Bbox.outer rect rect' in
              let k' = merge_k k k' in
              aux (insert order (rect', k') l)
            else
              (* Not much to be done there. *)
              (rect, k) :: aux l in
    aux merging_too_small in
  Array.to_list fine @ merging_too_small

(* From a function computing the missing rectangles and their associated knowledge,
  a must-box (in which everything has to be filled), and a may-box (which we might
  want to fill later), compute the list of rectangles and their associated knowledge. *)
let compute_rectangles le_k merge_k missing box bonus_box =
  let must_rectangles = missing box in
  let may_rectangles = missing bonus_box in
  let rectangles =
    List.fold_left (fun acc (rect_must, k) ->
      match List.find_opt (fun (rect_may, _k') ->
              Bbox.included rect_must rect_may) may_rectangles with
      | Some (rect, k') -> assert (K.le k k') ; (rect, k') :: acc
      | None -> assert false
    ) [] must_rectangles in
  (* We now have a list of rectangles that fit all the target area.
    We are just left to make sure that all their dimensions are correct. *)
  let rectangles =
    fit_to_dimensions le_k merge_k maxwidth maxheight minwidth minheight rectangles in
  (* Placing rectangles closer to the center first. *)
  let rectangles =
    List.sort_uniq (fun (rect1, _k1) (rect2, _k2) ->
      let c = compare (distance box rect1) (distance box rect2) in
      (* In order to avoid removing rectangles at equal distance, we add a comparison. *)
      if c = 0 then compare rect1 rect2 else c) rectangles in
  rectangles

let add_to_zone ?(maxwidth=infinity) ?(maxheight=maxwidth) ?(minwidth=0.) ?(minheight=minwidth)
    ?(safe_factor=1.) zone box =
  let bonus_box = Bbox.scale box safe_factor in
  let zone = extend_zone zone bonus_box in
  let (external_box, t) = zone in
  (* All the rectangles that would be missing for a given bbox. *)
  let missing box =
    let (_, missing_t) = extend_zone (zone_of_bbox box) external_box in
    let t = intersection (negation t) missing_t in
    to_bboxes (box, t) in
  let rectangles =
    (* The function compute_rectangles assumes a notion of knowledge:
      we just put a unit type there. *)
    let missing b = List.map (fun b -> (b, ())) (missing b) in
    let rectangles =
      compute_rectangles (fun () () -> true) (fun () () -> ()) missing box bonus_box in
    (* We then remove the notion of knowledge. *)
    List.map fst rectangles in
  (* Applying the chosen rectangles to the final box. *)
  let zone = List.fold_left add_bbox zone rectangles in
  ((external_box, simplify (fun () () -> ()) () false zone), rectangles)


module Make (K : StructuresSig.ExtLattice)
            (S : sig
                type t
                val bbox : t -> Bbox.t
              end)
            (P : sig
                type t
                val coordinates : t -> Geometry.real_coordinates
              end) = struct

let mix_objects l =
  let a = Array.of_seq l in
  let rec aux b e =
    (* Mix the elements of a from b to e included. *)
    if b = e then Seq.return a.(b)
    else if e < b then Seq.empty
    else (
      let c = (b + e) / 2 in
      Seq.cons a.(c) (Seq.append (aux b (c - 1)) (aux (c + 1) e))
    ) in
  aux 0 (Array.length a - 1)


(* Specialised trees adapted to the provided types. *)
type t_tree = (S.t option, K.t * P.t list) bbtree

type t = {
  bbtree : t_tree ;
  time_before_optimisation : int (* Optimisation is costly, so we only do it from time to time. *)
}

(* Number of operations performed between two optimisations. *)
let base_time_before_optimisation = 10

let empty = {
  bbtree = (default_bbox, Leaf None) ;
  time_before_optimisation = base_time_before_optimisation
}

(* A specialised version of extend for t_tree. *)
let extend_t : t_tree -> Bbox.t -> t_tree = extend (K.bot, []) None

(* Operations about node data information. *)
let merge_node (k1, l1) (k2, l2) =
  (K.join k1 k2, l1 @ l2)
let neutral_leaf = None
let neutral_node = (K.bot, [])
let is_in_bbox bbox (s : S.t) =
  Bbox.included (S.bbox s) bbox

(* Only optimise when the associated counter reaches 0. *)
let soft_optimise t =
  if t.time_before_optimisation <= 0 then {
    bbtree = specialise is_in_bbox (simplify merge_node neutral_node neutral_leaf t.bbtree) ;
    time_before_optimisation = base_time_before_optimisation
  } else { t with time_before_optimisation = t.time_before_optimisation - 1 }

let add_punctual t p =
  let coords = P.coordinates p in
  let rec aux bbox = function
    | Leaf None ->
      assert (Bbox.is_in bbox coords) ;
      Leaf (Some p)
    | Leaf (Some p') ->
      (* We need here to split either vertically or horizontally.
        We decide on the bigger separation between p and p'. *)
      let coords' = P.coordinates p' in
      let dist proj = Float.abs (proj coords -. proj coords') in
      let average x1 x2 = (x1 +. x2) /. 2. in
      if dist fst > dist snd then
        let (p1, p2) = if fst p > fst p' then (p', p) else (p, p') in
        Vertical (neutral_node, Leaf (Some p1), average (fst p1) (fst p2), Leaf (Some p2))
      else
        let (p1, p2) = if snd p > snd p' then (p', p) else (p, p') in
        Horizontal (neutral_node, Leaf (Some p1), average (snd p1) (snd p2), Leaf (Some p2))
    | Horizontal (kl, t1, x, t2) ->
      if fst coords > x then
        Horizontal (kl, t1, x, aux {bbox with Bbox.min_x = x} t2)
      else
        Horizontal (kl, aux {bbox with Bbox.max_x = x} t1, x, t2)
    | Vertical (kl, t1, y, t2) ->
      if snd coords > y then
        Vertical (kl, t1, y, aux {bbox with Bbox.min_y = y} t2)
      else
        Vertical (kl, aux {bbox with Bbox.max_y = y} t1, y, t2) in
  let (bbox, tree) = extend_t t.bbtree (Bbox.singleton coords) in
  let tree = aux bbox tree in
  { t with bbtree = (bbox, tree) }

let add_spatial t s =
  let sbbox = S.bbox s in
  (* We add the spatial object in the node best suited for it, without adding any node. *)
  let rec aux k bbox = function
    | Leaf None ->
      (* We have to create a node. By default, we create it according to the longer distance. *)
        let (dimx, dimy) = Bbox.dimensions bbox in
        if dimx > dimy then
          Horizontal ((k, [s]), Leaf None, fst (Bbox.center bbox), Leaf None)
        else
          Vertical ((k, [s]), Leaf None, snd (Bbox.center bbox), Leaf None)
    | Leaf (Some p) ->
      (* We have to create a node: we create it in the direction were this point is further
        away from the center. *)
      let coords = P.coordinates p in
      let dist proj = Float.abs (proj coords -. proj (Bbox.center bbox)) in
      if dist fst > dist snd then
        let y = snd (Bbox.center bbox) in
        let (p1, p2) = if snd proj > y then (None, Some p) else (Some p, None) in
        Vertical ((k, [s]), Leaf p1, y, Leaf p2)
      else
        let x = fst (Bbox.center bbox) in
        let (p1, p2) = if fst proj > x then (None, Some p) else (Some p, None) in
        Horizontal ((k, [s]), Leaf None, x, Leaf None)
    | Horizontal ((k, sl), t1, x, t2) ->
      let bbox1 = {bbox with Bbox.max_x = x} in
      let bbox2 = {bbox with Bbox.min_x = x} in
      if Bbox.included sbbox bbox1 && is_node t1 then
        Horizontal ((k, sl), aux k bbox1 t1, x, t2)
      else if Bbox.included sbbox bbox1 && is_node t2 then
        Horizontal ((k, sl), t1, x, aux k bbox2 t2)
      else (
        assert (Bbox.included sbbox bbox) ;
        Horizontal ((k, s :: sl), t1, x, t2)
      )
    | Vertical ((k, sl), t1, y, t2) ->
      let bbox1 = {bbox with Bbox.max_y = y} in
      let bbox2 = {bbox with Bbox.min_y = y} in
      if Bbox.included sbbox bbox1 && is_node t1 then
        Vertical ((k, sl), aux k bbox1 t1, y, t2)
      else if Bbox.included sbbox bbox1 && is_node t2 then
        Vertical ((k, sl), t1, y, aux k bbox2 t2)
      else (
        assert (Bbox.included sbbox bbox) ;
        Vertical ((k, s :: sl), t1, y, t2)
      ) in
  let (bbox, tree) = extend_t t.bbtree sbbox in
  let tree = aux K.bot bbox tree in
  { t with bbtree = (bbox, tree) }

let add_knowledge t kbbox k =
  (* In contrary to add_spatial, we want to store the information as precisely as possible here:
    we need to add nodes one the way. *)
  let rec aux k' bbox = function
    | Leaf po ->
      assert (Bbox.overlap bbox kbbox) ;
      assert (not (K.le k k')) ;
      (* A function that inserts [po] if it is in the right place. *)
      let add_po bbox =
        (* Coordinates to where to insert [po]. *)
        let coords =
          match po with
          | Some p -> P.coordinates p
          | None -> Bbox.center bbox in
        if Bbox.is_in bbox coords then po
        else None in
      (* The center box, containing k.
        It contains a dummy division to be able to store k. *)
      let t =
        let t = Leaf (add_po kbbox) in
        Horizontal ((k, []), Leaf None, neg_infinity, t) in
      (* We first consider the horizontal divisions. *)
      let t =
        if kbbox.Bbox.min_x > bbox.Bbox.min_x then
          let lbbox = {kbbox with Bbox.min_x = bbox.Bbox.min_x ;
                                  Bbox.max_x = kbbox.Bbox.min_x} in
          Horizontal ((k', []), Leaf (add_po lbbox), kbbox.Bbox.min_x, t)
        else t in
      let t =
        if kbbox.Bbox.max_x < bbox.Bbox.max_x then
          let lbbox = {kbbox with Bbox.min_x = kbbox.Bbox.max_x ;
                                  Bbox.max_x = bbox.Bbox.max_x} in
          Horizontal ((k', []), t, kbbox.Bbox.max_x, Leaf (add_po lbbox))
        else t in
      (* Then the vertical ones. *)
      let t =
        if kbbox.Bbox.min_y > bbox.Bbox.min_y then
          let lbbox = {bbox with Bbox.max_y = kbbox.Bbox.min_y} in
          Vertical ((k', []), Leaf (add_po lbbox), kbbox.Bbox.min_y, t)
        else t in
      let t =
        if kbbox.Bbox.max_y < bbox.Bbox.max_y then
          let lbbox = {bbox with Bbox.min_y = kbbox.Bbox.max_y} in
          Horizontal ((k', []), t, kbbox.Bbox.max_y, Leaf (add_po lbbox))
        else t in
      t
    | (Horizontal ((k', _), _, _, _) | Vertical ((k', _), _, _, _)) as t when K.le k k' -> t
    | Horizontal ((k', sl), t1, x, t2) ->
      let t1 =
        let bbox1 = {bbox with Bbox.max_x = x} in
        if Bbox.overlap bbox1 kbbox then
          aux bbox1 t1
        else t1 in
      let t2 =
        let bbox2 = {bbox with Bbox.min_x = x} in
        if Bbox.overlap bbox2 kbbox then
          aux bbox2 t2
        else t2 in
      Horizontal ((k', sl), t1, x, t2)
    | Vertical ((k', sl), t1, y, t2) ->
      let t1 =
        let bbox1 = {bbox with Bbox.max_y = y} in
        if Bbox.overlap bbox1 kbbox then
          aux bbox1 t1
        else t1 in
      let t2 =
        let bbox2 = {bbox with Bbox.min_y = y} in
        if Bbox.overlap bbox2 kbbox then
          aux bbox2 t2
        else t2 in
      Vertical ((k', sl), t1, y, t2)
    in
  let (bbox, tree) = extend_t t.bbtree sbbox in
  let tree = if K.eq k K.bot then tree else aux K.bot bbox tree in
  { t with bbtree = (bbox, tree) }

let get_punctual bbox t =
  let rec aux cbbox = function
    | Leaf None -> Seq.empty
    | Leaf (Some p) ->
      if Bbox.is_in bbox (P.coordinates p) then Seq.return p else Seq.empty
    | Horizontal (_, t1, x, t2) ->
      let bbox1 = {cbbox with Bbox.max_x = x} in
      let bbox2 = {cbbox with Bbox.min_x = x} in
      let s1 = if Bbox.overlap bbox bbox1 then aux bbox1 t1 else Seq.empty in
      let s2 = if Bbox.overlap bbox bbox2 then aux bbox2 t2 else Seq.empty in
      Seq.append s1 s2
    | Vertical (_, t1, y, t2) ->
      let bbox1 = {cbbox with Bbox.max_y = y} in
      let bbox2 = {cbbox with Bbox.min_y = y} in
      let s1 = if Bbox.overlap bbox bbox1 then aux bbox1 t1 else Seq.empty in
      let s2 = if Bbox.overlap bbox bbox2 then aux bbox2 t2 else Seq.empty in
      Seq.append s1 s2 in
  let (bbox, tree) = t.bbtree in
  aux bbox tree

let get_spatial bbox ?(partial=false) t =
  (* Given an object of bounding box bbox', should we accept it? *)
  let accept bbox' =
    if partial then Bbox.overlap bbox bbox'
    else Bbox.included bbox' bbox in
  let rec aux cbbox = function
    | Leaf _ -> Seq.empty
    | Horizontal ((_k, sl), t1, x, t2) ->
      let s = Seq.filter (fun s -> accept (S.bbox s)) (List.to_seq sl) in
      let bbox1 = {cbbox with Bbox.max_x = x} in
      let bbox2 = {cbbox with Bbox.min_x = x} in
      let s1 = if Bbox.overlap bbox bbox1 then aux bbox1 t1 else Seq.empty in
      let s2 = if Bbox.overlap bbox bbox2 then aux bbox2 t2 else Seq.empty in
      Seq.append s (Seq.append s1 s2)
    | Vertical (_, t1, y, t2) ->
      let s = Seq.filter (fun s -> accept (S.bbox s)) (List.to_seq sl) in
      let bbox1 = {cbbox with Bbox.max_y = y} in
      let bbox2 = {cbbox with Bbox.min_y = y} in
      let s1 = if Bbox.overlap bbox bbox1 then aux bbox1 t1 else Seq.empty in
      let s2 = if Bbox.overlap bbox bbox2 then aux bbox2 t2 else Seq.empty in
      Seq.append s (Seq.append s1 s2) in
  let (bbox, tree) = t.bbtree in
  aux bbox tree

let where_to_scan ?(maxwidth=infinity) ?(maxheight=maxwidth) ?(minwidth=0.) ?(minheight=minwidth)
    ?(safe_factor=1.) t bbox k_target =
  let bonus_box = Bbox.scale box safe_factor in
  let t = extend_t t bonus_box in
  let (external_box, tree) = t in
  (* All the rectangles and their associated missing knowledge that would be missing for a
    given bbox. *)
  let missing box =
    let rec aux acc cbbox k = function
      | Leaf _ -> [(Bbox.intersection bbox cbbox, K.diff k_target k)]
      | Horizontal ((k, _), t1, x, t2) ->
        if K.le k_target k then
          (* We already have all the needed knowledge here. *)
          acc
        else
          let acc =
            let bbox1 = {cbbox with Bbox.max_x = x} in
            if Bbox.overlap bbox bbox1 then
              aux acc bbox1 k t1
            else acc in
          let acc =
            let bbox2 = {cbbox with Bbox.min_x = x} in
            if Bbox.overlap bbox bbox2 then
              aux acc bbox2 k t2
            else acc in
          acc
      | Vertical ((k, _), t1, y, t2) ->
        if K.le k_target k then
          (* We already have all the needed knowledge here. *)
          acc
        else
          let acc =
            let bbox1 = {cbbox with Bbox.max_y = y} in
            if Bbox.overlap bbox bbox1 then
              aux acc bbox1 k t1
            else acc in
          let acc =
            let bbox2 = {cbbox with Bbox.min_y = y} in
            if Bbox.overlap bbox bbox2 then
              aux acc bbox2 k t2
            else acc in
          acc in
    aux [] external_box K.bot tree in
  compute_rectangles K.le K.union missing box bonus_box

end

