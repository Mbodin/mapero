
(* A type to store spacially localised information.
  Information can be stored at the level of nodes (typically for non punctual data) and leaves. *)
type ('node, 'leaf) tree =
  | Leaf of 'leaf
  | Horizontal of 'node * ('node, 'leaf) tree * float * ('node, 'leaf) tree (* Horizontal division, with the middle x coordinate *)
  | Vertical of 'node * ('node, 'leaf) tree * float * ('node, 'leaf) tree (* Vertical division, with the middle y coordinate *)

(* We associate this spatial information with the external bounding box to get a precise zone. *)
type ('node, 'leaf) bbtree = Bbox.t * ('node, 'leaf) tree

type zone = (unit, bool) bbtree


(* A default bounded box, for initialisation. *)
let default_bbox = Bbox.from_points (0., 0.) (0., 0.)

let empty : zone = (default_bbox, Leaf false)

(* Get a zone from a bbox, filling up the zone. *)
let zone_of_bbox b : zone = (b, Leaf true)


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
        let (d, t1, x, t2) = (data, Leaf neutral_leaf, neg_infinity, Lead d) in
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
  can be identified spacially.
  It takes a function stating whether an object is within a bbox or not. *)
let rec specialise is_in_bbox (bbox, t) =
  let is_leaf = function
    | Leaf _ -> true
    | _ -> false in
  (* Assuming that t is not a leaf, incorporate the provided data to t's data. *)
  let incorporate data = function
    | Leaf _ -> assert false
    | Horizontal ((state, data'), t1, x, t2) -> Horizontal ((state, data @ data'), t1, x, t2)
    | Vertical ((state, data'), t1, y, t2) -> Vertical ((state, data @ data'), t1, y, t2) in
  (* Extract from data the objects within the provided bbox and incorporate it into t. *)
  let aux data t bbox =
    let t = specialise is_in_bbox (bbox, t) in
    if is_leaf t then (data, t)
    else
      let (data_inner, data) = List.partition (is_in_bbox bbox) data in
      (data, incorporate data_inner t) in
  match t with
  | Leaf _ -> (bbox, t)
  | Horizontal ((state, data), t1, x, t2) ->
    let (data, t1) = aux t1 {bbox with Bbox.max_x = x} in
    let (data, t2) = aux t2 {bbox with Bbox.min_x = x} in
    Horizontal ((state, data), t1, x, t2)
  | Vertical ((state, data), t1, y, t2) ->
    let (data, t1) = aux t1 {bbox with Bbox.max_y = y} in
    let (data, t2) = aux t2 {bbox with Bbox.min_y = y} in
    Vertical ((state, data), t1, y, t2)

(* Extend a zone to a larger bbox, without changing its meaning.
  It takes a neutral element for leaves. *)
let extend neutral_leaf (b1, t) b2 =
  let open Bbox in
  let b2 = outer b1 b2 in
  let empty = Leaf neutral_leaf in
  let t = Horizontal (empty, b1.min_x, Horizontal (t, b1.max_x, empty)) in
  let t = Vertical (empty, b1.min_y, Vertical (t, b1.max_y, empty)) in
  (b2, t)

(* Add a bbox to a zone. *)
let add_bbox (z : zone) b2 =
  let (b, t) = extend false z b2 in
  let (b', t') = extend false (zone_of_bbox b2) b in
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

(* Given a list of rectangles, try to return a slightly different list
  covering the same area, and following the min and max dimensions. *)
let fit_to_dimensions maxwidth maxheight minwidth minheight rectangles =
  (* First, splitting the larger ones. *)
  let rectangles =
    List.concat_map (fun rectangle ->
      Bbox.split rectangle maxwidth maxheight) rectangles in
  (* At this point, rectangles are at most maxwidth by maxheight in dimension. *)
  (* Splitting all rectangles on whether their dimensions are fine or not. *)
  let (fine, too_small) =
    List.partition (fun rectangle ->
      let (width, height) = Bbox.dimensions rectangle in
      width >= minwidth && height >= minheight) rectangles in
  (* What follows is more efficient with in-place replacments. *)
  let fine = Array.of_list fine in
  let can_be_merged rect1 rect2 =
    let rect' = Bbox.outer rect1 rect2 in
    let (x, y) = Bbox.dimensions rect' in
    x <= maxwidth && y <= maxheight in
  let (_, still_too_small) =
    (* Merging too small rectangles if possible. *)
    List.partition (fun rect ->
      (* Fetching the closest fine rectangle that can be merged with. *)
      let best = ref (-1) in
      Array.iteri (fun i rect' ->
        if can_be_merged rect rect' then (
          if !best = -1 then
            best := i
          else (
            let rect_best = fine.(!best) in
            if distance rect rect' < distance rect rect_best then
              best := i
          )
        )) fine ;
      if !best = -1 then false
      else (
        (* Merging the two rectangles. *)
        let rect' = fine.(!best) in
        assert (can_be_merged rect rect') ;
        let rect' = Bbox.outer rect rect' in
        fine.(!best) <- rect' ;
        true
      )) too_small in
  let merging_too_small =
    (* The rectangles that are yet in still_too_small can't be merged with any rectangle
      from the fine group, but they might be merged together. *)
    let rec aux = function
      | [] -> []
      | rect :: l ->
        (* We just fetch any other rectangle in l that we could merge with. *)
        let rec fetch acc = function
          | [] -> (None, [])
          | rect' :: l ->
            if can_be_merged rect rect' then (Some rect', acc @ l)
            else fetch (rect' :: acc) l in
        let (o, l) = fetch [] l in
        match o with
        | None ->
          (* This rectangle has to be left unmerged. *)
          rect :: aux l
        | Some rect' ->
          let rect' = Bbox.outer rect rect' in
          let (width, height) = Bbox.dimensions rect' in
          if width >= minwidth && height >= minheight then
            (* We just merge our rectangle into something fine! *)
            rect' :: aux l
          else
            (* We keep looking for potential merges. *)
            aux (rect' :: l) in
    aux still_too_small in
  let merging_too_small =
    (* At this point, there might be rectangles within merging_too_small that overlap and that
      would be useless. *)
    let order rect1 rect2 = compare (Bbox.dimensions rect1) (Bbox.dimensions rect2) in
    let merging_too_small =
      List.sort order merging_too_small in
    let rec aux = function
      | [] -> []
      | rect :: l ->
        if List.exists (Bbox.included rect) l then
          (* This rectangle is actually useless. *)
          aux l
        else
          match List.find_opt (Bbox.overlap rect) l with
          | None -> rect :: aux l
          | Some rect' ->
            if can_be_merged rect rect' then
              (* We found an overlapping bbox that can be merged. *)
              let rect' = Bbox.outer rect rect' in
              aux (insert order rect' l)
            else
              (* Not much to be done there. *)
              rect :: aux l in
    aux merging_too_small in
  Array.to_list fine @ merging_too_small

let add ?(maxwidth=infinity) ?(maxheight=maxwidth) ?(minwidth=0.) ?(minheight=minwidth)
    ?(safe_factor=1.) zone box =
  let bonus_box = Bbox.scale box safe_factor in
  let zone = extend zone bonus_box in
  let (external_box, t) = zone in
  (* All the rectangles that would be missing for a given bbox. *)
  let missing box =
    let (_, missing_t) = extend (zone_of_bbox box) external_box in
    let t = intersection (negation t) missing_t in
    to_bboxes (box, t) in
  let must_rectangles = missing box in
  let may_rectangles = missing bonus_box in
  let rectangles =
    List.fold_left (fun acc rectangle ->
      match List.find_opt (Bbox.included rectangle) may_rectangles with
      | Some rect -> rect :: acc
      | None -> assert false
    ) [] must_rectangles in
  let rectangles = List.sort_uniq compare rectangles in
  (* We now have a list of rectangles that fit all the target area.
    We are just left to make sure that all their dimensions are correct. *)
  let rectangles =
    fit_to_dimensions maxwidth maxheight minwidth minheight rectangles in
  (* Placing rectangles closer to the center first. *)
  let rectangles =
    List.sort (fun rect1 rect2 ->
      compare (distance box rect1) (distance box rect2)) rectangles in
  (* Applying the chosen rectangles to the final box. *)
  let zone = List.fold_left add_bbox zone rectangles in
  ((external_box, simplify (fun () () -> ()) () false zone), rectangles)

