
(* The actual type storing the spatial information of whether a particular zone is covered. *)
type tree =
  | Full (* The provided zone is fully covered. *)
  | Empty (* The provided zone is not covered at all. *)
  | Horizontal of tree * float * tree (* Horizontal division, with the middle x coordinate *)
  | Vertical of tree * float * tree (* Vertical division, with the middle y coordinate *)

(* We associate this spatial information with the external bounding box to get a precise zone. *)
type t = Bbox.t * tree

let default_bbox = Bbox.from_points (0., 0.) (0., 0.)

let empty = (default_bbox, Empty)


(* Restriction functions of a tree along a semi-line. *)

let rec restrict_gen fx fy = function
  | Full -> Full
  | Empty -> Empty
  | Horizontal (t1, x, t2) ->
    let t1 = restrict_gen fx fy t1 in
    let t2 = restrict_gen fx fy t2 in
    fx t1 x t2
  | Vertical (t1, y, t2) ->
    let t1 = restrict_gen fx fy t1 in
    let t2 = restrict_gen fx fy t2 in
    fy t1 y t2

let restrict_before_x x0 =
  restrict_gen
    (fun t1 x t2 -> if x >= x0 then t1 else Horizontal (t1, x, t2))
    (fun t1 y t2 -> Vertical (t1, y, t2))

let restrict_after_x x0 =
  restrict_gen
    (fun t1 x t2 -> if x <= x0 then t2 else Horizontal (t1, x, t2))
    (fun t1 y t2 -> Vertical (t1, y, t2))

let restrict_before_y y0 =
  restrict_gen
    (fun t1 x t2 -> Horizontal (t1, x, t2))
    (fun t1 y t2 -> if y >= y0 then t1 else Horizontal (t1, y, t2))

let restrict_after_y y0 =
  restrict_gen
    (fun t1 x t2 -> Horizontal (t1, x, t2))
    (fun t1 y t2 -> if y <= y0 then t2 else Horizontal (t1, y, t2))


(* Negation of a tree. *)
let negation = function
  | Full -> Empty
  | Empty -> Full
  | Horizontal (t1, x, t2) -> Horizontal (negation t1, x, negation t2)
  | Vertical (t1, y, t2) -> Vertical (negation t1, y, negation t2)

(* Intersection of two trees. *)
let rec intersection t = function
  | Empty -> Empty
  | Full -> t
  | Horizontal (t1, x, t2) ->
    let t1 = intersection (restrict_before_x x t) t1 in
    let t2 = intersection (restrict_after_x x t) t2 in
    Horizontal (t1, x, t2)
  | Vertical (t1, y, t2) ->
    let t1 = intersection (restrict_before_y y t) t1 in
    let t2 = intersection (restrict_after_y y t) t2 in
    Vertical (t1, y, t2)

(* Union of two trees. *)
let rec union t = function
  | Empty -> t
  | Full -> Full
  | Horizontal (t1, x, t2) ->
    let t1 = union (restrict_before_x x t) t1 in
    let t2 = union (restrict_after_x x t) t2 in
    Horizontal (t1, x, t2)
  | Vertical (t1, y, t2) ->
    let t1 = union (restrict_before_y y t) t1 in
    let t2 = union (restrict_after_y y t) t2 in
    Vertical (t1, y, t2)

(* Split a list in two at the given index. *)
let split_list =
  let rec aux acc i = function
    | l when i = 0 -> (List.rev acc, l)
    | x :: l -> aux (x :: acc) (i - 1) l
    | [] ->
      assert (i > 0) ;
      assert false in
  aux []

(* A (costly) function to simplify bounded trees. *)
let optimise ((bbox, t) : t) =
  (* Create a balanced tree from a list. *)
  let make_tree constr l =
    let rec aux s l =
      (* Invariant: List.length l = s > 0. *)
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
  (* Remove the dupplicates in a list of pointed trees. *)
  let rec remove_duplicates = function
    | (x, t1) :: (_, t2) :: l when t1 = t2 -> remove_duplicates ((x, t1) :: l)
    | (x, t) :: l -> (x, t) :: remove_duplicates l in
  (* Return a horizontal list of subtrees, each associated with the x coordinate they start with. *)
  let rec list_horizontal bbox = function
    | Horizontal (t1, x, t2) ->
      if x <= bbox.Bbox.min_x then list_horizontal bbox t2
      else if x >= bbox.Bbox.max_x then list_horizontal bbox t1
      else
        let l1 = list_horizontal {bbox with Bbox.max_x = x} t1 in
        let l2 = list_horizontal {bbox with Bbox.min_x = x} t2 in
        remove_duplicates (l1 @ l2)
    | t -> [(bbox.Bbox.min_x, optimise_vertical bbox t)]
  (* Group all the horizontal elements into a balanced tree. *)
  and optimise_horizontal bbox t =
    let l = list_horizontal bbox t in
    make_tree (fun (x1, t1) (x2, t2) ->
      (x1, Horizontal (t1, x2, t2))) l
  (* Same than list_horizontal, but vertically. *)
  and list_vertical bbox = function
    | Vertical (t1, y, t2) ->
      if x <= bbox.Bbox.min_y then list_vertical bbox t2
      else if x >= bbox.Bbox.max_y then list_vertical bbox t1
      else
        let l1 = list_vertical {bbox with Bbox.max_y = y} t1 in
        let l2 = list_vertical {bbox with Bbox.min_y = y} t2 in
        remove_duplicates (l1 @ l2)
    | t -> [(bbox.Bbox.min_y, optimise_horizontal bbox t)]
  (* Same than optimise_horizontal, but vertically. *)
  and optimise_vertical bbox t =
    let l = list_vertical bbox t in
    make_tree (fun (y1, t1) (y2, t2) ->
      (y1, Vertical (t1, y2, t2))) l in
  optimise_horizontal bbox t

(* Extend a zone to a larger bbox, without changing its meaning. *)
let extend (b1, t) b2 =
  let open Bbox in
  let b2 = outer b1 b2 in
  let t = Horizontal (Empty, b1.min_x, Horizontal (t, b1.max_x, Empty)) in
  let t = Vertical (Empty, b1.min_y, Vertical (t, b1.max_y, Empty)) in
  (b2, t)

(* Get a zone from a bbox. *)
let zone_of_bbox b = (b, Full)

(* Add a bbox to a zone. *)
let add_bbox z b2 =
  let (b, t) = extend z b2 in
  let (b', t') = extend (zone_of_bbox b2) b in
  assert (b = b') ;
  (b, union t t')

(* Convert a zone into a list of rectangles. *)
let to_bboxes (bbox, t) =
  let rec aux acc bbox = function
    | Empty -> acc
    | Full -> bbox :: acc
    | Horizontal (t1, x, t2) ->
      let acc = aux acc {bbox with Bbox.max_x = min bbox.Bbox.max_x x} t1 in
      let acc = aux acc {bbox with Bbox.min_x = max bbox.Bbox.min_x x} t2 in
      acc
    | Vertical (t1, y, t2) ->
      let acc = aux acc {bbox with Bbox.max_y = min bbox.Bbox.max_y y} t1 in
      let acc = aux acc {bbox with Bbox.min_y = max bbox.Bbox.min_y y} t2 in
      acc in
  acc [] bbox t

let add ?(maxwidth=infinity) ?(maxheight=maxwidth) ?(minwidth=0.) ?(minheight=minwidth)
    ?(safe_factor=1.) zone box =
  let bonus_box = Bbox.scale box safe_factor in
  let zone = extend zone bonus_box in
  let (external_box, t) = zone in
  (* All the rectangles that would be missing for a given bbox. *)
  let missing box =
    let (_, missing_t) = extend (zone_of_bbox box) external_box in
    let t = intersection (negation t) missing_t in
    to_bboxes t in
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
    List.concat_map (fun rectangle ->
      Bbox.split rectangle maxwidth maxheight) rectangles in
  (* TODO: Merge too small rectangles. *)
  let zone = List.fold_left add_bbox zone rectangles in
  (zone, rectangles)

