
type bbox = {
  min_x : float ;
  min_y : float ;
  max_x : float ;
  max_y : float
}

let to_bbox (x1, y1) (x2, y2) =
  let (x1, x2) = if x1 < x2 then (x1, x2) else (x2, x1) in
  let (y1, y2) = if y1 < y2 then (y1, y2) else (y2, y1) in
  {
    min_x = x1 ;
    min_y = y1 ;
    max_x = x2 ;
    max_y = y2
  }

(* Given a bounding box and a point, whether the point is in the box. *)
let is_in b (x, y) =
  b.min_x <= x && x <= b.max_x
  && b.min_y <= y && y <= b.may_y

(* The list of angles of a bbox. *)
let angles b = [
  (b.min_x, b.min_y) ;
  (b.max_x, b.min_y) ;
  (b.max_x, b.max_y) ;
  (b.min_x, b.max_y)
]

(* Given two bbox, whether they overlap. *)
let overlap b1 b2 =
  List.exists (is_in b1) (angles b2)
  || List.exists (is_in b2) (angles b1)

(* The intersection of two overlapping bboxes. *)
let inter b1 b2 = {
  min_x = max b1.min_x b2.min_x ;
  min_y = max b1.min_y b2.min_y ;
  max_x = min b1.max_x b2.max_x ;
  max_y = min b1.max_y b2.max_y ;
}


(* The actual type storing the spatial information of whether a particular zone is covered. *)
type tree =
  | Full (* The provided zone is fully covered. *)
  | Empty (* The provided zone is not covered at all. *)
  | Horizontal of tree * float * tree (* Horizontal division, with the middle x coordinate *)
  | Vertical of tree * float * tree (* Vertical division, with the middle y coordinate *)

(* We associate this spatial information with the external bounding box to get a precise zone. *)
type t = bbox * tree


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
    let t1 = intersection (restrict_before_x x t) t1 in
    let t2 = intersection (restrict_after_x x t) t2 in
    Horizontal (t1, x, t2)
  | Vertical (t1, y, t2) ->
    let t1 = intersection (restrict_before_y y t) t1 in
    let t2 = intersection (restrict_after_y y t) t2 in
    Vertical (t1, y, t2)

(* Split a list in two at the given index. *)
let split_list i =
  let rec aux acc i = function
    | l when i = 0 -> (List.rev acc, l)
    | x :: l -> aux (x :: acc) (i - 1) l
    | [] ->
      assert (i > 0) ;
      assert false

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
      if x <= bbox.min_x then list_horizontal bbox t2
      else if x >= bbox.max_x then list_horizontal bbox t1
      else
        let l1 = list_horizontal {bbox with max_x = x} t1 in
        let l2 = list_horizontal {bbox with min_x = x} t2 in
        remove_duplicates (l1 @ l2)
    | t -> [(bbox.min_x, optimise_vertical bbox t)]
  and optimise_horizontal bbox t =
    let l = list_horizontal bbox t in
    make_tree (fun (x1, t1) (x2, t2) ->
      (x1, Horizontal (t1, x2, t2))) l


(* TODO *)


(* Merge two subzones. *)
let merge z1 z2 = (* TODO *) z1


(* All the missing subzones to fill a given box. *)
let missings box (b, t) =
  let rec aux b (* Invariant: overlap box b *) = function
    | Full -> []
    | Empty -> inter box b
    | Division d ->
      (if d.sep_x > box.max_x then []
       else ((* box intersects one of the East subzones. *)
          (if d.sep_y < box.min_y then []
           else aux b d.sub_se)
          @
          (if d.sep_y > box.max_y then []
           else aux b d.sub_ne)
        )
      )
      @
      (if d.sep_x < box.min_x then []
       else ((* box intersects one of the West subzones. *)
          (if d.sep_y < box.min_y then []
           else aux b d.sub_sw)
          @
          (if d.sep_y > box.max_y then []
           else aux b d.sub_nw)
        )
      )
  in
  if overlap box b then aux b t
  else []

(* Grouping boxes that can be grouped. *)
let optimise bboxes = (* TODO *) bboxes

let empty = []

let add ?(maxwidth=infinity) ?(maxheight=maxwidth) ?(minwidth=0.) ?(minheight=minwidth) zone box =
  (* First, we get all the relevant subzones. *)
  let (relevant, other) = List.partition (fun (b, _) -> overlap box b) in
  match relevant with
  | [] ->
    (* In this case, we just add a new zone. *)
    ((box, Full) :: other, [box])
  | z :: lz ->
    let zone = List.fold_left merge z lz in
    let to_be_filled = missings box zone in
    let zone = merge (box, Full) zone in
    let to_be_filled = optimise to_be_filled in
    (zone :: other, to_be_filled)

