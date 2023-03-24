
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
  | Division of {
      (* The provided zone is divised into four smaller zones. *)
      sep_x : float (* The x coordinate where the division happens *) ;
      sep_y : float (* The y coordinate where the division happens *) ;
      sub_nw : tree (* North-West subzone *) ;
      sub_ne : tree (* North-East subzone *) ;
      sub_se : tree (* South-East subzone *) ;
      sub_sw : tree (* South-West subzone *)
    }

(* We associate this spatial information with the external bounding box to get a precise zone. *)
type subzone = bbox * tree

(* Several zones might be disjoint: we store a list of them. *)
type t = subzone list


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

let add zone box =
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

