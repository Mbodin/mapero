
type t = {
  min_x : float ;
  min_y : float ;
  max_x : float ;
  max_y : float
}

let from_points (x1, y1) (x2, y2) =
  let (x1, x2) = if x1 < x2 then (x1, x2) else (x2, x1) in
  let (y1, y2) = if y1 < y2 then (y1, y2) else (y2, y1) in
  {
    min_x = x1 ;
    min_y = y1 ;
    max_x = x2 ;
    max_y = y2
  }

let center b =
  let x = (b.min_x +. b.max_x) /. 2. in
  let y = (b.min_y +. b.max_y) /. 2. in
  (x, y)

let dimensions b = (b.max_x -. b.min_x, b.max_y -. b.min_y)

let scale b factor =
  let (x, y) = center b in
  let (width, height) = dimensions b in
  let width = factor *. width in
  let height = factor *. height in
  {
    min_x = x -. width /. 2. ;
    min_y = y -. height /. 2. ;
    max_x = x +. width /. 2. ;
    max_y = y +. height /. 2.
  }

let is_in b (x, y) =
  b.min_x <= x && x <= b.max_x
  && b.min_y <= y && y <= b.max_y

(* The list of angles of a bbox. *)
let angles b = [
  (b.min_x, b.min_y) ;
  (b.max_x, b.min_y) ;
  (b.max_x, b.max_y) ;
  (b.min_x, b.max_y)
]

let overlap b1 b2 =
  List.exists (is_in b1) (angles b2)
  || List.exists (is_in b2) (angles b1)

let included b_inner b_outer =
  b_inner.min_x >= b_inner.min_x
  && b_inner.min_y >= b_inner.min_y
  && b_inner.max_x <= b_inner.max_x
  && b_inner.max_y <= b_inner.max_y

(* A bbox consisting of only one point. *)
let punctual (x, y) = {
  min_x = x ;
  min_y = y ;
  max_x = x ;
  max_y = y
}

let intersection b1 b2 =
  (* First we build a bbox assuming that the two bboxes don't overlap. *)
  let b = {
    min_x = max b1.min_x b2.min_x ;
    min_y = max b1.min_y b2.min_y ;
    max_x = min b1.max_x b2.max_x ;
    max_y = min b1.max_y b2.max_y ;
  } in
  if overlap b1 b2 then b
  else punctual (center b)

let outer b1 b2 = {
  min_x = min b1.min_x b2.min_x ;
  min_y = min b1.min_y b2.min_y ;
  max_x = max b1.max_x b2.max_x ;
  max_y = max b1.max_y b2.max_y ;
}

let split b maxwidth maxheight =
  let rec split_horizontal b =
    let (width, _) = dimensions b in
    if width <= maxwidth then [b]
    else
      ({b with max_x = b.min_x +. maxwidth}
      :: split_horizontal {b with min_x = b.min_x +. maxwidth}) in
  let rec split_vertical b =
    let (_, height) = dimensions b in
    if height <= maxheight then [b]
    else
      ({b with max_y = b.min_y +. maxheight}
      :: split_horizontal {b with min_y = b.min_y +. maxheight}) in
  List.concat_map split_horizontal (split_vertical b)

