
type real_coordinates = float * float
type coordinates = int * int

let convert_real_coordinates (x, y) = (Float.to_int x, Float.to_int y)
let convert_coordinates_to_real (x, y) = (Float.of_int x, Float.of_int y)


type style =
  | Round
  | Square
  | Adaptive


module IntOrder : Set.OrderedType = struct
  type t = int
  let compare : t -> t -> int = compare
end

module Data : Structures.DirectedSet =
  let open Structures in
  AddPriority (Priority)
    (AddPriority (IntOrder)
       (DirectedSetProduct (Shape)
          (TopDirectedSet (struct type t = Dot.color let top = Dot.white end))))


type t = (* TODO *)

let within_bounds m (x, y) = (* TODO *)

let empty (x, y) = (* TODO *)

let to_dot_matrix m = (* TODO *)

let get m (x, y) = (* TODO *)
let set m (x, y) v = (* TODO *)

let add m (x, y) v =
  if within_bounds m (x, y) then
    let v0 = get m (x, y) in
    let v' = join v v0 in
    set m (x, y) v'
  else m


let map_adaptive v = function
  | Round -> Dot.Round
  | Square -> Dot.Square
  | Adaptive -> v

let add_PoI_int m xy priority level shape color =
  let v = (priority, (level, (shape, color))) in
  add m xy v

let add_PoI m xy ?(priority = Structures.High) ?(level = 0) ?(style = Round) color =
  let shape = map_adaptive Dot.Round style in
  add_PoI_int m (convert_real_coordinates xy) priority level shape color


let is_letter c = (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')

let add_text m (x, y) ?(only_letters = true)
    ?(priority = Structures.VeryHigh) ?(level = max_int) str =
  Seq.fold_lefti (fun m i c ->
      if only_letters and not (is_letter c) then m
      else add_PoI_int m (x + i, y) priority level Dot.Round (Dot.Letter c)
    ) m (String.to_seq str)


let fold_within m max start endv =
  let (start, endv) = if start > endv then (endv, start) else (start, endv) in
  if endv < 0 (* start < 0 *) then m
  else
    let start = if start < 0 (* endv >= 0 *) then 0 else start in
    if start > max then m
    else
      let endv = if endv > max (* start <= max *) then max else endv in
      let rec aux m current =
        if current > endv then m
        else
          let m = f m in
          aux m (1 + current) in
      aux m start

(* Iterate from start_x to end_x, but within the map. *)
let fold_within_x m start_x end_x (f : t -> int -> t) =
  let max_x = (* TODO *) in
  fold_within m max_x start_x end_x f

(* Same, but for start_y to end_y. *)
let fold_within_y m start_y end_y (f : t -> int -> t) =
  let max_y = (* TODO *) in
  fold_within m max_y start_y end_y f


let add_line m xy xy'
    ?(priority = Structures.Medium) ?(non_core_priority = Structures.VeryLow)
    ?(level = 0) ?(style = Adaptive) (?non_core_style = Some Adaptive) color =
  (* *)
  let v = (priority, (level, (shape, color))) in
  (* Looking for special cases. *)
  let special_cases =
    let (x, y) = convert_coordinates_to_real xy in
    let (x', y') = convert_coordinates_to_real xy' in
    if x = x' and y = y' then
      let shape = map_adaptive Dot.Round style in
      let m = add_PoI_int m (x, y) priority level shape color in
      Some m
    else if x = x' (* y <> y' *) then
      let m =
        let (y, y') = if y > y' then (y', y) else (y, y') in
        fold_within_y m y y' (fun yc ->
          let shape =
            let shape =
              if yc = y then Dot.Half_circle Dot.South
              else if yc = y' then Dot.Half_circle Dot.North
              else Dot.Square in
            map_adaptive shape style in
          add_PoI_int m (x, yc) priority level shape color) in
      Some m
    else if y = y' (* x <> x' *) then
      let m =
        let (x, x') = if x > x' then (x', x) else (x, x') in
        fold_within_x m x x' (fun xc ->
          let shape =
            let shape =
              if xc = x then Dot.Half_circle Dot.West
              else if xc = x' then Dot.Half_circle Dot.East
              else Dot.Square in
            map_adaptive shape style in
          add_PoI_int m (xc, y) priority level shape color) in
      Some m
    else None
  in
  match special_cases with
  | Some m -> m
  | None ->
    (* Getting from left to right. *)
    let ((x, y), (x', y')) =
      if fst xy < fst xy' then (xy, xy') else (xy', xy) in
    let slope = (y' -. y) /. (x' -. x) in
    (* y (resp. x) position at a given integer xt (resp. yt) target position. *)
    let y_at xt = y +. (Float.of_int xt -. x) *. slope in
    let x_at yt = x +. (Float.of_int yt -. y) /. slope in
    (* Given that the slope is not necessarily positive, the next cell may be above or below. *)
    let next_yt yt = if slope > 0. then (yt + 1) else (yt - 1) in
    let (end_x, end_y) = convert_real_coordinates (x', y') in
    let rec aux m (xc, yc) =
      if (xc, yc) = (end_x, end_y) or not within_bounds m (xc, yc) then m
      else
        let y' = y_at (xc + 1) in
        let yc' = next_yt yc in
        let x' = next_x yc' in
        let distance_x = x' -. Float.of_int xc in
        assert (distance_x >= 0) ;
        let distance_y = Float.abs (y' -. Float.of_int xc) in
        if distance_x > distance_y then
          (* The smallest distance is to go to the right. *)
          (* TODO: determine whether the current cell is core or non-core. *)
          let m = add_PoI_int m (xc, yc) priority level (Dot.Half_circle Dot.West) color in
          let xc = xc + 1 in
          let m = add_PoI_int m (xc, yc) priority level (Dot.Half_circle Dot.East) color in
          aux m (xc, yc)
        else
          (* The smallest distance is to go above or below. *)
          (* TODO: determine whether the current cell is core or non-core. *)
          let dir = if slope > 0. then Dot.North else Dot.South
          let m = add_PoI_int m (xc, yc) priority level (Dot.Half_circle dir) color in
          let m = add_PoI_int m (xc, yc') priority level (Dot.Half_circle (Shape.uturn dir)) color in
          aux m (xc, yc')
    in
    (* Drawing as a dot the extremities. *)
    let draw_extremes m xy = add_PoI_int m priority level Dot.Round in
    let start_xy = convert_real_coordinates (x, y) in
    let m = draw_extremes m start_xy in
    let m = draw_extremes m (end_x, end_y) in
    (* TODO: if xy is out of the map, start with better coordinates. *)
    aux m start_xy

