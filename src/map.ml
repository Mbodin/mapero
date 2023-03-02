
type real_coordinates = float * float
type coordinates = int * int

let convert_coordinates_from_real (x, y) = (Float.to_int x, Float.to_int y)
let convert_coordinates_to_real (x, y) = (Float.of_int x, Float.of_int y)


type style =
  | Round
  | Square
  | Adaptive


module IntOrder = struct
  type t = int
  let compare : t -> t -> int = compare
end

module Open = struct (* This module just serve to circumvent the absence of [let open in] in the syntax for modules. *)
open Structures
module Data =
  (*let open Structures in*)
  AddPriority (Priority)
    (AddPriority (IntOrder)
       (OptionDirectedSet
          (DirectedSetProduct (Shape)
             (TopDirectedSet (struct type t = Dot.color let top = Dot.White end)))))
end
open Open

type point = { x : int ; y : int }

type t = {
  size : point ;
  data : Data.t array array
}

let within_bounds m (x, y) =
  x >= 0 && x < m.size.x &&
  y >= 0 && y < m.size.y


let empty (x, y) = {
  size = { x ; y } ;
  data = Array.make_matrix x y (Structures.VeryLow, (min_int, None))
}

let to_dot_matrix m =
  Array.map (Array.map (fun (_priority, (_level, o)) -> o)) m.data

let add m (x, y) v =
  let get m (x, y) = m.data.(x).(y) in
  let set m (x, y) v = m.data.(x).(y) <- v; m in
  if within_bounds m (x, y) then
    let v0 = get m (x, y) in
    let v' = Data.join v v0 in
    set m (x, y) v'
  else m


let map_adaptive v = function
  | Round -> Dot.Round
  | Square -> Dot.Square
  | Adaptive -> v

let add_PoI_int m xy priority level shape color =
  let v = (priority, (level, Some (shape, color))) in
  add m xy v

let add_PoI m xy ?(priority = Structures.High) ?(level = 0) ?(style = Round) color =
  let shape = map_adaptive Dot.Round style in
  add_PoI_int m (convert_coordinates_from_real xy) priority level shape color


let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let add_text m (x, y) ?(only_letters = true)
    ?(priority = Structures.VeryHigh) ?(level = max_int) str =
  Seq.fold_lefti (fun m i c ->
      if only_letters && not (is_letter c) then m
      else add_PoI_int m (x + i, y) priority level Dot.Round (Dot.Letter c)
    ) m (String.to_seq str)


let fold_within m max start endv f =
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
          let m = f m current in
          aux m (1 + current) in
      aux m start

(* Iterate from start_x to end_x, but within the map. *)
let fold_within_x m start_x end_x (f : t -> int -> t) =
  let max_x = m.size.x - 1 in
  fold_within m max_x start_x end_x f

(* Same, but for start_y to end_y. *)
let fold_within_y m start_y end_y (f : t -> int -> t) =
  let max_y = m.size.y - 1 in
  fold_within m max_y start_y end_y f


let add_line m xy xy'
    ?(priority = Structures.Medium) ?(non_core_priority = Structures.VeryLow)
    ?(style = Adaptive) ?(non_core_style = Some Adaptive) ?(level = 0) color =
  (* Looking for special cases. *)
  let special_cases =
    let (x, y) = convert_coordinates_from_real xy in
    let (x', y') = convert_coordinates_from_real xy' in
    if x = x' && y = y' then
      let shape = map_adaptive Dot.Round style in
      let m = add_PoI_int m (x, y) priority level shape color in
      Some m
    else if x = x' (* y <> y' *) then
      let m =
        let (y, y') = if y > y' then (y', y) else (y, y') in
        fold_within_y m y y' (fun m yc ->
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
        fold_within_x m x x' (fun m xc ->
          let shape =
            let shape =
              if xc = x then Dot.Half_circle Dot.West
              else if xc = x' then Dot.Half_circle Dot.East
              else Dot.Square in
            map_adaptive shape style in
          add_PoI_int m (xc, y) priority level shape color) in
      Some m
    else if x < 0 && x' < 0 then Some m
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
    let (end_x, end_y) = convert_coordinates_from_real (x', y') in
    let rec aux m (xc, yc) =
      if (xc, yc) = (end_x, end_y) || not (within_bounds m (xc, yc)) then m
      else
        let draw m xy is_core adaptive_style =
          let draw style =
            let shape = map_adaptive adaptive_style style in
            add_PoI_int m xy priority level shape color in
          if is_core then draw style
          else
            match non_core_style with
            | None -> m
            | Some style -> draw style in
        let y' = y_at (xc + 1) in
        let yc' = next_yt yc in
        let x' = x_at yc' in
        let distance_x = x' -. Float.of_int xc in
        assert (distance_x >= 0.) ;
        let distance_y = Float.abs (y' -. Float.of_int xc) in
        if distance_x > distance_y then
          (* The smallest distance is to go to the right. *)
          let is_core = true (* TODO *) in
          let m = draw m (xc, yc) is_core (Dot.Half_circle Dot.West) in
          let xc = xc + 1 in
          let is_core = true (* TODO *) in
          let m = draw m (xc, yc) is_core (Dot.Half_circle Dot.East) in
          aux m (xc, yc)
        else
          (* The smallest distance is to go above or below. *)
          let is_core = true (* TODO *) in
          let dir = if slope > 0. then Dot.North else Dot.South in
          let m = draw m (xc, yc) is_core (Dot.Half_circle dir) in
          let is_core = true (* TODO *) in
          let m = draw m (xc, yc') is_core (Dot.Half_circle (Shape.uturn dir)) in
          aux m (xc, yc')
    in
    (* Drawing as a dot the extremities. *)
    let draw_extremes m xy =
      let shape = map_adaptive Dot.Round style in
      add_PoI_int m xy priority level shape color in
    let start_xy =
      let xy =
        if x < 0. then (0., y_at 0)
        else (x, y) in
      convert_coordinates_from_real xy in
    (* TODO: As is, if the extremes are out of bounds, then the drawn extremeties will be
      displayed as half-circle instead of quarters or square. *)
    (* TODO: As is, if the x-coordinate of the leftbound extremity is out of bounds, but
      that the line eventually becomes in bounds, then it won't be displayed. *)
    let m = draw_extremes m start_xy in
    let m = draw_extremes m (end_x, end_y) in
    (* Drawing in between. *)
    aux m start_xy

let add_polygon m xyl
    ?(border_priority = Structures.Low) ?(inner_priority = Structures.VeryLow)
    ?(border_style = Adaptive) ?(inner_style = Some Square)
    ?(border_level = 0) ?(inner_level = 0) border_color ?(inner_color = border_color) () =
  let (x0, y0) =
    match xyl with
    | xy :: _ -> xy
    | [] -> failwith "Empty list given to add_polygon" in
  (* Draw inner *)
  let m =
    match inner_style with
    | None -> m
    | Some inner_style ->
      let ((min_x, max_x), (min_y, max_y)) =
        List.fold_left (fun ((min_x, max_x), (min_y, max_y)) (x, y) ->
          let min_x = min min_x x in
          let max_x = max max_x x in
          let min_y = min min_y y in
          let max_y = max max_y y in
          ((min_x, max_x), (min_y, max_y))) ((x0, x0), (y0, y0)) xyl in
        let min_x = Float.to_int min_x in
        let max_x = Float.to_int max_x + 1 in
        let min_y = Float.to_int min_y in
        let max_y = Float.to_int max_y + 1 in
        let inner_shape = map_adaptive Dot.Square inner_style in
        fold_within_x m min_x max_x (fun m x ->
          fold_within_y m min_y max_y (fun m y ->
            let is_in = (* TODO *) false in
            if is_in then
              add_PoI_int m (x, y) inner_priority inner_level inner_shape inner_color
          else m)) in
  (* Draw border *)
  let line m xy1 xy2 =
    add_line m xy1 xy2 ~priority:border_priority ~style:border_style ~non_core_style:None
      ~level:border_level border_color in
  let rec aux m = function
    | xy1 :: xy2 :: xyl ->
      let m = line m xy1 xy2 in
      aux m (xy2 :: xyl)
    | xyn :: [] -> line m (x0, y0) xyn
    | [] -> assert false in
  aux m xyl

