
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


let add_PoI_int m xy priority level style color =
  let shape =
    match style with
    | Round | Adaptive -> Dot.Round
    | Square -> Dot.Square in
  let v = (priority, (level, (shape, color))) in
  add m xy v

let add_PoI m xy ?(priority = Structures.High) ?(level = 0) ?(style = Round) color =
  add_PoI_int m (convert_real_coordinates xy) priority level style color


let is_letter c = (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z')

let add_text m (x, y) ?(only_letters = true)
    ?(priority = Structures.VeryHigh) ?(level = max_int) str =
  Seq.fold_lefti (fun m i c ->
      if only_letters and not (is_letter c) then m
      else add_PoI_int m (x + i, y) priority level Round (Dot.Letter c)
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
    if x = x' then
      let m =
        fold_within_y m y y' (fun y ->
          add_PoI_int m (x, y) priority level style color) in
      Some m
    else if y = y' then
      let m =
        fold_within_x m x x' (fun x ->
          add_PoI_int m (x, y) priority level style color) in
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
    (* TODO *)

