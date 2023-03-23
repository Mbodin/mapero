
type action =
  | NoAction
  | Link of string
  | Action of (unit -> unit)

type large_piece = {
  start : int * int ;
  dim : int * int ;
  color : Dot.color ;
  sticker : string
}

type piece =
  | Dot of Dot.t
  | Plate of large_piece
  | Tile of large_piece
  | Round of large_piece
  | Occupied

type t =
  (Dot.color list (* The list of plates and their color below the main piece. The first element is topmost. *)
   * int (* The length of the previous list. *)
   * (piece * action) option) array array

let to_matrix m =
  Array.map (Array.map (fun (l, n, o) -> (assert (n = List.length l) ; (List.rev l, o)))) m

let empty (x, y) = Array.make_matrix x y ([], 0, None)

let get_size m =
  let sx = Array.length m in
  if sx = 0 then (0, 0)
  else (sx, Array.length (m.(0)))

(* Return the color of a piece.
  Can't be called on Occupied. *)
let piece_color = function
  | Dot (_, c) -> c
  | Plate p | Tile p | Round p -> p.color
  | Occupied -> assert false

(* Return the size of a piece. *)
let piece_size = function
  | Dot _ -> (1, 1)
  | Plate p | Tile p | Round p -> p.dim
  | Occupied -> (0, 0)


(* Build an iteration sequence from a pair of points. *)
let build_sequences m (x1, y1) (x2, y2) =
  let (x1, x2) = if x1 < x2 then (x1, x2) else (x2, x1) in
  let (y1, y2) = if y1 < y2 then (y1, y2) else (y2, y1) in
  let x1 = max x1 0 in
  let y1 = max y1 0 in
  let (x2, y2) =
    let (sx, sy) = get_size m in
    (min x2 (sx - 1), min y2 (sy - 1)) in
  let x2 = max x1 x2 in
  let y2 = max y1 y2 in
  let seqx = Seq.init (1 + x2 - x1) (fun x -> x + x1) in
  let seqy = Seq.init (1 + y2 - y1) (fun y -> y + y1) in
  (seqx, seqy)

(* Determine the level needed to be above everything drawn between these two points. *)
let get_level m (x1, y1) (x2, y2) =
  let (seqx, seqy) = build_sequences m (x1, y1) (x2, y2) in
  Seq.fold_left (fun x i ->
    Seq.fold_left (fun y i ->
      let (_, j, _) = m.(x).(y) in
      max i j
    ) i seqy
  ) 0 seqx

(* Leverage the zone defined by the rectangle according to the given level. *)
let flatten_to_level m (x1, y1) (x2, y2) level c =
  let (seqx, seqy) = build_sequences m (x1, y1) (x2, y2) in
  (* Draw the block, completing the levels if needed. *)
  Seq.iter (fun x ->
    Seq.iter (fun y ->
      let (l, i, _) = m.(x).(y) in
      assert (i <= level) ;
      let l = List.init (level - i) (fun _ -> c) @ l in
      m.(x).(y) <- (l, level, None)
    ) seqy
  ) seqx

let add_block m (x1, y1) (x2, y2) c =
  let level = 1 + get_level m (x1, y1) (x2, y2) in
  flatten_to_level m (x1, y1) (x2, y2) level c ;
  m

(* Very similar to add_block, but without adding a level. *)
let flatten m (x1, y1) (x2, y2) c =
  let level = get_level m (x1, y1) (x2, y2) in
  flatten_to_level m (x1, y1) (x2, y2) level c

let add_piece m (x, y) ?(action=NoAction) p =
  if p = Occupied then m
  else
    let c = piece_color p in
    let (sx, sy) = piece_size p in
    flatten m (x, y) (x + sx, y + sy) c ;
    let (seqx, seqy) = build_sequences m (x, y) (x + sx, y + sy) in
    Seq.iter (fun x ->
      Seq.iter (fun y ->
        let (l, i, _) = m.(x).(y) in
        m.(x).(y) <- (l, i, Some (Occupied, action))
      ) seqy
    ) seqx ;
    let (l, i, _) = m.(x).(y) in
    m.(x).(y) <- (l, i, Some (p, action)) ;
    m

let add_text m (x, y) ?(action=NoAction) txt =
  let c = Dot.White in
  (* TODO: Split according to Unicode characters, not just bytes. *)
  flatten m (x, y) (x + String.length txt, y) c ;
  let seqx = Seq.init (String.length txt) (fun dx -> x + dx) in
  Seq.iteri (fun d x ->
    let (l, i, _) = m.(x).(y) in
    let letter = (Dot.Round, Dot.Letter (Printf.sprintf "%c" (txt.[d]))) in
    m.(x).(y) <- (l, i, Some (Dot letter, action))
  ) seqx ;
  m

