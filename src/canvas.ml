
open Js_of_ocaml
open Js_of_ocaml_lwt


(* Size in pixel of a single LEGO cell. *)
let pixel_stud = 30

(* Height in pixel of a stud. *)
let pixel_stud_height = pixel_stud / 4

(* Approximate distance between the screen and the user, in pixel.
  The value may be underestimated to exagerate the effect. *)
let distance_to_user = 200


(* Some generic DOM functions *)

let window = Dom_html.window
let document = window##.document

let createSVGElement kind =
  document##createElementNS (Js.string "http://www.w3.org/2000/svg") (Js.string kind)

let set_attributes elem l =
  List.iter (fun (key, value) ->
    ignore (elem##setAttribute (Js.string key) value)) l

let rec clear_node n =
  match Js.Opt.to_option n##.firstChild with
  | Some c ->
    ignore (n##removeChild c) ;
    clear_node n
  | None -> ()



type node = Dom_html.element Js.t

module IMap = Map.Make (Structures.IntOrder)

type t = {
  svg : node ; (* The main svg element. *)
  levels : node IMap.t ref ; (* A group for each level. *)
  size : (int * int) ref (* The current size of the whole canvas. *) ;
  min_coord : (int * int) ref (* The coordinate of the minimum currently displayed cell. *)
}
(* Note that the cell (0, 0) is the center for the perspective, not the minimum cell.
  The minimum cell has negative coordinates. *)
(* The level of the top of a stud is 1, a usual LEGO brick without stud is 6, a usual tile is 2. *)


let iter f canvas =
  let (min_x, min_y) = !(canvas.min_coord) in
  let (s_x, s_y) = !(canvas.size) in
  for x = 0 to s_x do
    for y = 0 to s_y do
      f (min_x + x, min_y + y)
    done
  done


let init_svg () =
  let id = Js.string "map" in
  Js.Opt.get (Dom_html.document##getElementById id) (fun () ->
      (* No map found: creating one. *)
      let svg = createSVGElement "svg" in
      set_attributes svg [
          ("width", Js.string "100%") ;
          ("height", Js.string "100%") ;
          ("xmlns", Js.string "http://www.w3.org/2000/svg") ;
          ("id", id)
        ] ;
      Dom.appendChild document##.body svg ;
      svg)

let window_x, window_y =
  (fun _ -> window##.innerWidth),
  (fun _ -> window##.innerHeight)

let init on_change =
  let xy = ref (0, 0) in
  let min_coord = ref (0, 0) in
  let update_xy () =
    let round_to_stud v =
      v / pixel_stud + if v mod pixel_stud = 0 then 0 else 1 in
    let x, y = round_to_stud (window_x ()), round_to_stud (window_y ()) in
    xy := (x, y) ;
    let compute_min v =
      -v/2 - (v mod 2) in
    min_coord := (compute_min x, compute_min y) in
  let r = {
    svg = init_svg () ;
    levels = ref IMap.empty ;
    size = xy ;
    min_coord = min_coord
  } in
  (* Adding basic styles *)
  let style =
    let style = createSVGElement "style" in
    let styles =
      let content =
        String.concat "\n\t" [
          "* { transform-origin: center; transform-box: fill-box; }" ;
          "text { font-family: \"Noto\", sans-serif; font-weight: bold;"
          ^ "text-anchor: middle; dominant-baseline: middle; }"
        ] in
      document##createTextNode (Js.string content) in
    Dom.appendChild style styles ;
    style in
  Dom.appendChild r.svg style ;
  (* Creating a level 0 *)
  let g = createSVGElement "g" in
  set_attributes g [("id", Js.string "level-0")] ;
  Dom.appendChild r.svg g ;
  r.levels := IMap.add 0 g !(r.levels) ;
  let on_change _ =
    update_xy () ;
    let (min_coord_x, min_coord_y) = !(r.min_coord) in
    let (size_x, size_y) = !(r.size) in
    let (shift_coord_x, shift_coord_y) =
      let shift w =
        let m = w mod pixel_stud in
        if m = 0 then 0
        else pixel_stud - (w mod pixel_stud) in
      (shift (window_x ()), shift (window_y ())) in
    let convert v = string_of_int (v * pixel_stud) in
    set_attributes r.svg [
        ("width", Js.string (convert size_x)) ;
        ("height", Js.string (convert size_y)) ;
        ("style", Js.string
          (Printf.sprintf "position: absolute; left: -%dpx; top: -%dpx;"
            shift_coord_x shift_coord_y)) ;
        ("viewBox", Js.string
           (String.concat " "
             (List.map convert
                [min_coord_x; min_coord_y;
                 size_x; size_y])))
      ] ;
    on_change r ;
    Js._true in
  window##.onresize := Dom_html.handler on_change ;
  ignore (on_change ()) ;
  r

let clear canvas =
  IMap.iter (fun _level node -> clear_node node) !(canvas.levels)


(* Fetch the group corresponding to the given level. *)
let get_level canvas level =
  let rec aux level =
    assert (level >= 0) ;
    match IMap.find_opt level !(canvas.levels) with
    | Some g -> g
    | None ->
      (* Creating all previous levels and adding them before the current level. *)
      ignore (aux (level - 1)) ;
      (* Adding the current level. *)
      let g = createSVGElement "g" in
      set_attributes g [("id", Js.string (Printf.sprintf "level-%d" level))] ;
      Dom.insertBefore canvas.svg g Js.Opt.empty ;
      canvas.levels := IMap.add level g !(canvas.levels) ;
      g in
  aux level

(* Get the coordinates on screen of the given coordinate at the given level.
  Because of perspective, they can be shifted. *)
let get_perspective =
  let pixel_stud = Float.of_int pixel_stud in
  let pixel_stud_height = Float.of_int pixel_stud_height in
  let distance_to_user = Float.of_int distance_to_user in
  let perspective_shift =
    let perspective_shift_memoise = ref IMap.empty in
    fun v ->
      match IMap.find_opt v !perspective_shift_memoise with
      | Some r -> r
      | None ->
        (* sin (arctan x) = x / sqrt (1 + x^2)
          It represents the shift due to perspective. *)
        let sin_arctan v = v /. sqrt (1. +. v *. v) in
        let r =
          let v = Float.of_int v in
          sin_arctan (v /. distance_to_user) *. pixel_stud_height in
        perspective_shift_memoise := IMap.add v r !perspective_shift_memoise ;
        r in
  fun (x, y) level ->
    let compute v =
      let shift = perspective_shift v in
      let v = Float.of_int v in
      v *. pixel_stud +. shift *. Float.of_int level in
    (compute x, compute y)


(* Basic drawing functions. *)
module DrawBasic : sig

(* Draw a rectangle filling the given cell and level.
  The size argument enables it to fill that many cells.
  If the proportion is less than 1., its borders will be shrinked by that many.
  We can also specify a rotation in degrees.
  Finally, the darken boolean can be specified to make the color darker. *)
val draw_rectangle : t -> (int * int) -> int -> ?size:(int * int) -> ?proportion:float -> ?rotation:float -> ?darken:bool -> Dot.color -> unit

(* Same, but for a circle. *)
val draw_circle : t -> (int * int) -> int -> ?diameter:int -> ?proportion:float -> ?rotation:float -> ?darken:bool -> Dot.color -> unit

(* Draw the shades that can be seen underneath a transparent circle tile. *)
val draw_circle_shades : t -> (int * int) -> int -> ?diameter:int -> ?proportion:float -> ?rotation:float -> Dot.color -> unit

end = struct

let pixel_stud = Float.of_int pixel_stud

let print_float v = Js.string (Printf.sprintf "%g" v)

let transform_rotate = function
  | 0. -> []
  | rotation -> [("transform", Js.string (Printf.sprintf "rotate(%g)" rotation))]

(* Prepare the drawing coordinates and colors. *)
let draw_figure canvas (x, y) level ?(size=(1, 1)) ?(rotation=0.) ?(darken=false) ?(lighten=false) color build_nodes =
  let (dx, dy) = size in
  let x', y' = x + dx, y + dy in
  let coord = get_perspective (x, y) level in
  let coord' = get_perspective (x', y') level in
  let style =
    let (r, g, b) = Color.to_rgb color in
    let (r, g, b) =
      let dark v = (v * 3) / 4 in
      if darken then (dark r, dark g, dark b) else (r, g, b) in
    let (r, g, b) =
      let light v = (2 * v + 255) / 3 in
      if lighten then (light r, light g, light b) else (r, g, b) in
    Printf.sprintf "fill:rgb(%d,%d,%d);" r g b in
  let style =
    if Color.is_transparent color then
      style ^ "fill-opacity:.5;"
    else style in
  let l = build_nodes coord coord' style in
  let level = get_level canvas level in
  List.iter (Dom.appendChild level) l ;
  (* Dealing with special cases. *)
  match color with
  | Letter c ->
    let element = createSVGElement "text" in
    let (coordx, coordy) = coord in
    let (coordx', coordy') = coord' in
    set_attributes element ([
        ("x", print_float ((coordx +. coordx') /. 2.)) ;
        ("y", print_float ((coordy +. coordy') /. 2.))
      ] @ transform_rotate rotation) ;
    let text = document##createTextNode (Js.string c) in
    Dom.appendChild element text ;
    Dom.appendChild level element
  | Satin_trans_clear ->
    (* TODO: Try to reproduce this effect. *)
    ()
  | _ -> ()

let draw_rectangle canvas (x, y) level
    ?(size=(1, 1)) ?(proportion=1.) ?(rotation=0.) ?(darken=false) color =
  draw_figure canvas (x, y) level ~size ~rotation ~darken color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let rect = createSVGElement "rect" in
      let delta = (1. -. proportion) *. pixel_stud /. 2. in
      set_attributes rect ([
          ("x", print_float (coordx +. delta)) ;
          ("y", print_float (coordy +. delta)) ;
          ("width", print_float ((coordx' -. coordx) *. proportion)) ;
          ("height", print_float ((coordy' -. coordy) *. proportion)) ;
          ("style", Js.string style)
        ] @ transform_rotate rotation) ;
      [rect]
    )

let draw_circle canvas (x, y) level
    ?(diameter=1) ?(proportion=1.) ?(rotation=0.) ?(darken=false) color =
  draw_figure canvas (x, y) level ~size:(diameter, diameter) ~rotation ~darken color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let circle = createSVGElement "circle" in
      let radius = pixel_stud *. Float.of_int diameter /. 2. in
      set_attributes circle ([
          ("cx", print_float ((coordx +. coordx') /. 2.)) ;
          ("cy", print_float ((coordy +. coordy') /. 2.)) ;
          ("r", print_float (radius *. proportion)) ;
          ("style", Js.string style)
        ] @ transform_rotate rotation) ;
      [circle]
    )

let draw_circle_shades canvas (x, y) level
    ?(diameter=1) ?(proportion=1.) ?(rotation=0.) color =
  draw_figure canvas (x, y) level ~size:(diameter, diameter) ~rotation ~lighten:true color
    (fun (coordx, coordy) (coordx', coordy') style ->
       [] (* TODO *)
    )

end

(* Drawing LEGO pieces. *)
module Lego = struct

open DrawBasic

module I2Map = Map.Make (Structures.ProductOrderedType (Structures.IntOrder) (Structures.IntOrder))

(* A global map of small rotations, to be kept identical between two frames. *)
let get_rotation =
  let rotations = ref I2Map.empty in fun xy ->
  match I2Map.find_opt xy !rotations with
  | Some r -> r
  | None ->
    let r = (Random.float 8.) -. 4. in
    rotations := I2Map.add xy r !rotations ;
    r

(* We only rotate dots. *)
let get_rotation_size ?(double_rotation=false) xy size =
  if size = (1, 1) then
    let r = get_rotation xy in
    if double_rotation then 2. *. r else r
  else 0.


let base_plate canvas xy ?(size=(1, 1)) color =
  (* Stud proportion. *)
  let proportion = 0.57 in
  draw_rectangle canvas xy 0 ~size color ;
  draw_circle canvas xy 0 ~proportion ~darken:true color ;
  draw_circle canvas xy 1 ~proportion color

let square_tile canvas xy ?(level=2) ?(size=(1, 1)) color =
  let rotation = get_rotation_size xy size in
  let proportion = 0.92 in
  draw_rectangle canvas xy (level - 2) ~size ~proportion ~rotation ~darken:true color ;
  draw_rectangle canvas xy level ~size ~proportion ~rotation color

let round_tile canvas xy ?(level=2) ?(diameter=1) color =
  let rotation = get_rotation_size ~double_rotation:true xy (diameter, diameter) in
  let proportion = 0.95 in
  if Color.is_transparent color then
    (* Adding special shades underneath the circle. *)
    draw_circle_shades canvas xy (level - 1) ~diameter ~proportion ~rotation color ;
  draw_circle canvas xy (level - 2) ~diameter ~proportion ~rotation ~darken:true color ;
  draw_circle canvas xy level ~diameter ~proportion ~rotation color

end


