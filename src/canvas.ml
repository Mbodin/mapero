
open Js_of_ocaml
open Js_of_ocaml_lwt


(* Size in pixel of a single LEGO cell. *)
let pixel_stud = 30

(* Height in pixel of a stud. *)
let pixel_stud_height = pixel_stud / 4

(* Approximate distance between the screen and the user, in pixel.
  The value may be underestimated to exagerate the effect. *)
let distance_to_user = 200

let shadow_angle = -135


(* Some generic DOM functions *)

let window = Dom_html.window
let document = window##.document

let set_attributes elem l =
  List.iter (fun (key, value) ->
    ignore (elem##setAttribute (Js.string key) value)) l

let createSVGElement kind attributes =
  let element =
    document##createElementNS (Js.string "http://www.w3.org/2000/svg") (Js.string kind) in
  set_attributes element attributes ;
  element

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
      let svg =
        createSVGElement "svg" [
          ("width", Js.string "100%") ;
          ("height", Js.string "100%") ;
          ("xmlns", Js.string "http://www.w3.org/2000/svg") ;
          ("id", id)
        ] in
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
    let style = createSVGElement "style" [] in
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
  (* Adding some filters *)
  let defs =
    let defs = createSVGElement "defs" [] in
    let blur_light_moon =
      let filter =
        createSVGElement "filter" [
          ("id", Js.string "blur_light_moon") ;
          ("style", Js.string "color-interpolation-filters:sRGB") ;
          ("x", Js.string "-0.1") ;
          ("width", Js.string "1.2") ;
          ("y", Js.string "-0.1") ;
          ("height", Js.string "1.2")
        ] in
      let blur =
        createSVGElement "feGaussianBlur" [
          ("stdDeviation", Js.string ".3") ;
        ] in
      Dom.appendChild filter blur ;
      filter in
    Dom.appendChild defs blur_light_moon ;
    let gradient_cardioid =
      let gradient =
        createSVGElement "linearGradient" [
          ("id", Js.string "gradient_cardioid") ;
          ("gradientTransform", Js.string (Printf.sprintf "rotate(%d,.5,.5)" (-shadow_angle)))
        ] in
      let stop1 =
        createSVGElement "stop" [
          ("offset", Js.string "0") ;
          ("stop-color", Js.string "white") ;
          ("stop-opacity", Js.string "1")
        ] in
      let stop1 =
        createSVGElement "stop" [
          ("offset", Js.string ".5") ;
          ("stop-color", Js.string "white") ;
          ("stop-opacity", Js.string "1")
        ] in
      let stop3 =
        createSVGElement "stop" [
          ("offset", Js.string "1") ;
          ("stop-color", Js.string "white") ;
          ("stop-opacity", Js.string "0")
        ] in
      Dom.appendChild gradient stop1 ;
      Dom.appendChild gradient stop2 ;
      Dom.appendChild gradient stop3 ;
      gradient in
    Dom.appendChild defs gradient_cardioid ;
    defs in
  Dom.appendChild r.svg defs ;
  (* Creating a level 0 *)
  let g = createSVGElement "g" [("id", Js.string "level-0")] in
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
      let g = createSVGElement "g" [("id", Js.string (Printf.sprintf "level-%d" level))] in
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

(* Draw a quarter of a circle, with its angle at North-West. *)
val draw_quarter : t -> (int * int) -> int -> ?proportion:float -> ?rotation:float -> ?darken:bool -> Dot.color -> unit

(* Draw a shape like the LEGO half-circle of a circle, with its straight border Noth. *)
val draw_half_circle : t -> (int * int) -> int -> ?proportion:float -> ?rotation:float -> ?darken:bool -> Dot.color -> unit

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
    let (coordx, coordy) = coord in
    let (coordx', coordy') = coord' in
    let element =
      createSVGElement "text" ([
          ("x", print_float ((coordx +. coordx') /. 2.)) ;
          ("y", print_float ((coordy +. coordy') /. 2.))
        ] @ transform_rotate rotation) in
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
      let delta = (1. -. proportion) *. pixel_stud /. 2. in
      let rect =
        createSVGElement "rect" ([
            ("x", print_float (coordx +. delta)) ;
            ("y", print_float (coordy +. delta)) ;
            ("width", print_float ((coordx' -. coordx) *. proportion)) ;
            ("height", print_float ((coordy' -. coordy) *. proportion)) ;
            ("style", Js.string style)
          ] @ transform_rotate rotation) in
      [rect]
    )

let draw_circle canvas (x, y) level
    ?(diameter=1) ?(proportion=1.) ?(rotation=0.) ?(darken=false) color =
  draw_figure canvas (x, y) level ~size:(diameter, diameter) ~rotation ~darken color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let radius = pixel_stud *. Float.of_int diameter /. 2. in
      let circle =
        createSVGElement "circle" ([
            ("cx", print_float ((coordx +. coordx') /. 2.)) ;
            ("cy", print_float ((coordy +. coordy') /. 2.)) ;
            ("r", print_float (radius *. proportion)) ;
            ("style", Js.string style)
          ] @ transform_rotate rotation) in
      [circle]
    )

let draw_quarter canvas (x, y) level
    ?(proportion=1.) ?(rotation=0.) ?(darken=false) color =
  draw_figure canvas (x, y) level ~rotation ~darken color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let delta = (1. -. proportion) *. pixel_stud /. 2. in
      let (coordx, coordy) = (coordx +. delta, coordy +. delta) in
      let (coordx', coordy') = (coordx' -. delta, coordy' -. delta) in
      let radius = pixel_stud *. proportion in
      let path =
        let path_d =
          String.concat " " Printf.[
              sprintf "M %g,%g" coordx coordy ;
              sprintf "L %g,%g" coordx' coordy ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius coordx coordy' ;
              "z"
            ] in
        createSVGElement "path" ([
            ("d", Js.string path_d) ;
            ("style", Js.string style)
          ] @ transform_rotate rotation) in
      [path]
    )

let draw_half_circle canvas (x, y) level
    ?(proportion=1.) ?(rotation=0.) ?(darken=false) color =
  draw_figure canvas (x, y) level ~rotation ~darken color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let delta = (1. -. proportion) *. pixel_stud /. 2. in
      let (coordx, coordy) = (coordx +. delta, coordy +. delta) in
      let (coordx', coordy') = (coordx' -. delta, coordy' -. delta) in
      let middley = (coordy +. coordy') /. 2. in
      let radius = pixel_stud *. proportion /. 2. in
      let path =
        let path_d =
          String.concat " " Printf.[
              sprintf "M %g,%g" coordx coordy ;
              sprintf "L %g,%g" coordx' coordy ;
              sprintf "L %g,%g" coordx' middley ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius coordx middley ;
              "z"
            ] in
        createSVGElement "path" ([
            ("d", Js.string path_d) ;
            ("style", Js.string style)
          ] @ transform_rotate rotation) in
      [path]
    )

let draw_circle_shades canvas (x, y) level
    ?(diameter=1) ?(proportion=1.) ?(rotation=0.) color =
  draw_figure canvas (x, y) level ~size:(diameter, diameter) ~rotation ~lighten:true color
    (fun (coordx, coordy) (coordx', coordy') _style ->
      let style_blur = "fill:white; fill-opacity:.5; filter:url(#blur_light_moon);" in
      let style_cardioid = "fill:url(#gradient_cardioid); fill-opacity:.5; filter:url(#blur_light_moon);" in
      let delta = (1. -. proportion) *. pixel_stud /. 2. in
      let (coordx, coordy) = (coordx +. delta, coordy +. delta) in
      let (coordx', coordy') = (coordx' -. delta, coordy' -. delta) in
      let middlex = (coordx +. coordx') /. 2. in
      let middley = (coordy +. coordy') /. 2. in
      let radius = pixel_stud *. proportion /. 2. in
      let moon =
        let path_d =
          let larger_radius = radius *. 1.5 in
          String.concat " " Printf.[
              sprintf "M %g,%g" middlex coordy ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius coordx' middley ;
              sprintf "A %g %g 0 0 0 %g,%g" larger_radius larger_radius middlex coordy ;
              "z"
            ] in
        createSVGElement "path" ([
            ("d", Js.string path_d) ;
            ("style", Js.string style_blur)
          ] @ transform_rotate rotation) in
      let cardioid =
        let path_d =
          let radiusa = radius *. 1.5 in
          let radiusb = radius *. 2. in
          let ext = radius *. 0.5 in
          let ext_coordx = coordx -. ext in
          let ext_coordy' = coordy' +. ext in
          let pausex = middlex -. radius /. sqrt 2. in
          let pausey = middley +. radius /. sqrt 2. in
          String.concat " " Printf.[
              sprintf "M %g,%g" ext_coordx middley ;
              sprintf "A %g %g 0 0 0 %g,%g" radiusa radiusa pausex pausey ;
              sprintf "A %g %g 0 0 0 %g,%g" radiusa radiusa middlex ext_coordy' ;
              sprintf "A %g %g 0 0 1 %g,%g" radiusb radiusb ext_coordx middley ;
              "z"
            ] in
        createSVGElement "path" ([
            ("d", Js.string path_d) ;
            ("style", Js.string style_cardioid)
          ] @ transform_rotate rotation) in
      [moon; cardioid]
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

let remove_letters = function
  | Dot.Letter _ -> Dot.White
  | c -> c

let base_plate canvas xy ?(size=(1, 1)) color =
  (* Stud proportion. *)
  let proportion = 0.57 in
  draw_rectangle canvas xy 0 ~size (remove_letters color) ;
  draw_circle canvas xy 0 ~proportion ~darken:true (remove_letters color) ;
  draw_circle canvas xy 1 ~proportion color

let square_tile canvas xy ?(level=2) ?(size=(1, 1)) color =
  let rotation = get_rotation_size xy size in
  let proportion = 0.95 in
  draw_rectangle canvas xy (level - 2) ~size ~proportion ~rotation ~darken:true
    (remove_letters color) ;
  draw_rectangle canvas xy level ~size ~proportion ~rotation color

let round_tile canvas xy ?(level=2) ?(diameter=1) color =
  let rotation = get_rotation_size ~double_rotation:true xy (diameter, diameter) in
  let proportion = 0.95 in
  if Color.is_transparent color then (
    (* Adding special shades underneath the circle. *)
    let proportion = 0.57 in
    (* Note that we dropped the rotation: the light is always coming from the same direction. *)
    draw_circle_shades canvas xy (level - 1) ~diameter ~proportion color
  ) ;
  draw_circle canvas xy (level - 2) ~diameter ~proportion ~rotation ~darken:true
    (remove_letters color) ;
  draw_circle canvas xy level ~diameter ~proportion ~rotation color

let convert_direction = function
  | Dot.North -> 0.
  | Dot.West -> -90.
  | Dot.South -> 180.
  | Dot.East -> 90.

let quarter_tile canvas xy ?(level=2) direction color =
  let rotation =
    let size = (1, 1) in
    get_rotation_size xy size in
  let rotation = rotation +. convert_direction direction in
  let rotation = rotation +. 90. in (* The function draw_quarter is drawing it West by default. *)
  let proportion = 0.95 in
  draw_quarter canvas xy (level - 2) ~proportion ~rotation ~darken:true
    (remove_letters color) ;
  draw_quarter canvas xy level ~proportion ~rotation color

let half_circle_tile canvas xy ?(level=2) direction color =
  let rotation =
    let size = (1, 1) in
    get_rotation_size xy size in
  let rotation = rotation +. convert_direction direction in
  let proportion = 0.95 in
  draw_half_circle canvas xy (level - 2) ~proportion ~rotation ~darken:true
    (remove_letters color) ;
  draw_half_circle canvas xy level ~proportion ~rotation color

end


