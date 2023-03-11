
open Js_of_ocaml
open Js_of_ocaml_lwt


(* Size in pixel of a single LEGO cell. *)
let pixel_stud = 30

(* Height in pixel of a stud. *)
let pixel_stud_height = pixel_stud / 4

(* Approximate distance between the screen and the user, in pixel.
  The value may be underestimated to exagerate the effect. *)
let distance_to_user = 200

let shadow_angle = 135.
let shadow_distance = Float.of_int pixel_stud /. 2.5

(* Some generic DOM functions *)

let window = Dom_html.window
let document = window##.document


type node = Dom.element Js.t
type nodeHTML = Dom_html.element Js.t
type nodeSVG = Dom_svg.element Js.t

let setAttributes elem l =
  List.iter (fun (key, value) ->
    ignore (elem##setAttribute (Js.string key) value)) l

let namespace_SVG = Js.string "http://www.w3.org/2000/svg"

let setSVGAttributes (elem : nodeSVG) l =
  List.iter (fun (key, value) ->
    ignore (elem##setAttributeNS namespace_SVG (Js.string key) value)) l

let createSVGElement kind attributes : nodeSVG =
  let element : nodeSVG =
    Dom_svg.createElement Dom_svg.document kind in
  setSVGAttributes element attributes ;
  element

let rec clear_node n =
  match Js.Opt.to_option n##.firstChild with
  | Some c ->
    ignore (n##removeChild c) ;
    clear_node n
  | None -> ()


let coerce_HTML_to_SVG (node : nodeHTML) : nodeSVG = Js.Unsafe.coerce node (* TODO *)
let coerce_to_SVG (node : node) : nodeSVG = Js.Unsafe.coerce node (* TODO *)
let coerce_node node : node =
  Js.Opt.get (Dom.CoerceTo.element node) (fun () ->
    invalid_arg "coerce_node")

module IMap = Map.Make (Structures.IntOrder)

(* Each levels is divided into four sublevels, each containing an SVG layer. *)
type sub_levels = {
  objects : nodeSVG (* The main objects of this level. *) ;
  shadows : nodeSVG (* Shadows drawn directly on these objects, or any substractive light. *) ;
  lights : nodeSVG (* Additive lights added directly on these objects. *) ;
  passing_through : nodeSVG (* Objects with volume that pass through the current level. *)
}

type sub_level_type =
  | Objects
  | Shadows
  | Lights
  | PassingThrough

type t = {
  svg : nodeSVG (* The main svg element. *) ;
  levels : sub_levels IMap.t ref (* A group for each level. *) ;
  size : (int * int) ref (* The current size of the whole canvas. *) ;
  min_coord : (int * int) ref (* The coordinate of the minimum currently displayed cell. *)
}
(* Note that the cell (0, 0) is the center for the perspective, not the minimum cell.
  The minimum cell has negative coordinates. *)
(* The level of the top of a stud is 1, a usual LEGO brick without stud is 6, a usual tile is 2. *)


let get_sub_level sub_level = function
  | Objects -> sub_level.objects
  | Shadows -> sub_level.shadows
  | Lights -> sub_level.lights
  | PassingThrough -> sub_level.passing_through


let iter f canvas =
  let (min_x, min_y) = !(canvas.min_coord) in
  let (s_x, s_y) = !(canvas.size) in
  for x = 0 to s_x do
    for y = 0 to s_y do
      f (min_x + x, min_y + y)
    done
  done


let init_svg () : nodeSVG =
  let id = Js.string "map" in
  Js.Opt.get (Js.Opt.map (Dom_html.document##getElementById id) coerce_HTML_to_SVG) (fun () ->
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

let create_sub_levels level =
  let create name =
    createSVGElement "g" [("id", Js.string (Printf.sprintf "%s-%d" name level))]
  in {
    objects = create "level" ;
    shadows = create "shadows" ;
    lights = create "lights" ;
    passing_through = create "volumes"
  }

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
          ^ " text-anchor: middle; dominant-baseline: central; }"
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
          ("gradientTransform", Js.string (Printf.sprintf "rotate(%g,.5,.5)" shadow_angle))
        ] in
      let stop1 =
        createSVGElement "stop" [
          ("offset", Js.string "0") ;
          ("stop-color", Js.string "white") ;
          ("stop-opacity", Js.string "1")
        ] in
      let stop2 =
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
  let sub_level = create_sub_levels 0 in
  Dom.appendChild r.svg sub_level.objects ;
  Dom.appendChild r.svg sub_level.shadows ;
  Dom.appendChild r.svg sub_level.lights ;
  Dom.appendChild r.svg sub_level.passing_through ;
  r.levels := IMap.add 0 sub_level !(r.levels) ;
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
    setSVGAttributes r.svg [
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
  IMap.iter (fun _level sub_level ->
      clear_node sub_level.objects ;
      clear_node sub_level.shadows ;
      clear_node sub_level.lights ;
      clear_node sub_level.passing_through
    ) !(canvas.levels)


(* Fetch the group corresponding to the given level. *)
let get_level canvas ?(sub_level=Objects) level =
  let rec aux level =
    assert (level >= 0) ;
    match IMap.find_opt level !(canvas.levels) with
    | Some sub_level -> sub_level
    | None ->
      (* Creating all previous levels and adding them before the current level. *)
      ignore (aux (level - 1)) ;
      (* Adding the current level. *)
      let sub_level = create_sub_levels level in
      Dom.insertBefore canvas.svg sub_level.objects Js.Opt.empty ;
      Dom.insertBefore canvas.svg sub_level.shadows Js.Opt.empty ;
      Dom.insertBefore canvas.svg sub_level.lights Js.Opt.empty ;
      Dom.insertBefore canvas.svg sub_level.passing_through Js.Opt.empty ;
      canvas.levels := IMap.add level sub_level !(canvas.levels) ;
      sub_level in
  let sub_levels = aux level in
  get_sub_level sub_levels sub_level

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

(* Convert a angle in degree to an angle in radian. *)
let to_radian angle = angle *. Float.pi /. 180.

(* Given a center point, an angle, and a distance, compute the coordinates of the point
  at this distance from the center point following this angle. *)
let angular_from (x, y) angle distance =
  let angle = to_radian angle in
  (x +. distance *. cos angle, y +. distance *. sin angle)


(* Basic drawing functions. *)
module DrawBasic : sig

(* Draw a rectangle filling the given cell and level.
  The size argument enables it to fill that many cells.
  If the proportion is less than 1., its borders will be shrinked by that many.
  We can also specify a rotation in degrees.
  Finally, the darken boolean can be specified to make the color darker.
  The accumulator is a function that gets the raw nodes being drawn. *)
val draw_rectangle : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?size:(int * int) -> ?proportion:float -> ?rotation:float -> ?darken:bool -> ?accumulator:(nodeSVG -> unit) -> Dot.color -> unit

(* Same, but for a circle. *)
val draw_circle : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?diameter:int -> ?proportion:float -> ?rotation:float -> ?darken:bool -> ?accumulator:(nodeSVG -> unit) -> Dot.color -> unit

(* Draw a quarter of a circle, with its angle at North-West. *)
val draw_quarter : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?proportion:float -> ?rotation:float -> ?darken:bool -> ?accumulator:(nodeSVG -> unit) -> Dot.color -> unit

(* Draw a shape like the LEGO half-circle of a circle, with its straight border Noth. *)
val draw_half_circle : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?proportion:float -> ?rotation:float -> ?darken:bool -> ?accumulator:(nodeSVG -> unit) -> Dot.color -> unit

(* Draw the halo due to the surface reflection of the angle at the side of a circular dot. *)
val draw_side_halo : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?diameter:int -> ?proportion:float -> ?accumulator:(nodeSVG -> unit) -> Dot.color -> unit

(* Draw the halo that can be seen underneath a transparent circle tile.
  This halo is probably caused by the reflection of the side halo above on the bottom of the
  transparent tile. *)
val draw_halo : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?diameter:int -> ?proportion:float -> ?accumulator:(nodeSVG -> unit) -> Dot.color -> unit

(* Draw the cardioid due to the inner reflection of light that can be seen underneath
  a transparent circle tile. *)
val draw_cardioid : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?diameter:int -> ?proportion:float -> ?accumulator:(nodeSVG -> unit) -> Dot.color -> unit

(* This function is to draw shadows.
  It takes as argument two pairs of angle and distance: these correspond to the left and right
  extreme points of the shape.
  It returns an accumulator that will copy any given shape to the shadow. *)
val draw_shadow : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?angle_left:float -> float -> ?angle_right:float -> float -> ?color:Dot.color -> (nodeSVG -> unit)

end = struct

let pixel_stud = Float.of_int pixel_stud

let print_float v = Js.string (Printf.sprintf "%g" v)

let transform_rotate = function
  | 0. -> []
  | rotation -> [("transform", Js.string (Printf.sprintf "rotate(%g)" rotation))]

(* Prepare the drawing coordinates and colors. *)
let draw_figure canvas (x, y) level ?(sub_level=Objects) ?(size=(1, 1))
    ?(rotation=0.) ?(darken=false) ?(lighten=false)
    ?(accumulator=fun _ -> ()) color build_nodes =
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
  let level = get_level canvas ~sub_level:sub_level level in
  List.iter (Dom.appendChild level) l ;
  List.iter accumulator l ;
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

let draw_rectangle canvas xy level ?(sub_level=Objects)
    ?(size=(1, 1)) ?(proportion=1.) ?(rotation=0.) ?(darken=false) ?(accumulator=fun _ -> ()) color =
  draw_figure canvas xy level ~sub_level ~size ~rotation ~darken ~accumulator color
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

let draw_circle canvas xy level ?(sub_level=Objects)
    ?(diameter=1) ?(proportion=1.) ?(rotation=0.) ?(darken=false) ?(accumulator=fun _ -> ()) color =
  draw_figure canvas xy level ~sub_level ~size:(diameter, diameter)
    ~rotation ~darken ~accumulator color
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

let draw_quarter canvas (x, y) level ?(sub_level=Objects)
    ?(proportion=1.) ?(rotation=0.) ?(darken=false) ?(accumulator=fun _ -> ()) color =
  draw_figure canvas (x, y - 1) level ~sub_level ~size:(2, 2)
    ~rotation ~darken ~accumulator color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let radius = pixel_stud *. proportion in
      let cx = (coordx +. coordx') /. 2. in
      let cy = (coordy +. coordy') /. 2. in
      let (coordx, coordy) = angular_from (cx, cy) 180. radius in
      let (coordx', coordy') = angular_from (cx, cy) 90. radius in
      let path =
        let path_d =
          String.concat " " Printf.[
              sprintf "M %g,%g" coordx' coordy ;
              sprintf "L %g,%g" coordx' coordy' ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius coordx coordy ;
              "z"
            ] in
        createSVGElement "path" ([
            ("d", Js.string path_d) ;
            ("style", Js.string style)
          ] @ transform_rotate rotation) in
      [path]
    )

let draw_half_circle canvas xy level ?(sub_level=Objects)
    ?(proportion=1.) ?(rotation=0.) ?(darken=false) ?(accumulator=fun _ -> ()) color =
  draw_figure canvas xy level ~sub_level ~rotation ~darken ~accumulator color
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

let draw_side_halo canvas xy level ?(sub_level=Lights)
    ?(diameter=1) ?(proportion=1.) ?(accumulator=fun _ -> ()) color =
  draw_figure canvas xy level ~sub_level ~size:(diameter, diameter)
    ~lighten:true ~accumulator color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let radius = pixel_stud *. proportion *. Float.of_int diameter /. 2. in
      let cx = (coordx +. coordx') /. 2. in
      let cy = (coordy +. coordy') /. 2. in
      let (ax, ay) = angular_from (cx, cy) (-75.) radius in
      let (bx, by) = angular_from (cx, cy) (-15.) radius in
      let halo =
        let path_d =
          let larger_radius = radius *. 2. in
          String.concat " " Printf.[
              sprintf "M %g,%g" ax ay ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius bx by ;
              sprintf "A %g %g 0 0 0 %g,%g" larger_radius larger_radius ax ay ;
              "z"
            ] in
        createSVGElement "path" [
            ("d", Js.string path_d) ;
            ("fill-opacity", Js.string ".8") ;
            ("style", Js.string style)
          ] in
        [halo]
    )


let draw_halo canvas xy level ?(sub_level=Lights)
    ?(diameter=1) ?(proportion=1.) ?(accumulator=fun _ -> ()) color =
  draw_figure canvas xy level ~sub_level ~size:(diameter, diameter)
    ~lighten:true ~accumulator color
    (fun (coordx, coordy) (coordx', coordy') _style ->
      let style = "fill:white; fill-opacity:.2; filter:url(#blur_light_moon);" in
      let delta = (1. -. proportion) *. pixel_stud /. 2. in
      let (coordx, coordy) = (coordx +. delta, coordy +. delta) in
      let (coordx', coordy') = (coordx' -. delta, coordy' -. delta) in
      let middlex = (coordx +. coordx') /. 2. in
      let middley = (coordy +. coordy') /. 2. in
      let radius = pixel_stud *. proportion /. 2. in
      let halo =
        let path_d =
          let larger_radius = radius *. 1.5 in
          String.concat " " Printf.[
              sprintf "M %g,%g" middlex coordy ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius coordx' middley ;
              sprintf "A %g %g 0 0 0 %g,%g" larger_radius larger_radius middlex coordy ;
              "z"
            ] in
        createSVGElement "path" [
            ("d", Js.string path_d) ;
            ("style", Js.string style)
          ] in
      [halo]
    )

let draw_cardioid canvas xy level ?(sub_level=Lights)
    ?(diameter=1) ?(proportion=1.) ?(accumulator=fun _ -> ()) color =
  draw_figure canvas xy level ~sub_level ~size:(diameter, diameter)
    ~lighten:true ~accumulator color
    (fun (coordx, coordy) (coordx', coordy') _style ->
      let style = "fill:url(#gradient_cardioid); fill-opacity:.5; filter:url(#blur_light_moon);" in
      let delta = (1. -. proportion) *. pixel_stud /. 2. in
      let (coordx, coordy) = (coordx +. delta, coordy +. delta) in
      let (coordx', coordy') = (coordx' -. delta, coordy' -. delta) in
      let middlex = (coordx +. coordx') /. 2. in
      let middley = (coordy +. coordy') /. 2. in
      let radius = pixel_stud *. proportion /. 2. in
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
        createSVGElement "path" [
            ("d", Js.string path_d) ;
            ("style", Js.string style)
          ] in
      [cardioid]
    )

let draw_shadow canvas xy level ?(sub_level=Shadows)
      ?(angle_left=(-135.)) distance_left ?(angle_right=45.) distance_right ?(color=Dot.Black) =
  let g = createSVGElement "g" [] in
  let shiftx = shadow_distance *. cos (to_radian shadow_angle) in
  let shifty = shadow_distance *. sin (to_radian shadow_angle) in
  draw_figure canvas xy level ~sub_level ~darken:true color
    (fun (coordx, coordy) (coordx', coordy') _style ->
       [g] (* TODO *)
    ) ;
  fun (node : nodeSVG) -> (
    let node1 = node##cloneNode Js._true in
    let node2 = node##cloneNode Js._true in
    let node2 = coerce_to_SVG (coerce_node node2) in
    let transform =
      Js.Opt.get
        (Js.Opt.map (node2##getAttribute (Js.string "transform")) Js.to_string)
        (fun () -> "") in
    let transform = transform ^ Printf.sprintf " translate(%g, %g)" shiftx shifty in
    setSVGAttributes node2 [("transform", Js.string transform)] ;
    Dom.appendChild g node1 ;
    Dom.appendChild g node2
  )

end

(* Drawing LEGO pieces. *)
module Lego = struct

(* Note that the levels in this module differ from the other representation of the level
  in the file: beforehand, it was splited in 2 to accomodate for smaller measurments like
  stud height.  In this module it is meant to be the actual number of plates needed, so
  there is a 2-factor in place there. *)

(* The following two constant converts the levels from the current system to the old one. *)
let default_level = 2
let convert_level level = 2 * level


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
  draw_rectangle canvas xy 0 ~size (remove_letters color) ;
  (* Stud proportion. *)
  let proportion = 0.57 in
  draw_circle canvas xy 0 ~sub_level:PassingThrough ~proportion ~darken:true (remove_letters color) ;
  draw_circle canvas xy 1 ~proportion color ;
  draw_side_halo canvas xy 1 ~sub_level:Lights ~proportion Dot.White

let square_tile canvas xy ?(level=default_level) ?(size=(1, 1)) color =
  let level = convert_level level in
  let rotation = get_rotation_size xy size in
  let proportion = 0.95 in
  draw_rectangle canvas xy (level - 2) ~sub_level:PassingThrough ~size ~proportion ~rotation
    ~darken:true (remove_letters color) ;
  draw_rectangle canvas xy level ~size ~proportion ~rotation color

let round_tile canvas xy ?(level=default_level) ?(diameter=1) color =
  let level = convert_level level in
  let rotation = get_rotation_size ~double_rotation:true xy (diameter, diameter) in
  let proportion = 0.95 in
  if Color.is_transparent color then (
    (* Adding special shades underneath the circle. *)
    let proportion = 0.57 in
    (* Note that we dropped the rotation: the light is always coming from the same direction. *)
    draw_halo canvas xy (level - 1) ~sub_level:Lights ~diameter ~proportion color ;
    draw_cardioid canvas xy (level - 2) ~sub_level:Lights ~diameter ~proportion color
  ) ;
  draw_circle canvas xy (level - 2) ~sub_level:PassingThrough ~diameter ~proportion ~rotation
    ~darken:true (remove_letters color) ;
  draw_circle canvas xy level ~diameter ~proportion ~rotation color ;
  draw_side_halo canvas xy level ~sub_level:Lights ~diameter ~proportion Dot.White

let convert_direction = function
  | Dot.North -> 0.
  | Dot.West -> -90.
  | Dot.South -> 180.
  | Dot.East -> 90.

let quarter_tile canvas xy ?(level=default_level) direction color =
  let level = convert_level level in
  let rotation =
    let size = (1, 1) in
    get_rotation_size xy size in
  let rotation = rotation +. convert_direction direction in
  let proportion = 0.95 in
  draw_quarter canvas xy (level - 2) ~sub_level:PassingThrough ~proportion ~rotation
    ~darken:true (remove_letters color) ;
  draw_quarter canvas xy level ~proportion ~rotation color
  (* if direction = Dot.South then (
    draw_side_halo canvas xy level ~sub_level:Lights ~diameter:2 ~proportion Dot.White
  ) *)

let half_circle_tile canvas xy ?(level=default_level) direction color =
  let level = convert_level level in
  let rotation =
    let size = (1, 1) in
    get_rotation_size xy size in
  let rotation = rotation +. convert_direction direction in
  let proportion = 0.95 in
  draw_half_circle canvas xy (level - 2) ~sub_level:PassingThrough ~proportion ~rotation
    ~darken:true (remove_letters color) ;
  draw_half_circle canvas xy level ~proportion ~rotation color ;
  if direction = Dot.South || direction = Dot.West then
    draw_side_halo canvas xy level ~sub_level:Lights ~proportion Dot.White

end


