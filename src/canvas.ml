
open Js_of_ocaml
open Tyxml
open Js_of_ocaml_tyxml
open Tyxml_js


(* Size in pixel of a single LEGO cell. *)
let pixel_stud = 30

(* Height in pixel of a stud. *)
let pixel_stud_height = pixel_stud / 4

(* Approximate distance between the screen and the user, in pixel.
  The value may be underestimated to exagerate the effect. *)
let distance_to_user = 200

let shadow_angle = 135.
let shadow_distance = Float.of_int pixel_stud /. 4.

(* Some generic DOM functions *)

let window = Dom_html.window

module To_dom = Tyxml_cast.MakeTo (struct
  type 'a elt = 'a Tyxml_js.Svg.elt
  let elt = Tyxml_js.Svg.toelt
end)

(* We differenciate two different types for SVG elements:
  the first one, element, is a direct pointer to an element in the DOM.
  The second one is a Tyxml representation that can be safely copied,
  and converted to an actual element. *)
type element = Dom_svg.element Js.t
type element_repr = Svg_types.g_content Svg.elt

(* Converting a Tyxml object to an element.
  Theorically, it is possible to get a non-element node (text, comments, etc.),
  but we won't generate any isolated such nodes in this file. *)
let to_element (repr : element_repr) : element =
  let node = To_dom.of_node repr in
  Js.Opt.get (Dom_svg.CoerceTo.element node) (fun () -> assert false)

(* Setting the attributes of an element. *)
let setAttributes (elem : element) l =
  List.iter (fun (key, value) ->
    elem##setAttribute (Js.string key) value) l

let mapAttribute (elem : element) attr f =
  let v =
    Js.Opt.get
      (Js.Opt.map (elem##getAttribute (Js.string attr)) Js.to_string)
      (fun () -> "") in
  let v = f v in
  elem##setAttribute (Js.string attr) (Js.string v)

(* Convert a representation to an element and add it to the DOM at a specific place,
  then returning the built element. *)
let build_append_to elem (new_child : element_repr) =
  let new_child = to_element new_child in
  Dom.appendChild elem new_child ;
  new_child

(* Same but without returning the build element. *)
let append_to elem new_child =
  ignore (build_append_to elem new_child)

(* Removing all the content of an element. *)
let rec clear_element (n : element) =
  match Js.Opt.to_option n##.firstChild with
  | Some c ->
    ignore (n##removeChild c) ;
    clear_element n
  | None -> ()


module IMap = Map.Make (Structures.IntOrder)

(* Each levels is divided into four sublevels, each containing an SVG layer. *)
type sub_levels = {
  objects : element (* The main objects of this level. *) ;
  shadows : element (* Shadows drawn directly on these objects, or any substractive light. *) ;
  lights : element (* Additive lights added directly on these objects. *) ;
  passing_through : element (* Objects with volume that pass through the current level. *)
}

type sub_level_type =
  | Objects
  | Shadows
  | Lights
  | PassingThrough

type t = {
  svg : element (* The main svg element. *) ;
  interface : element (* An element on top of everything else, for the interface. *) ;
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


let get_size canvas = !(canvas.size)
let get_min_coords canvas = !(canvas.min_coord)

let iter f canvas =
  let (min_x, min_y) = !(canvas.min_coord) in
  let (s_x, s_y) = !(canvas.size) in
  for x = 0 to s_x do
    for y = 0 to s_y do
      f (min_x + x, min_y + y)
    done
  done


let init_svg () : element =
  let id = "map" in
  Js.Opt.get (Dom_svg.document##getElementById (Js.string id)) (fun () ->
      (* No map found: creating one. *)
      let%svg svg = "<svg width='100%' height='100%' id="id"></svg>" in
      build_append_to Dom_html.document##.body svg)

let window_x, window_y =
  (fun _ -> window##.innerWidth),
  (fun _ -> window##.innerHeight)

let create_sub_levels canvas level =
  let create ?(style="") name =
    let id = Printf.sprintf "%s-%d" name level in
    let g =
      if style = "" then
        let%svg g = "<g id="id"></g>" in g
      else
        let%svg g = "<g id="id" style="style"></g>" in g in
    let g = to_element g in
    Dom.insertBefore canvas.svg g (Js.Opt.return canvas.interface) ;
    g in
  (* Due to side-effects, the order is important there. *)
  let objects = create "level" in
  let shadows = create ~style:"opacity:0.2;" "shadows" in
  let lights = create "lights" in
  let passing_through = create "volumes" in
  {
    objects ;
    shadows ;
    lights ;
    passing_through
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
  let%svg interface = "<g id='interface'></g>" in
  let canvas = {
    svg = init_svg () ;
    interface = to_element interface ;
    levels = ref IMap.empty ;
    size = xy ;
    min_coord = min_coord
  } in
  (* Adding basic styles *)
  append_to canvas.svg
    (let%svg style =
       "<style>"
        "* { transform-origin: center; transform-box: fill-box; }"
        "text {"
          "font-family: \"Noto\", sans-serif; font-weight: bold;"
          "text-anchor: middle; dominant-baseline: central;"
        "}"
        "</style>" in
     style) ;
  (* Adding some filters *)
  append_to canvas.svg
    (let%svg defs =
       (* FIXME: Issue https://github.com/ocsigen/tyxml/issues/307 prevents us to write it all nicely. *)
      "<defs>"
        "<filter id='blur_light_moon' style='color-interpolation-filters:sRGB'
            x=-0.1 width=1.2 y=-0.1 height=1.2>"
          Svg.[feGaussianBlur ~a:[a_stdDeviation (0.2, None)] []]
        "</filter>"
        "<filter id='blur_shadow' style='color-interpolation-filters:sRGB'
            x=-0.3 width=1.6 y=-0.3 height=1.6>"
          Svg.[feGaussianBlur ~a:[a_stdDeviation (0.5, None)] []]
        "</filter>"
        "<filter id='blur_reflection_ray' style='color-interpolation-filters:sRGB'
            x=-0.8 width=2.6 y=-0.8 height=2.6>"
          Svg.[feGaussianBlur ~a:[a_stdDeviation (2., None)] []]
        "</filter>"
        Svg.[linearGradient ~a:[
            a_id "gradient_cardioid" ;
            (* FIXME: I'm adding this Obj.magic to circumvent this silly issue: https://github.com/ocsigen/tyxml/pull/314 *)
            (Obj.magic (a_gradientTransform [`Rotate ((shadow_angle, None), Some (0.5, 0.5))]))
          ] [%svg
            "<stop offset=0 stop-color='white' stop-opacity=1 />"
            "<stop offset=0.5 stop-color='white' stop-opacity=1 />"
            "<stop offset=1 stop-color='white' stop-opacity=0 />"
          ]]
      "</defs>" in
     defs) ;
  (* Creating a level 0 *)
  Dom.appendChild canvas.svg canvas.interface ;
  let sub_level = create_sub_levels canvas 0 in
  canvas.levels := IMap.add 0 sub_level !(canvas.levels) ;
  let on_change _ =
    update_xy () ;
    let (min_coord_x, min_coord_y) = !(canvas.min_coord) in
    let (size_x, size_y) = !(canvas.size) in
    let (shift_coord_x, shift_coord_y) =
      let shift w =
        let m = w mod pixel_stud in
        if m = 0 then 0
        else pixel_stud - (w mod pixel_stud) in
      (shift (window_x ()), shift (window_y ())) in
    let convert v = string_of_int (v * pixel_stud) in
    setAttributes canvas.svg [
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
    on_change canvas ;
    Js._true in
  window##.onresize := Dom_html.handler on_change ;
  ignore (on_change ()) ;
  canvas

let clear canvas =
  IMap.iter (fun _level sub_level ->
      clear_element sub_level.objects ;
      clear_element sub_level.shadows ;
      clear_element sub_level.lights ;
      clear_element sub_level.passing_through
    ) !(canvas.levels)


(* Fetch the group corresponding to the given level. *)
let get_level canvas ?(sub_level=Objects) level =
  assert (level >= 0) ;
  let rec aux level =
    assert (level >= 0) ;
    match IMap.find_opt level !(canvas.levels) with
    | Some sub_level -> sub_level
    | None ->
      (* Creating all previous levels and adding them before the current level. *)
      ignore (aux (level - 1)) ;
      (* Adding the current level. *)
      let sub_level =
        create_sub_levels canvas level in
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
val draw_rectangle : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?size:(int * int) -> ?proportion:float -> ?rotation:float -> ?darken:bool -> ?accumulator:(element_repr -> unit) -> ?url:string -> Dot.color -> unit

(* Same, but for a circle. *)
val draw_circle : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?diameter:int -> ?proportion:float -> ?rotation:float -> ?darken:bool -> ?accumulator:(element_repr -> unit) -> ?url:string -> Dot.color -> unit

(* Draw a quarter of a circle, with its angle at North-West. *)
val draw_quarter : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?proportion:float -> ?rotation:float -> ?darken:bool -> ?accumulator:(element_repr -> unit) -> ?url:string -> Dot.color -> unit

(* Draw a shape like the LEGO half-circle of a circle, with its straight border Noth. *)
val draw_half_circle : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?proportion:float -> ?rotation:float -> ?darken:bool -> ?accumulator:(element_repr -> unit) -> ?url:string -> Dot.color -> unit

(* Draw the halo due to the surface reflection of the angle at the side of a circular dot. *)
val draw_side_halo : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?diameter:int -> ?proportion:float -> ?accumulator:(element_repr -> unit) -> Dot.color -> unit

(* Draw the halo that can be seen underneath a transparent circle tile.
  This halo is probably caused by the reflection of the side halo above on the bottom of the
  transparent tile. *)
val draw_halo : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?diameter:int -> ?proportion:float -> ?accumulator:(element_repr -> unit) -> Dot.color -> unit

(* Draw the cardioid due to the inner reflection of light that can be seen underneath
  a transparent circle tile. *)
val draw_cardioid : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?diameter:int -> ?proportion:float -> ?accumulator:(element_repr -> unit) -> Dot.color -> unit

(* This function can be used to draw shadows.
  It takes as argument two pairs of angle and distance: these correspond to the left and right
  extreme points of the shape. The distance is expressed as a proportion of the stud length.
  It returns an accumulator that will accumulate any given shape to the shadow. *)
val draw_shadow : t -> (int * int) -> int -> ?height:int -> ?sub_level:sub_level_type -> ?color:Dot.color -> ?angle_left:float -> float -> ?angle_right:float -> float -> (element_repr -> unit)

(* Add an external image of the given size with the provided URL in the given position. *)
val add_external_image : t -> (int * int) -> int -> ?sub_level:sub_level_type -> ?size:(int * int) -> ?rotation:float -> ?link_url:string -> string -> unit

end = struct

let pixel_stud = Float.of_int pixel_stud

let print_float v = Js.string (Printf.sprintf "%g" v)

let transform_rotate = function
  | 0. -> []
  | rotation -> [`Rotate ((rotation, None), None)]

(* Prepare the default style. *)
let get_style ?(darken=false) ?(lighten=false) color =
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
      if darken then
        (* This is about a shadow drawn below a transparent element,
          and we don't want to overmake it. *)
        style ^ "fill-opacity:.1;"
      else style ^ "fill-opacity:.5;"
    else style in
  style

(* Prepare the drawing coordinates and colors. *)
let draw_figure canvas (x, y) level ?(sub_level=Objects) ?(size=(1, 1))
    ?(rotation=0.) ?(darken=false) ?(lighten=false)
    ?(accumulator=fun _ -> ()) ?(url="") color build_nodes =
  let (dx, dy) = size in
  let x', y' = x + dx, y + dy in
  let coord = get_perspective (x, y) level in
  let coord' = get_perspective (x', y') level in
  let style = get_style ~darken ~lighten color in
  let l = build_nodes coord coord' style in
  let level = get_level canvas ~sub_level level in
  let target =
    if url = "" then level
    else (
      let%svg a = "<a href="url"></a>" in
      build_append_to level a
    ) in
  List.iter (append_to target) l ;
  List.iter accumulator l ;
  (* Dealing with special cases. *)
  match color with
  | Letter c ->
    let (coordx, coordy) = coord in
    let (coordx', coordy') = coord' in
    append_to target
      (let%svg elem =
         "<text x="[((coordx +. coordx') /. 2., None)]
           " y="[((coordy +. coordy') /. 2., None)]
           " transform="(transform_rotate rotation)">"
           Svg.[txt c]
         "</text>" in
       elem)
  | Satin_trans_clear ->
    (* TODO: Try to reproduce this effect. *)
    ()
  | _ -> ()

let draw_rectangle canvas xy level ?(sub_level=Objects)
    ?(size=(1, 1)) ?(proportion=1.) ?(rotation=0.) ?(darken=false)
    ?(accumulator=fun _ -> ()) ?(url="") color =
  draw_figure canvas xy level ~sub_level ~size ~rotation ~darken ~accumulator ~url color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let delta = (1. -. proportion) *. pixel_stud /. 2. in
      let%svg rect =
        "<rect x="(coordx +. delta, None)
          " y="(coordy +. delta, None)
          " width="((coordx' -. coordx) *. proportion, None)
          " height="((coordy' -. coordy) *. proportion, None)
          " style="style
          " transform="(transform_rotate rotation)" />" in
      [rect]
    )

let draw_circle canvas xy level ?(sub_level=Objects)
    ?(diameter=1) ?(proportion=1.) ?(rotation=0.) ?(darken=false)
    ?(accumulator=fun _ -> ()) ?(url="") color =
  draw_figure canvas xy level ~sub_level ~size:(diameter, diameter)
    ~rotation ~darken ~accumulator ~url color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let radius = pixel_stud *. Float.of_int diameter /. 2. in
      let%svg circle =
        "<circle cx="((coordx +. coordx') /. 2., None)
          " cy="((coordy +. coordy') /. 2., None)
          " r="(radius *. proportion, None)
          " style="style
          " transform="(transform_rotate rotation)" />" in
      [circle]
    )

let draw_half_circle canvas xy level ?(sub_level=Objects)
    ?(proportion=1.) ?(rotation=0.) ?(darken=false)
    ?(accumulator=fun _ -> ()) ?(url="") color =
  draw_figure canvas xy level ~sub_level ~rotation ~darken ~accumulator ~url color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let delta = (1. -. proportion) *. pixel_stud /. 2. in
      let (coordx, coordy) = (coordx +. delta, coordy +. delta) in
      let (coordx', coordy') = (coordx' -. delta, coordy' -. delta) in
      let middley = (coordy +. coordy') /. 2. in
      let radius = pixel_stud *. proportion /. 2. in
      let%svg path =
        "<path d="
            (String.concat " " Printf.[
              sprintf "M %g,%g" coordx coordy ;
              sprintf "L %g,%g" coordx' coordy ;
              sprintf "L %g,%g" coordx' middley ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius coordx middley ;
              "z"
            ])
          " style="style
          " transform="(transform_rotate rotation)" />" in
      [path]
    )

let draw_quarter canvas (x, y) level ?(sub_level=Objects)
    ?(proportion=1.) ?(rotation=0.) ?(darken=false)
    ?(accumulator=fun _ -> ()) ?(url="") color =
  draw_figure canvas (x, y - 1) level ~sub_level ~size:(2, 2)
    ~rotation ~darken ~accumulator ~url color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let radius = pixel_stud *. proportion in
      let cx = (coordx +. coordx') /. 2. in
      let cy = (coordy +. coordy') /. 2. in
      let (coordx, coordy) = angular_from (cx, cy) 180. radius in
      let (coordx', coordy') = angular_from (cx, cy) 90. radius in
      let%svg path =
        "<path d="
            (String.concat " " Printf.[
              sprintf "M %g,%g" coordx' coordy ;
              sprintf "L %g,%g" coordx' coordy' ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius coordx coordy ;
              "z"
            ])
          " style="style
          " transform="(transform_rotate rotation)" />" in
      [path]
    )

let draw_side_halo canvas (x, y) level ?(sub_level=Lights)
    ?(diameter=1) ?(proportion=1.) ?(accumulator=fun _ -> ()) color =
  draw_figure canvas (x + 1 - diameter, y) level ~sub_level ~size:(diameter, diameter)
    ~lighten:true ~accumulator color
    (fun (coordx, coordy) (coordx', coordy') style ->
      let style = style ^ " fill-opacity:.8;" in
      let radius = pixel_stud *. proportion *. Float.of_int diameter /. 2. in
      let correction =
        (* Some correction due to the way larger shapes are drawn. *)
        Float.of_int (diameter - 1) *. (1. -. proportion) *. pixel_stud in
      let cx = (coordx +. coordx') /. 2. +. correction in
      let cy = (coordy +. coordy') /. 2. -. correction in
      let (ax, ay) = angular_from (cx, cy) (-75.) radius in
      let (bx, by) = angular_from (cx, cy) (-15.) radius in
      let%svg halo =
        "<path d="
           (let larger_radius = radius +. pixel_stud *. 0.5 in
            String.concat " " Printf.[
              sprintf "M %g,%g" ax ay ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius bx by ;
              sprintf "A %g %g 0 0 0 %g,%g" larger_radius larger_radius ax ay ;
              "z"
            ])
          " style="style" />" in
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
      let%svg halo =
        "<path d="
           (let larger_radius = radius *. 1.5 in
            String.concat " " Printf.[
              sprintf "M %g,%g" middlex coordy ;
              sprintf "A %g %g 0 0 1 %g,%g" radius radius coordx' middley ;
              sprintf "A %g %g 0 0 0 %g,%g" larger_radius larger_radius middlex coordy ;
              "z"
            ])
          " style="style" />" in
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
      let%svg cardioid =
        "<path d="path_d
          " style="style" />" in
      [cardioid]
    )

let draw_shadow canvas (x, y) level ?(height=1) ?(sub_level=Shadows) ?(color=Dot.Black)
      ?(angle_left=(-135.)) proportion_left ?(angle_right=45.) proportion_right =
  let style = get_style ~darken:true color in
  let distance_left = proportion_left *. pixel_stud /. 2. in
  let distance_right = proportion_right *. pixel_stud /. 2. in
  let (coordx, coordy) = get_perspective (x, y) level in
  let (coordx', coordy') = get_perspective (x + 1, y + 1) level in
  let cx = (coordx +. coordx') /. 2. in
  let cy = (coordy +. coordy') /. 2. in
  let (xl, yl) = angular_from (cx, cy) angle_left distance_left in
  let (xr, yr) = angular_from (cx, cy) angle_right distance_right in
  let shiftx = Float.of_int height *. shadow_distance *. cos (to_radian shadow_angle) in
  let shifty = Float.of_int height *. shadow_distance *. sin (to_radian shadow_angle) in
  let points = [
    (xl, yl) ;
    (xr, yr) ;
    (xr +. shiftx, yr +. shifty) ;
    (xl +. shiftx, yl +. shifty)
  ] in
  let%svg g =
    "<g style='filter:url(#blur_shadow);'>"
      "<polyline style="style" points="points" />"
    "</g>" in
  let g = to_element g in
  let level = get_level canvas ~sub_level level in
  Dom.appendChild level g ;
  fun (elem : element_repr) -> (
    let elem1 = to_element elem in
    let elem2 = to_element elem in
    let clear_style (elem : element) =
      elem##setAttribute (Js.string "style") (Js.string style) in
    clear_style elem1 ;
    clear_style elem2 ;
    mapAttribute elem2 "transform" (fun transform ->
      Printf.sprintf "translate(%g, %g) %s" shiftx shifty transform) ;
    Dom.appendChild g elem1 ;
    Dom.appendChild g elem2
  )

let add_external_image canvas xy level ?(sub_level=Objects) ?(size=(1, 1)) ?(rotation=0.)
    ?(link_url="") url =
  draw_figure canvas xy level ~sub_level ~size ~rotation ~url:link_url Dot.White
    (fun (coordx, coordy) (coordx', coordy') _style ->
      let%svg image =
        "<image x="(coordx, None)
          " y="(coordy, None)
          " width="(coordx' -. coordx, None)
          " height="(coordy' -. coordy, None)
          " href="url
          " transform="(transform_rotate rotation)" />" in
      [image]
    )

end

(* Drawing LEGO pieces. *)
module Lego = struct

(* Note that the levels in this module differ from the other representation of the level
  in the file: beforehand, it was splited in 2 to accomodate for smaller measurments like
  stud height.  In this module it is meant to be the actual number of plates needed, so
  there is a 2-factor in place there. *)

(* The following two constant converts the levels from the current system to the old one. *)
let default_level = 1
let convert_level level =
  assert (level >= 1) ;
  2 * level


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

(* Ratio between the diagonal of a square compare to a base unit. *)
let square_proportion proportion = proportion *. sqrt 2.

(* Angular position of the angle of a square and circles. *)
let circle_angle_left = -135.
let circle_angle_right = 45.
let square_angle_left rotation = rotation +. circle_angle_left
let square_angle_right rotation = rotation +. circle_angle_right

(* TODO: Using the <use> SVG element to take up less memory. *)

let draw_stud canvas xy color =
  let proportion = 0.57 in
  let shadow = draw_shadow canvas xy 0 proportion proportion in
  draw_circle canvas xy 0 ~sub_level:PassingThrough ~proportion ~darken:true
    ~accumulator:shadow (remove_letters color) ;
  draw_circle canvas xy 1 ~proportion color ;
  draw_side_halo canvas xy 1 ~sub_level:Lights ~proportion Dot.White

let base_plate canvas xy ?(size=(1, 1)) color =
  draw_rectangle canvas xy 0 ~size (remove_letters color) ;
  draw_stud canvas xy color

let square_tile canvas xy ?(level=default_level) ?(size=(1, 1))
    ?(sticker="") ?(url="") color =
  let level = convert_level level in
  let rotation = get_rotation_size xy size in
  let proportion = 0.95 in
  let shadow =
    let sq_proportion = square_proportion proportion in
    let angle_l = square_angle_left rotation in
    let angle_r = square_angle_right rotation in
    draw_shadow canvas xy 0 ~angle_left:angle_l sq_proportion ~angle_right:angle_r sq_proportion in
  draw_rectangle canvas xy (level - 2) ~sub_level:PassingThrough ~size ~proportion ~rotation
    ~darken:true ~accumulator:shadow (remove_letters color) ;
  draw_rectangle canvas xy level ~size ~proportion ~rotation ~url color ;
  if sticker <> "" then
    add_external_image canvas xy level ~size ~rotation ~link_url:url sticker

let plate canvas xy ?(level=default_level) ?(size=(1, 1)) color =
  square_tile canvas xy ~level ~size color ;
  let (x0, y0) = xy in
  let (xm, ym) =
    let (sx, sy) = size in
    (x0 + sx - 1, y0 + sy - 1) in
  for x = x0 to xm do
    for y = y0 to ym do
      draw_stud canvas (x, y) color
    done
  done

let round_tile canvas xy ?(level=default_level) ?(diameter=1)
    ?(sticker="") ?(url="") color =
  let level = convert_level level in
  let size = (diameter, diameter) in
  let rotation = get_rotation_size ~double_rotation:true xy size in
  let proportion = 0.95 in
  let shadow = draw_shadow canvas xy 0 proportion proportion in
  if Color.is_transparent color then (
    (* Adding special shades underneath the circle. *)
    let proportion = 0.57 in
    (* Note that we dropped the rotation: the light is always coming from the same direction. *)
    draw_halo canvas xy (level - 1) ~sub_level:Lights ~diameter ~proportion color ;
    draw_cardioid canvas xy (level - 2) ~sub_level:Lights ~diameter ~proportion color
  ) ;
  draw_circle canvas xy (level - 2) ~sub_level:PassingThrough ~diameter ~proportion ~rotation
    ~darken:true ~accumulator:shadow (remove_letters color) ;
  draw_circle canvas xy level ~diameter ~proportion ~rotation ~url color ;
  draw_side_halo canvas xy level ~sub_level:Lights ~diameter ~proportion Dot.White ;
  if sticker <> "" then
    add_external_image canvas xy level ~size ~rotation ~link_url:url sticker

let convert_direction = function
  | Dot.North -> 0.
  | Dot.West -> -90.
  | Dot.South -> 180.
  | Dot.East -> 90.

let quarter_tile canvas xy ?(level=default_level) direction
    ?(sticker="") ?(url="") color =
  let level = convert_level level in
  let base_rotation =
    let size = (1, 1) in
    get_rotation_size xy size in
  let rotation = base_rotation +. convert_direction direction in
  let proportion = 0.95 in
  let shadow =
    let sq_proportion = square_proportion proportion in
    let (angle_l, proportion_l) =
      match direction with
      | Dot.East -> (circle_angle_left, proportion *. 0.6)
      | _ -> (square_angle_left base_rotation, sq_proportion) in
    let (angle_r, proportion_r) =
      match direction with
      | Dot.West -> (circle_angle_right, proportion *. 0.6)
      | _ -> (square_angle_right base_rotation, sq_proportion) in
    draw_shadow canvas xy 0 ~angle_left:angle_l proportion_l ~angle_right:angle_r proportion_r in
  draw_quarter canvas xy (level - 2) ~sub_level:PassingThrough ~proportion ~rotation
    ~darken:true ~accumulator:shadow (remove_letters color) ;
  draw_quarter canvas xy level ~proportion ~rotation ~url color ;
  if direction = Dot.South then (
    draw_side_halo canvas xy level ~sub_level:Lights ~diameter:2 ~proportion Dot.White
  ) ;
  if sticker <> "" then
    add_external_image canvas xy level ~rotation ~link_url:url sticker

let half_circle_tile canvas xy ?(level=default_level) direction
    ?(sticker="") ?(url="") color =
  let level = convert_level level in
  let base_rotation =
    let size = (1, 1) in
    get_rotation_size xy size in
  let rotation = base_rotation +. convert_direction direction in
  let proportion = 0.95 in
  let shadow =
    let sq_proportion = square_proportion proportion in
    let (angle_l, proportion_l) =
      match direction with
      | Dot.North | Dot.West -> (square_angle_left base_rotation, sq_proportion)
      | Dot.South | Dot.East -> (circle_angle_left, proportion) in
    let (angle_r, proportion_r) =
      match direction with
      | Dot.North | Dot.West -> (circle_angle_right, proportion)
      | Dot.South | Dot.East -> (square_angle_right base_rotation, sq_proportion) in
    draw_shadow canvas xy 0 ~angle_left:angle_l proportion_l ~angle_right:angle_r proportion_r in
  draw_half_circle canvas xy (level - 2) ~sub_level:PassingThrough ~proportion ~rotation
    ~darken:true ~accumulator:shadow (remove_letters color) ;
  draw_half_circle canvas xy level ~proportion ~rotation ~url color ;
  if direction = Dot.South || direction = Dot.West then
    draw_side_halo canvas xy level ~sub_level:Lights ~proportion Dot.White ;
  if sticker <> "" then
    add_external_image canvas xy level ~rotation ~link_url:url sticker

end


