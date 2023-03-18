
let _ = Random.self_init ()

(* The current state of the map. *)
type map_state = {
  zoom_level : int ;
  map_center : Geometry.real_coordinates ;
}

let default_map_state = {
  zoom_level = 18 ;
  map_center = (45.18573, 5.73954) (* TODO: What should be the default location? *)
}

(* The current settings of the map. *)
let map_settings = ref default_map_state

(* TODO: It would be nice to retrieve the value it was in the last visit of the page, if any. *)


(* Building an Overpass query, as a function of the current settings. *)
let overpass_query map_info =
  ignore Settings.objects (* TODO *) ;
  ()

(* Add a text in the right low angle. *)
let add_text_angle map txt =
  let (size_x, _size_y) = Geometry.get_size map in
  Geometry.add_text map (size_x - String.length txt, 0) ~priority:VeryHigh txt

(* Draw the map on a Geometry.t object. *)
let draw_map map =
  let open Geometry in
  let map = add_text_angle map "TEST" in
  let map =
    add_PoI map (1., 5.) ~level:1
      ~style:(Geometry.Pattern (fun _ -> Dot.Half_circle Dot.North)) Dot.Yellowish_green in
  let map = add_PoI map (1., 1.) ~level:1 Dot.Yellowish_green in
  let map = add_line map (3., 3.) (3., 7.) ~level:2 Dot.Lavender in
  let map = add_line map (5., 2.) (13., 2.) ~level:3 Dot.Medium_azure in
  let map = add_line map (7., 9.) (14., 4.) ~level:4 Dot.Bright_green in
  let map = add_line map (10., 10.) (16., 16.) ~level:5 Dot.Coral in
  let map = add_line map (16., 10.) (10., 16.) ~level:6 Dot.Bright_light_yellow in
  let map = add_polygon map [(18., 2.) ; (25., 4.) ; (22., 5.)] ~border_level:6 Dot.Dark_turquoise () in
  let map = add_PoI map (18., 2.) ~level:1 Dot.Yellowish_green in
  let map = add_PoI map (25., 4.) ~level:1 Dot.Yellowish_green in
  let map = add_PoI map (22., 5.) ~level:1 Dot.Yellowish_green in
  let map = add_line map (22., 8.) (25., 7.) ~level:7 Dot.Coral in
  let map = add_line map (16., 8.) (25., 10.) ~level:5 Dot.Bright_light_orange in
  let map = add_line map (1., 12.) (6., 14.) ~level:5 ~style:Round Dot.Bright_light_blue in
  (* TODO *)
  ignore map

(* Print the dot onto the canvas. *)
let print_dot canvas coord (shape, color) =
  let open Canvas.Lego in
  match shape with
  | Dot.Round -> round_tile canvas coord color
  | Dot.Square -> square_tile canvas coord color
  | Dot.Half_circle dir -> half_circle_tile canvas coord dir color
  | Dot.Quarter dir -> quarter_tile canvas coord dir color

let draw canvas =
  Canvas.clear canvas ;
  (* Initialising a new map to draw on.  *)
  let size = Canvas.get_size canvas in
  let map = Geometry.empty size in
  (* Display the actual map within it. *)
  draw_map map ;
  (* Print this map onto the canvas. *)
  let map = Geometry.to_dot_matrix map in
  let background = Dot.Light_aqua in (* TODO: Make it depend on the local climate. *)
  let min_coords = Canvas.get_min_coords canvas in
  Array.iteri (fun x ->
    Array.iteri (fun y dot ->
        (* We first convert the coordinate systems. *)
        (* SVG makes the y axes run from top to bottom: we put it back from bottom to top. *)
        let y = snd size - 1 - y in
        let (x, y) = (x + fst min_coords, y + snd min_coords) in
        (* Then draw the object. *)
        let open Canvas.Lego in
        base_plate canvas (x, y) background ;
        Option.iter (print_dot canvas (x, y)) dot
      )) map

let _ =
  let _canvas = Canvas.init draw in
  ()

