
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
  let styles = Settings.styles in
  Overpass.set_lookup (Overpass.settings_to_attributes styles) ;
  ignore Overpass.get_objects (* TODO *) ;
  ()

(* Draw the map on a Geometry.t object. *)
let draw_map map =
  let open Geometry in
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

(* Draw the menu on a Menu.t object. *)
let draw_menu menu =
  let open Menu in
  (* Add a text in the right low angle. *)
  let (size_x, _size_y) = Menu.get_size menu in
  let txt = "TEST" in
  let menu = Menu.add_text menu (size_x - String.length txt, 0) txt in
  ignore menu


(* Get optional URL of an action. Default to the empty string. *)
let action_url = function
  | Menu.Link url -> url
  | _ -> ""

(* Print the dot onto the canvas. *)
let print_dot canvas coord ?(level=1) ?(action=Menu.NoAction) (shape, color) =
  let open Canvas.Lego in
  match shape with
  | Dot.Round -> round_tile canvas coord ~level ~url:(action_url action) color
  | Dot.Square -> square_tile canvas coord ~level  ~url:(action_url action) color
  | Dot.Half_circle dir -> half_circle_tile canvas coord ~level dir ~url:(action_url action) color
  | Dot.Quarter dir -> quarter_tile canvas coord ~level dir ~url:(action_url action) color

(* Print onto the canvas a piece, associated with a Menu action. *)
let print_piece_with_action canvas coord ?(level=1) (piece, action) =
  let open Canvas.Lego in
  match piece with
  | Menu.Dot d -> print_dot canvas coord ~level ~action d
  | Menu.Plate p -> plate canvas coord ~level ~size:p.dim p.color
  | Menu.Tile p ->
    square_tile canvas coord ~level ~size:p.dim
      ~sticker:p.sticker ~url:(action_url action) p.color
  | Menu.Round p ->
    round_tile canvas coord ~level ~diameter:(fst p.dim)
      ~sticker:p.sticker ~url:(action_url action) p.color
  | Menu.Occupied -> ()

let draw canvas =
  Canvas.clear canvas ;
  (* Initialising a new map to draw on.  *)
  let size = Canvas.get_size canvas in
  let map = Geometry.empty size in
  let menu = Menu.empty size in
  (* Display the actual map within it. *)
  draw_map map ;
  draw_menu menu ;
  (* Print this map onto the canvas. *)
  let map = Geometry.to_dot_matrix map in
  let menu = Menu.to_matrix menu in
  let background = Dot.Light_aqua in (* TODO: Make it depend on the local climate. *)
  let min_coords = Canvas.get_min_coords canvas in
  Array.iteri (fun x ->
    Array.iteri (fun y dot ->
        let (l, o) = menu.(x).(y) in
        (* We first convert the coordinate systems. *)
        (* SVG makes the y axes run from top to bottom: we put it back from bottom to top. *)
        let y = snd size - 1 - y in
        let coord = (x + fst min_coords, y + snd min_coords) in
        (* Then draw the object. *)
        let open Canvas.Lego in
        base_plate canvas coord background ;
        if l = [] && o = None then
          Option.iter (print_dot canvas coord) dot
        else (
          List.iteri (fun i c ->
              plate canvas coord ~level:i c
            ) l ;
          Option.iter (print_piece_with_action canvas coord ~level:(1 + List.length l)) o
        )
      )) map

let _ =
  let _canvas = Canvas.init draw in
  ()

