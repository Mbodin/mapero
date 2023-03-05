
open Js_of_ocaml
open Js_of_ocaml_lwt


(* Size in pixel of a single LEGO cell. *)
let pixel_stud = 60

(* Height in pixel of a stud. *)
let pixel_stud_height = pixel_stud / 5


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


type g = Dom_html.element Js.t


module Open = struct
open Structures
module IMap = Map.Make (IntOrder)
module I3Map =
  Map.Make (ProductOrderedType (ProductOrderedType (IntOrder) (IntOrder)) (IntOrder))
end
open Open

type t = {
  svg : Dom_html.element Js.t ; (* The main svg element. *)
  groups : g I3Map.t ref ; (* All the groups, indexed by ((x, y), level). *)
  levels : g IMap.t ref ; (* A group for each level. *)
  size : (int * int) ref (* The current size of the whole canvas. *) ;
  min_coord : (int * int) ref (* The coordinate of the minimum currently displayed cell. *)
}
(* Important note: the cell (0, 0) is the center for the perspective, not the minimum cell.
  The minimum cell has negative coordinates. *)


let for_all_used f canvas =
  I3Map.iter (fun ((x, y), level) _ -> f (x, y) level) !(canvas.groups)

let for_all_visible f canvas =
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
      -v/2 - if v mod 2 = 0 then 0 else 1 in
    min_coord := (compute_min x, compute_min y) in
  let r = {
    svg = init_svg () ;
    groups = ref I3Map.empty ;
    levels = ref IMap.empty ;
    size = xy ;
    min_coord = min_coord
  } in
  (* Creating a level 0 *)
  let g = createSVGElement "g" in
  set_attributes g [("id", Js.string "level-0")] ;
  Dom.appendChild r.svg g ;
  r.levels := IMap.add 0 g !(r.levels) ;
  let on_change _ =
    update_xy () ;
    let (min_coord_x, min_coord_y) = !(r.min_coord) in
    let (size_x, size_y) = !(r.size) in
    let convert_min v = v * pixel_stud - pixel_stud / 2 in
    let convert_size v = v * pixel_stud in
    set_attributes r.svg [
        ("viewBox", Js.string
           (String.concat " "
             (List.map string_of_int
                [convert_min min_coord_x; convert_min min_coord_y;
                 convert_size size_x; convert_size size_y])))
      ] ;
    on_change r ;
    Js._true in
  window##.onresize := Dom_html.handler on_change ;
  ignore (on_change ()) ;
  r

let clear canvas =
  I3Map.iter (fun ((_x, _y), _level) node -> clear_node node) !(canvas.groups)


let get canvas (x, y) level =
  let key = ((x, y), level) in
  match I3Map.find_opt key !(canvas.groups) with
  | Some g -> g
  | None ->
    let g =
      let g = createSVGElement "g" in
      let compute_d v =
        let v = Float.of_int v in
        (* sin (arctan x) = x / sqrt (1 + x^2)
          It represents the shift due to perspective. *)
        let sin_arctan v = v /. sqrt (1. +. v *. v) in
        v *. Float.of_int pixel_stud +. sin_arctan v *. Float.of_int pixel_stud_height in
      let dx = compute_d x in
      let dy = compute_d y in
      set_attributes g [
          ("transform", Js.string (Printf.sprintf "translate(%g, %g)" dx dy))
        ];
      (* We can't add it directly to the svg element or it might superimpose to a level above.
        To avoid that, we have to add it in the right level. *)
      let level =
        let rec aux level =
          assert (level >= 0) ;
          match IMap.find_opt level !(canvas.levels) with
          | Some g -> g
          | None ->
            ignore (aux (level - 1)) ;
            let g = createSVGElement "g" in
            set_attributes g [("id", Js.string (Printf.sprintf "level-%d" level))] ;
            Dom.insertBefore canvas.svg g Js.Opt.empty ;
            canvas.levels := IMap.add level g !(canvas.levels) ;
            g in
        aux level in
      Dom.appendChild level g ;
      g in
    canvas.groups := I3Map.add key g !(canvas.groups) ;
    g


(* TODO: Deal with rotation, letters, transparency. *)

let draw_rectangle g ?(proportion=1.) ?(rotation=0.) color =
  let rect = createSVGElement "rect" in
  let convert v = Js.string (Printf.sprintf "%g" (v *. proportion)) in
  let pixel_stud = Float.of_int pixel_stud in
  let style =
    let (r, g, b) = Color.to_rgb color in
    Printf.sprintf "fill:rgb(%d,%d,%d)" r g b in
  set_attributes rect [
      ("x", convert (-. pixel_stud /. 2.)) ;
      ("y", convert (-. pixel_stud /. 2.)) ;
      ("width", convert pixel_stud) ;
      ("height", convert pixel_stud) ;
      ("style", Js.string style)
    ] ;
  Dom.appendChild g rect

let draw_circle g proportion ?(rotation=0.) color =
  let circ = createSVGElement "circle" in
  let convert v = Js.string (Printf.sprintf "%g" (v *. proportion)) in
  let pixel_stud = Float.of_int pixel_stud in
  let style =
    let (r, g, b) = Color.to_rgb color in
    Printf.sprintf "fill:rgb(%d,%d,%d)" r g b in
  set_attributes circ [
      ("cx", convert 0.) ;
      ("cy", convert 0.) ;
      ("r", convert pixel_stud) ;
      ("style", Js.string style)
    ] ;
  Dom.appendChild g circ


