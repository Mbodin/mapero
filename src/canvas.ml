
open Js_of_ocaml
open Js_of_ocaml_lwt


(* Size in pixel of a single LEGO cell. *)
let pixel_stud = 10

(* Height in pixel of a stud. *)
let pixel_stud_height = pixel_stud / 5


(* Some generic DOM functions *)

let window = Dom_html.window
let document = window##.document

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
  size : (int * int) ref (* The current size of the whole canvas. *)
}


let for_all f canvas =
  I3Map.iter (fun ((x, y), level) _ -> f (x, y) level) !(canvas.groups)


let init_svg () =
  let id = Js.string "map" in
  Js.Opt.get (Dom_html.document##getElementById id) (fun () ->
      (* No map found: creating one. *)
      let svg = document##createElement (Js.string "svg") in
      set_attributes svg [
          ("version", Js.string "1.1") ;
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
  let update_xy () =
    let round_to_stud v =
      v / pixel_stud + if v mod pixel_stud = 0 then 0 else 1 in
    xy := (round_to_stud (window_x ()), round_to_stud (window_y ())) in
  let r = {
    svg = init_svg () ;
    groups = ref I3Map.empty ;
    levels = ref IMap.empty ;
    size = xy
  } in
  (* Adding basic styles *)
  let style = document##createElement (Js.string "style") in
  let styles =
    let styles = "text { font-family: \"Noto\", sans-serif; font-weight: bold; }" in
    document##createTextNode (Js.string styles) in
  Dom.appendChild style styles ;
  Dom.appendChild r.svg style ;
  (* Creating a level 0 *)
  let g = document##createElement (Js.string "g") in
  set_attributes g [("id", Js.string "level-0")] ;
  Dom.appendChild r.svg g ;
  r.levels := IMap.add 0 g !(r.levels) ;
  let on_change _ =
    update_xy () ;
    let to_string i = Js.string (string_of_int i) in
    set_attributes r.svg [
        ("width", to_string (window_x ())) ;
        ("height", to_string (window_y ()))
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
      let g = document##createElement (Js.string "g") in
      let compute_d v =
        let v = Float.of_int v in
        (* sin (arctan v) = 1 / sqrt (1 + v^2)
          It represents the shift due to perspective. *)
        let sin_arctan v = 1. /. sqrt (1. +. v *. v) in
        v *. Float.of_int pixel_stud +. sin_arctan v *. Float.of_int pixel_stud_height in
      let dx = compute_d x in
      let dy = compute_d y in
      set_attributes g [
          ("transform", Js.string (Printf.sprintf "translate(%f, %f)" dx dy))
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
            let g = document##createElement (Js.string "g") in
            set_attributes g [("id", Js.string (Printf.sprintf "level-%d" level))] ;
            Dom.insertBefore canvas.svg g Js.Opt.empty ;
            canvas.levels := IMap.add level g !(canvas.levels) ;
            g in
        aux level in
      Dom.appendChild level g ;
      g in
    canvas.groups := I3Map.add key g !(canvas.groups) ;
    g


(* TODO
      let rect = document##createElement (Js.string "rect") in
      set_attributes rect [
          ("x", convert 0) ;
          ("y", convert 0) ;
          ("with", convert pixel_stud) ;
          ("height", convert pixel_stud) ;
          ("style", Js.string "fill:rgb(0,0,0)") (* TODO: which color? *)
        ];
*)


