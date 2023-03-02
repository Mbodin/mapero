
open Js_of_ocaml
open Js_of_ocaml_lwt


(* Size in pixel of a single Lego cell. *)
let pixel_stud = 10


(* Some generic DOM functions *)

let document = Dom_html.window##.document

let set_attributes elem l =
  List.iter (fun (key, value) ->
    ignore (elem##setAttribute (Js.string key) value)) l

let rec clear_node n =
  match Js.Opt.to_option n##.firstChild with
  | Some c ->
    ignore (n##removeChild c) ;
    clear_node n
  | None -> ()


type g = Dom.node


module Open = struct
open Structures
module I3Map =
  Map.Make (ProductOrderedType (ProductOrderedType (IntOrder) (IntOrder)) (IntOrder))
end
open Open

type t = {
  svg : Dom.node ;
  groups : g I3Map.t
}


let init_svg () =
  let id = Js.string "map" in
  let svg =
    Js.Opt.get (Dom_html.document##getElementById id) (fun () ->
        (* No map found: creating one. *)
        let svg = document##createElement (Js.string "svg") in
        set_attributes svg [
            ("version", Js.number_of_float 1.1) ;
            ("width", Js.number_of_float 100.) ;
            ("height", Js.number_of_float 100.) ;
            ("xmlns", Js.string "http://www.w3.org/2000/svg") ;
            ("id", id)
          ] ;
        document##.body##appendChild svg ;
        svg
      ) in
  (* Setting the global attributes of the svg. *)
  let convert i = Js.number_of_float (Float.of_int i) in
  let width = convert (x * pixel_stud) in
  let height = convert (y * pixel_stud) in
  set_attributes svg [
      ("width", width) ;
      ("height", height)
    ] ;
  svg

let init () = {
    svg = init_svg () ;
    groups = I3Map.empty
  }


(* TODO: only generate groups g on demand. *)
(* TODO: do not draw in such a function: it should only generate the groups. *)
(* TODO: add some general groups so that we can easily drag-and-drop the whole map. *)
let init_svg (x, y) =
  (* Clearing the svg. *)
  clear_node svg ;
  (* Filling the svg with groups for each studs, as well as a base plate. *)
  for ix = 0 to x do
    for iy = 0 to y do
      let dx = ix * pixel_stud in
      let dy = iy * pixel_stud in
      let g = document##createElement (Js.string "g") in
      set_attributes g [
          ("transform", Js.string (Printf.sprintf "translate(%d, %d)" dx dy))
        ];
      svg##appendChild g ;
      let rect = document##createElement (Js.string "rect") in
      set_attributes rect [
          ("x", convert 0) ;
          ("y", convert 0) ;
          ("with", convert pixel_stud) ;
          ("height", convert pixel_stud) ;
          ("style", Js.string "fill:rgb(0,0,0)") (* TODO: which color? *)
        ];
      g##appendChild rect ;
      (* TODO: A g for the top of the stud. *)
      (* TODO: store these groups somewhere to ease later access. *)
    done
  done



