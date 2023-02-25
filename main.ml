
let display_map query_function map_object map_info =
  let query = query map_info in
  (* Fetching and parsing the result of the query. *)
  let%lwt data = (* TODO *) in
  (* Displaying the data. *)
  (* TODO *)

let _ =
  (* The default information about the map. *)
  let map_info = ref (* TODO: bbox, zoom level, etc. *) in
  (* Preparing the interface. *)
  (* TODO *)
  (* The DOM object in which the map should be displayed. *)
  let map_object = (* TODO *) in
  (* Getting the kind of information we are interested in. *)
  let map_settings = (* TODO *) in
  (* Translating it into an Overpass query, as a function of the map_info. *)
  let overpass_query = (* TODO *) map_settings in
  display_map overpass_query map_object !map_info

