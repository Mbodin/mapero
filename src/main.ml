
let display_map query_function map_object map_info =
  let query = query_function map_info in
  (* Fetching and parsing the result of the query. *)
  let%lwt data = (* TODO *)Lwt.return () in
  (* Displaying the data. *)
  (* TODO *)
  Lwt.return ()

let _ =
  (* The default information about the map. *)
  let map_info = ref (* TODO: bbox, zoom level, etc. *)() in
  (* Preparing the interface. *)
  (* TODO *)
  (* The DOM object in which the map should be displayed. *)
  let map_object = (* TODO *)() in
  (* Building an Overpass query, as a function of the map_info. *)
  let overpass_query = (* TODO *) let _ = Settings.objects in (fun _ -> "TODO") in
  display_map overpass_query map_object !map_info

