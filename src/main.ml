
let display_map query_function map_object map_info =
  let query = query_function map_info in
  (* Fetching and parsing the result of the query. *)
  let%lwt data = (* TODO *)Lwt.return () in
  (* Displaying the data. *)
  (* TODO *)
  Lwt.return ()

let draw canvas =
  let open Canvas in
  clear canvas ;
  iter (fun xy ->
      draw_rectangle canvas xy 0 Dot.Lavender ;
      draw_circle canvas xy 0 ~proportion:0.5 Dot.Black ;
      draw_circle canvas xy 1 ~proportion:0.5 Dot.Lavender ;
      ()
    ) canvas ;
  () (* TODO *)

let _ =
  (* The default information about the map. *)
  let map_info = ref (* TODO: bbox, zoom level, etc. *)() in
  (* Preparing the interface. *)
  let canvas = Canvas.init draw in
  (* The DOM object in which the map should be displayed. *)
  let map_object = (* TODO *)() in
  (* Building an Overpass query, as a function of the map_info. *)
  let overpass_query = (* TODO *) let _ = Settings.objects in (fun _ -> "TODO") in
  display_map overpass_query map_object !map_info

