
let _ = Random.self_init ()

let display_map query_function map_object map_info =
  let query = query_function map_info in
  (* Fetching and parsing the result of the query. *)
  let%lwt data = (* TODO *)Lwt.return () in
  (* Displaying the data. *)
  (* TODO *)
  Lwt.return ()

let draw canvas =
  let open Canvas.Lego in
  Canvas.clear canvas ;
  Canvas.iter (fun xy ->
      base_plate canvas xy Dot.White
    ) canvas ;
  (* Some tests. *)
  List.iteri (fun i c ->
      square_tile canvas (i - 10, 1) c ;
      round_tile canvas (i - 10, 2) c
    ) Dot.[Black; White; Bright_green; Bright_light_blue; Coral; Dark_azure; Dark_turquoise;
           Lavender; Trans_orange; Satin_trans_clear; Yellow;
           Letter "T"; Letter "E"; Letter "S"; Letter "T"] ;
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

