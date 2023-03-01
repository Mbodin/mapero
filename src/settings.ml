
let exact l = List.map (fun (k, v) -> (Overpass.Exact k, Overpass.Exact v)) l
let regexp l = List.map (fun (k, v) -> (Overpass.Regexp k, Overpass.Regexp v)) l
let vregexp l = List.map (fun (k, v) -> (Overpass.Exact k, Overpass.Regexp v)) l
let expand l =
  List.concat_map (fun (k, vs) -> List.map (fun v -> (Overpass.Exact k, Overpass.Exact v)) vs) l

let node l ?(style = Map.Round) color =
  (Overpass.Node, exact l, style, color)

let rnode l ?(style = Map.Round) color =
  (Overpass.Node, regexp l, style, color)

let rvnode l ?(style = Map.Round) color =
  (Overpass.Node, vregexp l, style, color)

let enode l ?(style = Map.Round) color =
  (Overpass.Node, expand l, style, color)

let way l ?(style = Map.Adaptive) color =
  (Overpass.Way, exact l, style, color)

let rway l ?(style = Map.Adaptive) color =
  (Overpass.Way, regexp l, style, color)

let vrway l ?(style = Map.Adaptive) color =
  (Overpass.Way, vregexp l, style, color)

let eway l ?(style = Map.Adaptive) color =
  (Overpass.Way, expand l, style, color)



let objects = [
  (* PoIs *)
  node [("natural", "tree")] Dot.Bright_green ;
  node [("leisure", "playground")] Dot.Satin_trans_clear ;
  way [("leisure", "playground")] Dot.Satin_trans_clear ;
  node [("amenity", "public_bookcase")] Dot.Trans_orange ;
  node [("emergency", "fire_hydrant"); ("fire_hydrant:type", "pillar")] Dot.Coral ;
  node [("amenity", "bicycle_parking"); ("bicycle_parking", "wide_stands")] Dot.Dark_turquoise ;
  node [("amenity", "bicycle_parking"); ("bicycle_parking", "stands")] Dot.Dark_turquoise ;
  node [("amenity", "bicycle_parking"); ("bicycle_parking", "bollard")] Dot.Dark_turquoise ;
  (* Ways *)
  (* TODO: expand doesn't actually work like that: this will be looking for the intersection of all the values (which is empty) instead of the sum. *)
  eway [("highway", ["motorway"; "trunk"; "primary"; "secondary"; "tertiary"; "unclassified"; "road"])] Dot.Dark_azure ;
  eway [("highway", ["motorway_link"; "trunk_link"; "primary_link"; "secondary_link"; "tertiary_link"])] Dot.Dark_azure ;
  eway [("highway", ["residential"; "living_street"; "service"; "track"])] Dot.Light_aqua ;
  eway [("highway", ["pedestrian"; "footway"; "bridleway"; "path"; "cycleway"; "crossing"])] Dot.Bright_light_blue ;
]

