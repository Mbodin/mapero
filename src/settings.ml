
let exact l = List.map (fun (k, v) -> (Overpass.Exact k, Overpass.Exact v)) l
let regexp l = List.map (fun (k, v) -> (Overpass.Regexp k, Overpass.Regexp v)) l
let vregexp l = List.map (fun (k, v) -> (Overpass.Exact k, Overpass.Regexp v)) l
let rec expand cxt = function
  | [] -> cxt []
  | (k, vs) :: l ->
    List.concat_map (fun v -> expand (fun l -> cxt ((Overpass.Exact k, Overpass.Exact v) :: l)) l) vs

let node l ?(style = Geometry.Round) color =
  [(Overpass.Node, exact l, style, color)]

let rnode l ?(style = Geometry.Round) color =
  [(Overpass.Node, regexp l, style, color)]

let rvnode l ?(style = Geometry.Round) color =
  [(Overpass.Node, vregexp l, style, color)]

let enode l ?(style = Geometry.Round) color =
  expand (fun l -> [(Overpass.Node, l, style, color)]) l

let way l ?(style = Geometry.Adaptive) color =
  [(Overpass.Way, exact l, style, color)]

let rway l ?(style = Geometry.Adaptive) color =
  [(Overpass.Way, regexp l, style, color)]

let vrway l ?(style = Geometry.Adaptive) color =
  [(Overpass.Way, vregexp l, style, color)]

let eway l ?(style = Geometry.Adaptive) color =
  expand (fun l -> [(Overpass.Way, l, style, color)]) l

let poly l ?(style = Geometry.Adaptive) color =
  [(Overpass.Polygon, exact l, style, color)]

let rpoly l ?(style = Geometry.Adaptive) color =
  [(Overpass.Polygon, regexp l, style, color)]

let vrpoly l ?(style = Geometry.Adaptive) color =
  [(Overpass.Polygon, vregexp l, style, color)]

let epoly l ?(style = Geometry.Adaptive) color =
  expand (fun l -> [(Overpass.Polygon, l, style, color)]) l



let objects =
  List.flatten Dot.[
    (* Main PoIs I'm interested at *)
    node [("natural", "tree")] Bright_green ;
    node [("leisure", "playground")] Satin_trans_clear ;
    way [("leisure", "playground")] Satin_trans_clear ;
    node [("amenity", "public_bookcase")] Trans_orange ;
    (* Some good references PoIs *)
    node [("amenity", "bicycle_parking"); ("bicycle_parking", "wide_stands")] Dark_turquoise ;
    node [("amenity", "bicycle_parking"); ("bicycle_parking", "stands")] Dark_turquoise ;
    node [("amenity", "bicycle_parking"); ("bicycle_parking", "bollard")] Dark_turquoise ;
    node [("emergency", "fire_hydrant"); ("fire_hydrant:type", "pillar")] Coral ;
    (* Structurung elements *)
    vrway [("waterway", ".*")] Dark_azure ;
    eway [("highway", ["motorway"; "trunk"; "primary"; "secondary"; "tertiary"; "unclassified"; "road"])] Lavender ;
    eway [("highway", ["motorway_link"; "trunk_link"; "primary_link"; "secondary_link"; "tertiary_link"])] Lavender ;
    eway [("highway", ["residential"; "living_street"; "service"; "track"])] Yellow ;
    eway [("highway", ["pedestrian"; "footway"; "bridleway"; "path"; "cycleway"; "crossing"])] Bright_light_blue ;
    vrpoly [("building", ".*")] Bright_pink ;
    poly [("leisure", "playground")] Satin_trans_clear ;
    poly [("leisure", "parc")] Yellowish_green
  ]

