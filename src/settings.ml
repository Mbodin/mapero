
open Osm

let exact l = List.map (fun (k, v) -> (Exact k, Exact v)) l
let regexp l = List.map (fun (k, v) -> (Regexp k, Regexp v)) l
let vregexp l = List.map (fun (k, v) -> (Exact k, Regexp v)) l
let rec expand cxt = function
  | [] -> cxt []
  | (k, vs) :: l ->
    List.concat_map (fun v -> expand (fun l -> cxt ((Exact k, Exact v) :: l)) l) vs

let node l color =
  [(Node, exact l, color)]

let rnode l color =
  [(Node, regexp l, color)]

let rvnode l color =
  [(Node, vregexp l, color)]

let enode l color =
  expand (fun l -> [(Node, l, color)]) l

let way l color =
  [(Way, exact l, color)]

let rway l color =
  [(Way, regexp l, color)]

let vrway l color =
  [(Way, vregexp l, color)]

let eway l color =
  expand (fun l -> [(Way, l, color)]) l

let poly l color =
  [(Polygon, exact l, color)]

let rpoly l color =
  [(Polygon, regexp l, color)]

let vrpoly l color =
  [(Polygon, vregexp l, color)]

let epoly l color =
  expand (fun l -> [(Polygon, l, color)]) l


let objects = Dot.[
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


let empty_styles = {
  nodes = [] ;
  ways = [] ;
  polygons = []
}


let styles =
  let objects =
    let objects = List.rev objects in
    let objects = List.mapi (fun level objl -> (level, objl)) objects in
    let objects = List.to_seq objects in
    let objects =
      Seq.flat_map (fun (level, objl) ->
        let objl = List.to_seq objl in
        Seq.map (fun (kind, attributes, color) -> (kind, attributes, color, level)) objl) objects in
    objects in
  Seq.fold_left (fun styles (kind, attributes, color, level) ->
      match kind with
      | Node ->
        let style = {
          style = Geometry.Round ;
          color ;
          level ;
          priority = Structures.High
        } in
        { styles with nodes = (attributes, style) :: styles.nodes }
      | Way ->
        let base_style = {
          style = Geometry.Adaptive ;
          color ;
          level ;
          priority = Structures.Medium
        } in
        let style = {
          core = base_style ;
          non_core = Some { base_style with priority = Structures.VeryLow }
        } in
        { styles with ways = (attributes, style) :: styles.ways }
      | Polygon ->
        let base_style = {
          style = Geometry.Adaptive ;
          color ;
          level ;
          priority = Structures.Low
        } in
        let style = {
          border = base_style ;
          inner = Some { base_style with style = Geometry.Round ; priority = Structures.VeryLow }
        } in
        { styles with polygons = (attributes, style) :: styles.polygons }
    ) empty_styles objects

