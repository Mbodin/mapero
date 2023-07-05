
type 'a response = 'a * 'a Lwt.t

module IMap = Map.Make (Structures.IntOrder) (* TODO: Do we actually need to provide identifiers to objects in this file? *)

module AttrOrder : Set.OrderedType with type t = Osm.attributes = struct
    type t = Osm.attributes
    let compare : t -> t -> int = compare
  end

(* A store for attribute lists.
  In this context, they represent the attribute lists that have to be queried to display the map. *)
module AttrDiff = ListDiff.Make (AttrOrder)


module Make (R : sig
                   type node
                   type way
                   type polygon
                 end) = struct

(* OSM nodes. *)
type node = {
  coord : Geometry.real_coordinates ;
  repr : R.node
}

(* OSM ways. *)
type way = {
  nodes : Geometry.real_coordinates list (* The list of node coordinates composing the way. *) ;
  repr : R.way
}

(* OSM polygons. *)
type polygon = {
  nodes : Geometry.real_coordinates list (* The list of node coordinates composing the polygon. *) ;
  (* TODO: Include holes, as in multipolygons. *)
  repr : R.polygon
}

(* Modules to build the Zone interface. *)

module Punctual = struct
  type t = node
  let coordinates n = n.coord
end

module Spatial = struct
  type t =
    | Way of way
    | Polygon of polygon
  let bbox = function
    | Way w -> Bbox.enclosing w.nodes
    | Polygon p -> Bbox.enclosing p.nodes
end

module Area = Zone.Make (AttrDiff) (Spatial) (Punctual)

type cache_type = {
  pois : node IMap.t (* The PoIs of the map to be displayed *) ;
  ways : way IMap.t (* The ways. *) ;
  polygons : polygon IMap.t (* The polygons. *) ;
  zone : Area.t (* The zone which has already been requested. *) ;
  future_scans : (Bbox.t * AttrDiff.t) list (* Planned scans. *)
}

(* The global cache, to avoid calling Overpass too frequently. *)
let cache =
  ref {
    pois = IMap.empty ;
    ways = IMap.empty ;
    polygons = IMap.empty ;
    zone = Zone.empty ;
    future_scans = []
  }

let scan bbox =
  let scans = Area.where_to_scan !cache.zone bbox in (* TODO: Fill nice values for maxwidth, safe_factor, etc. *)
  cache := { !cache with future_scans = scans @ !cache.future_scans }

(* TODO: Add a runner that regularly checks for the next future scan. *)

end

