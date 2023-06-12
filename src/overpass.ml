
type 'a response = 'a * 'a Lwt.t

module IMap = Map.Make (Structures.IntOrder)

module AttrOrder : Set.OrderedType with type t = Osm.attributes = struct
    type t = Osm.attributes
    let compare : t -> t -> int = compare
  end

(* A store for attribute lists. *)
module AttrDiff = ListDiff.Make (AttrOrder)


(* OSM nodes. *)
type 'a node = {
  coord : Geometry.real_coordinates ;
  repr : 'a (* The representation (typically, graphical) of the node. *)
}

(* OSM ways. *)
type 'a way = {
  nodes : Geometry.real_coordinates list (* The list of node coordinates composing the way. *) ;
  repr : 'a (* The representation *)
}

(* OSM polygons. *)
type 'a polygon = {
  nodes : Geometry.real_coordinates list (* The list of node coordinates composing the polygon. *) ;
  repr : 'a (* The representation *)
}

type ('n, 'w, 'p) cache_type = {
  pois : 'n node IMap.t (* The PoIs of the map to be displayed *) ;
  ways : 'w way IMap.t ;
  polygons : 'p polygon IMap.t ;
  zone : Zone.t (* The zone which has already been requested. *)
}

(* The global cache, to avoid calling Overpass too frequently. *)
let cache =
  ref {
    pois = IMap.empty ;
    ways = IMap.empty ;
    polygons = IMap.empty ;
    zone = Zone.empty
  }

(* TODO: For each rectangle in the zone as well as for each kind (node, way, polygon),
  store the AttrDiff.id of the list of attributes that have already been queried:
  this is easy to update when changing the list of OSM objects we are interested at. *)
(* Warning: attributes are themselves a (conjunctive) list. *)

