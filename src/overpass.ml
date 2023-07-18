
type 'a response = 'a * 'a Lwt.t

module IMap = Map.Make (Structures.IntOrder) (* TODO: Do we actually need to provide identifiers to objects in this file? *)

module AttrOrder : Set.OrderedType with type t = Osm.attributes = struct
    type t = Osm.attributes
    let compare : t -> t -> int = compare
  end

(* A store for attribute lists.
  In this context, they represent the attribute lists that have to be queried to display the map. *)
module AttrDiff = struct
  include ListDiff.Make (AttrOrder)
  let bot = add []
end

let settings_to_attributes styles =
  let translate l = List.map fst l in
  List.concat [
    translate styles.Osm.nodes ;
    translate styles.Osm.ways ;
    translate styles.Osm.polygons
  ]


module Make (R : sig
                   type node
                   type way
                   type polygon
                 end) = struct

type node = {
  coord : Geometry.real_coordinates ;
  repr : R.node
}

type way = {
  nodes : Geometry.real_coordinates list ;
  repr : R.way
}

type polygon = {
  nodes : Geometry.real_coordinates list ;
  repr : R.polygon
}

type objects = {
  nodes : node list ;
  ways : way list ;
  polygons : polygon list ;
  partial : bool
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

  (* From a [t list], fetch the list of ways and polygons. *)
  let partition =
    List.partition_map (function
      | Way w -> Either.Left w
      | Polygon p -> Either.Right p)

end

module Area = Zone.Make (AttrDiff) (Spatial) (Punctual)

(* Each scan request is associated to an identifier, which helps remove then afterwards if needed. *)
module ScanId : sig
  type t
  val get : unit -> t
  module Map : Map.S with type key = t
end = struct
  type t = int
  let get =
    let current = ref 0 in fun _ ->
    incr current ;
    !current
  module Map = Map.Make (Structures.IntOrder)
end

type cache_type = {
  (*pois : node IMap.t (* The PoIs of the map to be displayed *) ;
  ways : way IMap.t (* The ways. *) ;
  polygons : polygon IMap.t (* The polygons. *) ;*) (* TODO: Do we really need them? *)
  zone : Area.t (* The zone which has already been requested. *) ;
  future_scans : (Bbox.t * AttrDiff.t * ScanId.t) list (* Planned scans. *) ;
  lookup : AttrDiff.t (* The current set of attributes that we are interested in. *) ;
  continuations : (objects -> unit) ScanId.Map.t (* Continuations to call when the corresponding scan has been completed. *)
}

(* The global cache, to avoid calling Overpass too frequently. *)
let cache =
  ref {
    (* pois = IMap.empty ;
    ways = IMap.empty ;
    polygons = IMap.empty ; *)
    zone = Area.empty ;
    future_scans = [] ;
    lookup = AttrDiff.bot ;
    continuations = ScanId.Map.empty ;
  }

let set_lookup l =
  cache := { !cache with lookup = AttrDiff.add l }

(* Set a bbox for future scanning.
  The id is the scan identifier, and k is the knowledge information. *)
let scan id bbox k =
  let scans = Area.where_to_scan !cache.zone bbox k in (* TODO: Fill nice values for maxwidth, safe_factor, etc. *)
  let scans = List.map (fun (bbox, k) -> (bbox, k, id)) scans in
  cache := { !cache with future_scans = scans @ !cache.future_scans }
(* TODO: Add a runner that regularly checks for the next future scan.
  This runner should call soft_optimise after doing the request. *)

(* Remove everything about a scan identifier. *)
let remove id =
  cache := { !cache with
               future_scans = List.filter (fun (_bbox, _k, id') -> id <> id') !cache.future_scans ;
               continuations = ScanId.Map.remove id !cache.continuations }

(* As of get_objects, return all the objects within a given bbox, but without actually
  performing any request. *)
let get_objects_raw id bbox =
  let spatial = Area.get_spatial bbox ~partial:true !cache.zone in
  let (ways, polygons) = Spatial.partition spatial in
  {
    nodes = Area.get_punctual bbox !cache.zone ;
    ways = ways ;
    polygons = polygons ;
    partial = List.exists (fun (_bbox, _k, id') -> id' = id) !cache.future_scans
  }

let get_objects bbox ?(update=fun _ -> ()) =
  let id = ScanId.get () in
  cache := { !cache with continuations = ScanId.Map.add id update } ;
  scan id bbox !cache.lookup ;
  (get_objects_raw id bbox, fun _ -> remove id)

end

