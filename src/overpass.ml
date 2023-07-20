
type 'a response = 'a * 'a Lwt.t


(* A module to build Overpass requests. *)
module Request : sig
    (* Build an Overpass request given a bbox and a list of (conjunction of) attributes
      to be scanned. *)
    val build : Bbox.t -> Osm.attributes list -> string
  end = struct

(* Build the request part associated to a bbox. *)
let build_bbox bbox =
  Printf.sprintf "%f,%f,%f,%f"
    bbox.Bbox.min_y
    bbox.Bbox.min_x
    bbox.Bbox.max_y
    bbox.Bbox.max_x

(* Build the filter associated to a single attribute value. *)
let build_attribute (key, value) =
  (* Some values are not accepted by Overpass as-is. *)
  let normalise = function
    | Exact "" -> Regexp "^$"
    | v -> v in
  match normalise key, normalise value with
  | Exact key, Exact value -> Printf.sprintf "[\"%s\"=\"%s\"]" key value
  | Exact key, Regexp value -> Printf.sprintf "[\"%s\"~\"%s\"]" key value
  | Regexp key, Exact value -> Printf.sprintf "[~\"%s\"~\"%s\"]" key (String.escaped value)
  | Regexp key, Regexp value -> Printf.sprintf "[~\"%s\"~\"%s\"]" key value

(* Build the request part associated to a single conjunction of attributes. *)
let build_unit attrs =
  Printf.sprintf "nwr%s;"
    (String.concat "" (List.map build_attribute attrs))

let build bbox attrs =
  Printf.sprintf "[out:json][bbox:%s];(%s);(._;>>;);out body;"
    (build_bbox bbox)
    (String.concat "" (List.map build_unit attrs))

end

(* A module to perform an Overpass request. *)
module Get : sig
    (* TODO *)
  end = struct

let build_url request =
  Printf.sprintf "https://overpass-api.de/api/interpreter?data=%s"
    (Url.urlencode request)

(* TODO:
let _ request =
  match Url.url_of_string (build_url request) with
  | None -> assert false
  | Some url -> ??
  or
  let%lwt json = XmlHttpRequest.get (build_url request) in
  ??
*)

end


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
  nodes : node Seq.t ;
  ways : way Seq.t ;
  polygons : polygon Seq.t ;
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

  (* From a [t Seq.t], fetch the list of ways and polygons. *)
  let partition =
    Seq.partition_map (function
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

(* A runner that regularly checks for the next future scan.
  This runner calls soft_optimise after doing the request. *)
module Runner : sig
    (* A type to make sure the runner is not deallocated. *)
    type t

    (* Initialise the runner. *)
    val init : t

    (* Add a task to the runner.
      The boolean states whether the task should be called afterwards or not. *)
    val add_task : t -> (unit -> bool Lwt.t) -> t
  end = struct

(* The runner is just a light-thread type.
  Storing it makes sure that it won't get desallocated. *)
type t = unit Lwt.t

let init = Lwt.return ()

let add_task runner task =
  let%lwt () = runner in
  Lwt.return () (* TODO *)

end

type cache_type = {
  (*pois : node IMap.t (* The PoIs of the map to be displayed *) ;
  ways : way IMap.t (* The ways. *) ;
  polygons : polygon IMap.t (* The polygons. *) ;*) (* TODO: Do we really need them? *)
  zone : Area.t (* The zone which has already been requested. *) ;
  future_scans : (Bbox.t * AttrDiff.t * ScanId.t * Bbox.t) Seq.t (* Planned scans, with the corresponding bbox, knowledge level, scan id, and the overall bbox. *) ;
  lookup : AttrDiff.t (* The current set of attributes that we are interested in. *) ;
  continuations : (objects -> unit) ScanId.Map.t (* Continuations to call when the corresponding scan has been completed. *) ;
  runner : Runner.t (* Some information about the runner. *)
}

(* The global cache, to avoid calling Overpass too frequently. *)
let cache =
  ref {
    (* pois = IMap.empty ;
    ways = IMap.empty ;
    polygons = IMap.empty ; *)
    zone = Area.empty ;
    future_scans = Seq.empty ;
    lookup = AttrDiff.bot ;
    continuations = ScanId.Map.empty ;
  }

let set_lookup l =
  cache := { !cache with lookup = AttrDiff.add l }


(* As of get_objects, return all the objects within a given bbox, but without actually
  performing any request. *)
let get_objects_raw id bbox =
  let spatial = Area.get_spatial bbox ~partial:true !cache.zone in
  let (ways, polygons) = Spatial.partition spatial in
  {
    nodes = Area.get_punctual bbox !cache.zone ;
    ways = List.of_seq ways ;
    polygons = List.of_seq polygons ;
    partial = Seq.exists (fun (_bbox, _k, id', _bbox_englobing) -> id' = id) !cache.future_scans
  }

(* Get the first task to be performed, and scans it. *)
let runner_task () =
  let%lwt () =
    match Seq.uncons !cache.future_scans with
    | None -> Lwt.return ()
    | Some ((bbox, k, id, bbox_englobing), s) ->
      cache := { !cache with future_scans = s } ;
      let request = Request.build bbox (AttrDiff.to_list k) in
      let zone =
        let objects = (* TODO: Use Get. *)
          {
            nodes = Seq.empty ;
            ways = Seq.empty ;
            polygons = Seq.empty ;
            partial = false (* It is not partial for the inner bbox. *)
          } in
        let zone = Seq.fold_left Area.add_punctual zone objects.nodes in
        let zone =
          Seq.fold_left (fun w -> Area.add_spatial (Spatial.Way w)) zone objects.ways in
        let zone =
          Seq.fold_left (fun p -> Area.add_spatial (Spatial.Polygon p)) zone objects.polygons in
        zone in
      cache := { !cache with zone = zone } ;
      let () =
        match ScanId.MapScanId.Map.find_opt id !cache.continuations with
        | None -> ()
        | Some f -> f (get_objects_raw id bbox_englobing) in
      Lwt.return () in
  Lwt.return (not (Seq.is_empty !cache.future_scans))

(* Set a bbox for future scanning.
  The id is the scan identifier, and k is the knowledge information. *)
let scan id bbox k =
  let scans = Area.where_to_scan !cache.zone bbox k in (* TODO: Fill nice values for maxwidth, safe_factor, etc. *)
  let scans = List.map (fun (bbox', k) -> (bbox', k, id, bbox)) scans in
  let scans = List.to_seq scans in
  cache := { !cache with future_scans = Seq.append scans !cache.future_scans } ;
  cache := { !cache with runner = Runner.add_task !cache.runner runner_task }

(* Remove everything about a scan identifier. *)
let remove id =
  cache := { !cache with
               future_scans =
                 Seq.filter (fun (_bbox, _k, id', _bbox_englobing) -> id <> id')
                 !cache.future_scans ;
               continuations = ScanId.Map.remove id !cache.continuations }

let get_objects bbox ?(update=fun _ -> ()) =
  let id = ScanId.get () in
  cache := { !cache with continuations = ScanId.Map.add id update } ;
  scan id bbox !cache.lookup ;
  (get_objects_raw id bbox, fun () -> remove id)

end

