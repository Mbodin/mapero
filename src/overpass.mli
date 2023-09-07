
(* Most requests to the API lead to several actual requests to Overpass-Turbo.
  A request will thus lead to several (updated) responses, each more detailed than the previous
  ones, as all these requests to Overpass are being performed.
  All these responses should thus be handled by a continuation, provided as an argument.
  This continuation also takes as argument an argument, stating whether the response is yet
  partial or not: if this boolean is false, then this will be the last call to this continuation.
  A response also returns a function to cancel the later calls. *)
type 'a response = ('a -> bool -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t

(* A helper function to translate Osm.styles into a raw list of attributes. *)
val settings_to_attributes : Osm.styles -> Osm.attributes list

(* The request API.
  It is parameterised by types to be associated to nodes, ways, and polygons.
  These types are not manipulated internally: they are returned as-is.
  Functions converting attributes to the provided representations are also expected. *)
module Make (R : sig
                   (* The representation (typically, graphical) of the node. *)
                   type node

                   (* The representation for ways *)
                   type way

                   (* The representation for polygon *)
                   type polygon

                   (* Function converting attribute values to these types.
                     There is a special case for ways, as attributes can reveal that they are
                     actually polygons in the provided representation.
                     Indeed, at this point, only multipolygons will be tagged as polygons.
                     Typically, a closed line with attribute area=yes in OpenStreetMap will
                     here appear as a line, and we probably expect the to_way function to
                     convert it into a polygon. *)
                   val to_node : Osm.concrete_attributes -> node
                   val to_way : Osm.concrete_attributes -> (way, polygon) Either.t
                   val to_polygon : Osm.concrete_attributes -> polygon

                 end) : sig

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

(* A sequence of objects, typically returned in a database request. *)
type objects = {
  nodes : node Seq.t ;
  ways : way Seq.t ;
  polygons : polygon Seq.t
}

(* Set-up the conjunction of attributes that will be look-up when fetching objects from Overpass.
  Each Osm.attributes is a list, to be interpreted as a conjunction, and we can fetch a list.
  In contrary to the Osm module (which is meant to precisely describe the object styles), we
  do not here separate between nodes, ways, and polygons. *)
val set_lookup : Osm.attributes list -> unit

(* Get all the objects in a bbox. *)
val get_objects : Bbox.t -> objects response

end

