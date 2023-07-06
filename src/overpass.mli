
(* A response is decomposed into two parts: the one already present in the cache,
  and the one that will be ready once we got the full response from the Internet. *)
type 'a response = 'a * 'a Lwt.t

(* The request API.
  It is parameterised by types to be associated to nodes, ways, and polygons.
  These types are not manipulated internally: they are returned as-is. *)
module Make (R : sig
                   (* The representation (typically, graphical) of the node. *)
                   type node

                   (* The representation for ways *)
                   type way

                   (* The representation for polygon *)
                   type polygon
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

(* A list of objects, typically returned in a databse request. *)
type objects = {
  nodes : node list ;
  ways : way list ;
  polygons : polygon list ;
  partial : bool (* State whether it is a partial response (that is, whether further objects are expected to come afterwards for the same request.) *)
}

(* Get all the objects in a bbox.
  The actual answer only features the objects known at request time.
  There might be further objects that are discovered afterwards (see the partial field of the objects type).
  When being called, an asynchronous request to Overpass is done in the background.
  Each time that we get a new information, the function update is called.
  The second output is a function cancel that cuts these requests, stopping the fetching loop. *)
val get_objects : Bbox.t -> ?update:(objects -> unit) -> objects * (unit -> unit)

end

