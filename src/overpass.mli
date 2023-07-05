
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

  (* Set a bbox for future scanning. *)
  val scan : Bbox.t -> unit

  (* TODO *)

end

