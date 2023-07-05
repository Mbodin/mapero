
(* This file provides two interfaces: a simplified one (boolean) called zone, and a more general one,
  also storing objects and a notion of knowledge. *)

(* A type to store a boolean zone, storing for each place whether they are covered or not. *)
type zone

(* A empty zone. *)
val empty : zone

(* Provide a bbox to be covered.
  Return the updated zone, as well as a list of smaller bbox to be scanned to properly cover
  the provided bbox, but without scanning too much.
  A maximum width and height can be provided: the larger elements will be split to fit these
  dimensions.
  Similarly, a minimum dimension can be provided: if so, the list of returned bbox may slightly
  overlap, but small rectangles that are close one with the other will be merged.
  A safe scale can be provided (for example 1.5) to enable the list of bbox returned to cover
  up to safe times the argument bbox in coverage. *)
val add : ?maxwidth:float -> ?maxheight:float -> ?minwidth:float -> ?minheight:float -> ?safe_factor:float ->
  zone -> Bbox.t -> zone * Bbox.t list


(* The more general interface.
  It is parameterised by the notion of knowledge, which is assumed to be a set of elements of K.t.
  It also takes two notions of objects: spatial objects that are non-punctual, and punctual objects. *)
module Make (K : StructuresSig.Lattice)
            (S : sig
                (* Type of spatial objects *)
                type t

                (* Determine whether the object is within this bbox. *)
                val is_in_bbox : t -> Bbox.t -> bool
              end)
            (P : sig
                (* Type of punctual objects *)
                type t

                (* Get the coordinate of a punctual object. *)
                val coordinate : t -> Geometry.real_coordinates
              end) : sig

(* A more general type than zone. *)
type t

(* Empty area *)
val empty : t

(* Add a spatial object. *)
val add_spatial : t -> S.t -> t

(* Add a punctual object *)
val add_punctual : t -> P.t -> t

(* State that we now know a bbox up to a given level of knowledge. *)
val add_knowledge : t -> Bbox.t -> K.t -> t

(* If one wants to scan an aera for a given level of knowledge, this function
  returns a list of subareas to be scanned, each with the lacking level of
  knowledge.
  A maximum width and height can be provided: the larger elements will be split to fit these
  dimensions.
  Similarly, a minimum dimension can be provided: if so, the list of returned bbox may slightly
  overlap, but small rectangles that are close one with the other will be merged.
  A safe scale can be provided (for example 1.5) to enable the list of bbox returned to cover
  up to safe times the argument bbox in coverage. *)
val where_to_scan : ?maxwidth:float -> ?maxheight:float -> ?minwidth:float -> ?minheight:float -> ?safe_factor:float ->
  t -> Bbox.t -> K.t -> (Bbox.t * K.t) list

end

