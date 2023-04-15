
(* A type to represent an aera already covered. *)
type t

(* The empty area. *)
val empty : t

(* Provide a bbox to be covered.
  Return the updated zone, as well as a list of smaller bbox to be scanned to properly cover
  the provided bbox, but without scanning too much.
  A maximum width and height can be provided: the larger elements will be split to fit these
  dimensions.
  Similarly, a minimum dimension can be provided: if so, the list of returned bbox may slightly
  overlap, but small rectangles that are close one with the other will be merged.
  A safe scale can be provided (for example 1.5) to enable the list of bbox returned to cover
  up to safe times the argument bbox in coverage. *)
val add : ?maxwidth:float -> ?maxheight:float -> ?minwidth:float -> ?minheight:float -> ?safe_factor:float -> t -> Bbox.t -> t * Bbox.t list

