
(* A type to represent an aera already covered. *)
type t

(* The empty area. *)
val empty : t

(* A bounding box defined by its minimum and maximum values for x and y. *)
type bbox = {
  min_x : float ;
  min_y : float ;
  max_x : float ;
  max_y : float
}

(* Define a bounding box from two coordinates. *)
val to_bbox : Geometry.real_coordinates -> Geometry.real_coordinates -> bbox

(* Provide a bbox to be covered.
  Return the updated zone, as well as a list of smaller bbox to be scanned to properly cover
  the provided bbox, but without scanning too much.
  A maximum width and height can be provided: the larger elements will be split to fit these
  dimensions.
  A safe scale can be provided (for example 1.5) to enable the list of bbox returned to cover
  up to safe times the argument bbox in coverage. *)
val add : t -> ?maxwidth:float -> ?maxheight:float -> ?safe:float -> bbox -> t * bbox list

