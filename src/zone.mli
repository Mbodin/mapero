
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
  the provided bbox, but without scanning too much. *)
val add : t -> bbox -> t * bbox list

