
(* A type to represent bounded boxes (bboxes). *)
type t = {
  min_x : float ;
  min_y : float ;
  max_x : float ;
  max_y : float
}

(* Create a bbox from two corner points. *)
val from_points : Geometry.real_coordinates -> Geometry.real_coordinates -> t

(* The center of a bbox. *)
val center : t -> Geometry.real_coordinates

(* Scale a bbox, conserving its center. *)
val scale : t -> float -> t

(* Given a bounding box and a point, whether the point is in the box. *)
val is_in : t -> Geometry.real_coordinates -> bool

(* Given two bboxes, whether they (non-strictly) overlap. *)
val overlap : t -> t -> bool

(* Whether a bbox is fully included into another. *)
val included : t -> t -> bool

(* The intersection of two bboxes. *)
val intersection : t -> t -> t

(* Get the minimal bbox which contains both its argument bboxes. *)
val outer : t -> t -> t

