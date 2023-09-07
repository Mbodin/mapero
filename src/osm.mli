
(* The three graphical kinds of objects in OSM.
  Relations are ignored in this rendering. *)
type kind =
  | Node
  | Way
  | Polygon

(* A key or a value, which may be expressed either as an exact string or a regexp. *)
type estr =
  | Exact of string
  | Regexp of string

(* A conjunction of attribute and values that is meant to be searched. *)
type attributes = (estr (* key *) * estr (* value *)) list

(* An actual attribute of a given object. *)
type concrete_attributes = (string * string) list

(* A style, defining which objects are to be displayed, and how.
  It is parameterised by the graphical representation associated to each of the three kinds of objects. *)
type ('n, 'w, 'p) styles_repr = {
  nodes : (attributes * 'n) list ;
  ways : (attributes * 'w) list ;
  polygons : (attributes * 'p) list
}

(* The style of PoIs, among other things. *)
type basic_style = {
  style : Geometry.style ;
  color : Dot.color ;
  level : int (* The priority level, as defined in the Geometry module. *) ;
  priority : Structures.priority
}

(* The style of ways. *)
type way_style = {
  core : basic_style (* The style of the core parts (see the Geometry module) *) ;
  non_core : basic_style option
}

(* The style of polygons. *)
type polygon_style = {
  border : basic_style (* The style of the core parts (see the Geometry module) *) ;
  inner : basic_style option
}

(* The style for different objects. *)
type styles = (basic_style, way_style, polygon_style) styles_repr

