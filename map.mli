
(* A type representing the map. *)
type t

(* Given the dimensions, create an empty map. *)
val empty : int * int -> t

(* The style of objects within the maps. *)
type style =
  | Round (* Must be round *)
  | Square (* Must be squared *)
  | Adaptive (* Round by default, but can adapt if needed be. *)


(* Add a single dot to the map, given its coordinates. *)
val add_PoI : t -> int * int -> ?priority:Structures.priority -> ?style:style -> Dot.t -> t


(* Add a line to the map.
  A line is composed of “core” and “non-core” parts: the core are necessary to see the line
  (“#” below), and the non-core (“*” below) can be used to make the line thicker:
   #*
    ##*
      ##
 *)
val add_line : t -> int * int (* start coordinates *) -> int * int (* end coordinates *) ->
  ?priority:Structures.priority (* priority of the core part of the line *) ->
  ?non_core_priority:Structures.priority (* priority of the non-core parts of the line. *) ->
  ?style:style -> ?non_core_style:style option (* None means that the non-core parts won't be drawn. *) ->
  Dot.color -> t

(* Draw a polygon, given as a list of coordinates.
  The style of the border and the inner part can be set independently. *)
val add_polygon : t -> (int * int) list ->
  ?border_priority:Structures.priority -> ?inner_priority:Structures.priority ->
  ?border_style:style -> ?inner_style:style option (* None means that they won't be drawn. *) ->
  Dot.color -> ?inner_color:Dot.colot -> t

(* Add a text in a given position. Spaces and non-letter characters will not be displayed. *)
val add_text : t -> int * int -> string -> t

(* Convert the map into a matrice of dots, loosing the priority information. *)
val to_dot_matrix : t -> Dot.t array array

