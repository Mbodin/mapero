
(* A type representing the map. *)
type t

(* Given the dimensions, create an empty map. *)
val empty : int * int -> t

(* Coordinates on the map. *)
type coordinates = int * int

(* Float coordinates, representing raw OSM data. *)
type real_coordinates = float * float

val convert_real_coordinates : real_coordinates -> coordinates

(* The style of objects within the maps. *)
type style =
  | Round (* Must be round *)
  | Square (* Must be squared *)
  | Adaptive (* Following the shape of the drawn object. *)


(* Add a single dot to the map, given its coordinates. *)
val add_PoI : t -> real_coordinates -> ?priority:Structures.priority -> ?level:int -> ?style:style -> Dot.color -> t


(* Add a line to the map.
  A line is composed of “core” and “non-core” parts: the core are necessary to see the line
  (“#” below), and the non-core (“*” below) can be used to make the line thicker:
   #*
    ##*
      ##
 *)
val add_line : t -> real_coordinates (* start coordinates *) -> real_coordinates (* end coordinates *) ->
  ?priority:Structures.priority (* priority of the core part of the line *) ->
  ?non_core_priority:Structures.priority (* priority of the non-core parts of the line. *) ->
  ?level:int -> ?style:style -> ?non_core_style:style option (* None means that the non-core parts won't be drawn. *) ->
  Dot.color -> t

(* Draw a polygon, given as a list of coordinates.
  The style of the border and the inner part can be set independently. *)
val add_polygon : t -> real_coordinates list ->
  ?border_priority:Structures.priority -> ?inner_priority:Structures.priority ->
  ?border_style:style -> ?inner_style:style option (* None means that they won't be drawn. *) ->
  ?border_level:int -> ?inner_level:int ->
  Dot.color -> ?inner_color:Dot.colot -> t

(* Add a text in a given position.
  The option states whether non-letter characters will be displayed, or whether the function
  will just skip them, leaving what was there before. *)
val add_text : t -> coordinates -> ?only_letters:bool ->
  ?priority:Structures.priority -> ?level:int -> string -> t

(* Convert the map into a matrice of dots, loosing the priority information. *)
val to_dot_matrix : t -> Dot.t array array

