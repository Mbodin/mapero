
(* A type representing the map. *)
(* Warning: current implementation is imperative. *)
type t

(* Given the dimensions, create an empty map. *)
val empty : (int * int) -> t

(* Return the size of the map. *)
val get_size : t -> (int * int)

(* Coordinates on the map. *)
type coordinates = int * int

(* Float coordinates, representing raw OSM data, but with its origin
  at the same origin than the map. *)
type real_coordinates = float * float

val convert_coordinates_from_real : real_coordinates -> coordinates
val convert_coordinates_to_real : coordinates -> real_coordinates

(* The style of objects within the maps. *)
type style =
  | Round (* Must be round *)
  | Square (* Must be squared *)
  | Adaptive (* Following the shape of the drawn object. *)
  | Pattern of (int * int -> Dot.shape) (* Some kind of original pattern (e.g., waves). *)


(* The levels taken as argument below are priority levels, and not physical levels
  (in contrary to the convention in the Canvas module. *)

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
  ?style:style -> ?non_core_style:style option (* None means that the non-core parts won't be drawn. *) ->
  ?level:int -> Dot.color -> t

(* Draw a polygon, given as a list of coordinates.
  The style of the border and the inner part can be set independently. *)
val add_polygon : t -> real_coordinates list ->
  ?border_priority:Structures.priority -> ?inner_priority:Structures.priority ->
  ?border_style:style -> ?inner_style:style option (* None means that they won't be drawn. *) ->
  ?border_level:int -> ?inner_level:int ->
  Dot.color -> ?inner_color:Dot.color -> unit -> t

(* Add a text in a given position.
  The option states whether non-letter characters will be displayed, or whether the function
  will just skip them, leaving what was there before. *)
val add_text : t -> coordinates -> ?only_letters:bool ->
  ?priority:Structures.priority -> ?level:int -> string -> t

(* Convert the map into a matrice of dots, loosing the priority information. *)
val to_dot_matrix : t -> Dot.t option array array

