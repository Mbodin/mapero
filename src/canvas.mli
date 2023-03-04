
(* Store to access any part of the canvas. *)
type t

(* Create a canvas in the DOM.
  The provided function will be called whenever its size changes. *)
val init : (t -> unit) -> t

(* A link to a SVG group to be displayed in. *)
type g

(* Fetch a group at a given coordinate on the canvas.
  The third integer is the altitude: a stud is at altitude 1, a usual LEGO brick without
  stud at altitude 6, a usual tile at altitude 2. *)
val get : t -> int * int -> int -> g

(* Apply a function to all the currently used coordinates and levels. *)
val for_all_used : (int * int -> int -> unit) -> t -> unit

(* Apply a function to all visible coordinates. *)
val for_all_visible : (int * int -> unit) -> t -> unit

(* Remove all drawing made within the canvas. *)
val clear : t -> unit


(* Draw a rectangle filling a given proportion of the given cell,
  rotated by this many degrees. *)
val draw_rectangle : g -> ?proportion:float -> ?rotation:float -> Dot.color -> unit

(* Same, but for a circle. *)
val draw_circle : g -> float -> ?rotation:float -> Dot.color -> unit

(* TODO: draw shadow *)

