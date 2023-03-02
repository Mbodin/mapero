
(* Store to quickly access any part of the canvas. *)
type t

(* Create a canvas in the DOM. *)
val init : unit -> t

(* A link to a SVG group to be displayed in. *)
type g

(* Fetch a group at a given coordinate on the canvas.
  The third integer is the altitude: a stud is at altitude 1, a usual LEGO brick without
  stud at altitude 6, a usual tile at altitude 2. *)
val get : t -> int * int -> int -> g

