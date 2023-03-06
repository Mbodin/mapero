
(* Store to access any part of the canvas. *)
type t

(* Create a canvas in the DOM.
  The provided function will be called whenever its size changes. *)
val init : (t -> unit) -> t

(* Apply a function to all visible coordinates. *)
val iter : (int * int -> unit) -> t -> unit

(* Remove all drawing made within the canvas. *)
val clear : t -> unit


(* Draw a rectangle filling the given cell and level.
  The size argument enables it to fill that many cells.
  If the proportion is less than 1., its borders will be shrinked by that many.
  We can also specify a rotation in degrees.
  Finally, the darken boolean can be specified to make the color darker. *)
val draw_rectangle : t -> (int * int) -> int -> ?size:(int * int) -> ?proportion:float -> ?rotation:float -> ?darken:bool -> Dot.color -> unit

(* Same, but for a circle. *)
val draw_circle : t -> (int * int) -> int -> ?diameter:int -> ?proportion:float -> ?rotation:float -> ?darken:bool -> Dot.color -> unit

(* TODO: draw shadow *)

