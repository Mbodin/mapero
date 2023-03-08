
(* Store to access any part of the canvas. *)
type t

(* Create a canvas in the DOM.
  The provided function will be called whenever its size changes. *)
val init : (t -> unit) -> t

(* Apply a function to all visible coordinates. *)
val iter : (int * int -> unit) -> t -> unit

(* Remove all drawing made within the canvas. *)
val clear : t -> unit


module Lego : sig

(* Draw a base plate at the given coordinates. *)
val base_plate : t -> (int * int) -> ?size:(int * int) -> Dot.color -> unit

(* Draw a square tile at the given coordinates. *)
val square_tile : t -> (int * int) -> ?level:int -> ?size:(int * int) -> Dot.color -> unit

(* Draw a round tile at the given coordinates. *)
val round_tile : t -> (int * int) -> ?level:int -> ?diameter:int -> Dot.color -> unit

end

