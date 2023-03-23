
(* A type representing the menu, overimposed over the map. *)
(* Warning: implementation is imperative. *)
type t

(* Initialise the menu with its dimensions. *)
val empty : (int * int) -> t

(* Return the size of the object. *)
val get_size : t -> (int * int)

(* Some printing actions can associate interaction actions to elements drawn in the menu. *)
type action =
  | NoAction (* No interaction *)
  | Link of string (* A URL *)
  | Action of (unit -> unit) (* An action to be performed when clicking at this place. *)


type large_piece = {
  start : int * int (* Start coordinate *) ;
  dim : int * int (* Its dimensions *) ;
  color : Dot.color ;
  sticker : string (* URL of an optionnal sticker. Empty string means no sticker. *)
}

(* The type of LEGO pieces that can be placed in a menu. *)
type piece =
  | Dot of Dot.t
  | Plate of large_piece (* A rectangular plate, with studs. The sticker must be empty. *)
  | Tile of large_piece (* A rectangular tile, without studs. *)
  | Round of large_piece (* A circular tile. Both its dimensions must be equal. *)
  | Occupied (* The place is occupied by a larger piece, defined in another cell. *)

(* Draw a menu block between the given positions. *)
val add_block : t -> (int * int) -> (int * int) -> Dot.color -> t

(* Print a text at the given coordinate. *)
val add_text : t -> (int * int) -> ?action:action -> string -> t

(* Adding a piece in a specific place.
  The piece can't be the specific value Occupied. *)
val add_piece : t -> (int * int) -> ?action:action -> piece -> t

(* Convert the menu into a matrice of pieces.
  The list represents the stack of submenus and their colors drawn upon it. *)
val to_matrix : t -> (Dot.color list * (piece * action) option) array array

