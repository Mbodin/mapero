
(* This file is meant to follow the signature of StructuresSig.DirectedSet. *)

type t = Dot.shape

(* Get the shape enclosing both shapes in argument. *)
val merge : t -> t -> t

let turn_clockwise : Dot.direction -> Dot.direction
let uturn : Dot.direction -> Dot.direction

