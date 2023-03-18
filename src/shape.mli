
(* This file is meant to follow the signature of StructuresSig.DirectedSet. *)

type t = Dot.shape

(* Get the shape enclosing both shapes in argument. *)
val join : t -> t -> t

(* Functions to transform directions. *)
(* Turn of 90 degrees. *)
val turn_clockwise : Dot.direction -> Dot.direction

(* Turn of 180 degrees. *)
val uturn : Dot.direction -> Dot.direction

(* Flip upside-down. *)
val flip_y : Dot.direction -> Dot.direction

(* Flip left-right. *)
val flip_y : Dot.direction -> Dot.direction

