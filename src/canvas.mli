
(* TODO: Who gets to fix the size of the map? Is it this file so that it can adapt to any resizing of the page, or is it the main file? *)

(* Initialise the svg for this size. *)
val init_svg : int * int -> unit (* TODO: Should probably return a t storing the g for quick access. *)

