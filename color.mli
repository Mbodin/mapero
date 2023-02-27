
(* Convert a color into the three RGB components, between 0 and 255.
  The special construction Letter gets mapped to its white background color. *)
val to_rgb : Dot.color -> int * int * int

(* Convert a color into a #hexa code. *)
val to_html : Dot.color -> string

