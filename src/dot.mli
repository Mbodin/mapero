
(* Colors from the LEGO dots sets (* Commented out are colors appearing in these sets,
  but not as dots. *). The list is currently not complete. *)
type color =
  | Black
  (* | Blue *)
  | Bright_green
  | Bright_light_blue
  | Bright_light_orange
  | Bright_light_yellow
  | Bright_pink
  | Coral
  | Dark_azure
  (* | Dark_bluish_gray *)
  (* | Dark_pink *)
  | Dark_turquoise
  | Lavender
  | Light_aqua
  (* | Light_bluish_gray *)
  | Medium_azure
  (* | Orange *)
  (* | Pearl_gold *)
  | Satin_trans_clear
  (* | Tan *)
  | Trans_orange
  | Yellow
  | Yellowish_green
  | White
  | Letter of char (* A special “color”, for letters. *)

type direction =
  | North
  | West
  | South
  | East

(* The shape of the dot.
  Note how we assume that dots are aligned with the grid: no diagonnally-placed dots are encodable
  here, although they are possible within the LEGO system (but constraining nearby dots). *)
type shape =
  | Round
  | Square
  | Half_circle of direction (* The direction of the straight border. *)
  | Quarter of direction (* The direction of the first straight border clockwise. *)


type t = shape * color

