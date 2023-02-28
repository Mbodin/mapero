
open Dot

let to_rgb = function
  | Black -> (17, 17, 18)
  | Bright_green -> (24, 206, 46)
  | Bright_light_blue -> (180, 223, 255)
  | Bright_light_orange -> (254, 185, 22)
  | Bright_light_yellow -> (255, 233, 122)
  | Bright_pink -> (255, 191, 225)
  | Coral -> (255, 119, 107)
  | Dark_azure -> (48, 184, 242)
  | Dark_turquoise -> (10, 186, 184)
  | Lavender -> (216, 198, 240)
  | Light_aqua -> (203, 255, 243)
  | Medium_azure -> (139, 231, 255)
  | Satin_trans_clear -> (203, 204, 190)
  | Trans_orange -> (254, 146, 13)
  | Yellow -> (255, 209, 2)
  | Yellowish_green -> (233, 250, 156)
  | White | Letter _ -> (247, 247, 245)

let is_transparent = function
  | Satin_trans_clear
  | Trans_orange -> true
  | _ -> false

let to_html c =
  let (r, g, b) = to_rgb c in
  let to_hexa v =
    let str = Printf.sprintf "%x" v in
    match String.length str with
    | 0 -> "00"
    | 1 -> "0" ^ str
    | 2 -> str
    | n -> String.sub str (n - 3) 2 in
  Printf.sprintf "#%s%s%s" (to_hexa r) (to_hexa g) (to_hexa b)

