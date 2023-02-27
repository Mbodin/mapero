
open Dot

let to_rgb = function
  (* TODO. *)


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

