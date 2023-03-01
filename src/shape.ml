
open Dot

type t = shape

let turn_clockwise = function
  | North -> West
  | West -> South
  | South -> East
  | East -> North

let uturn = function
  | North -> South
  | West -> East
  | South -> North
  | East -> West

let join s1 s2 =
  match s1, s2 with
  | Round, s | s, Round -> s
  | Square, _ | _, Square -> Square
  | Half_circle d1, Half_circle d2 ->
    if d1 = d2 then Half_circle d1
    else if d1 = turn_clockwise d2 then Quarter d1
    else if d2 = turn_clockwise d1 then Quarter d2
    else Square
  | Half_circle d1, Quarter d2 | Quarter d2, Half_circle d1 ->
    if d2 = d1 || turn_clockwise d2 = d1 then Quarter d2 else Square
  | Quarter d1, Quarter d2 ->
    if d1 = d2 then Quarter d1 else Square

