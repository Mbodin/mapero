
type kind =
  | Node
  | Way
  | Polygon


(* A key or a value, which may be expressed either as an exact string or a regexp. *)
type estr =
  | Exact of string
  | Regexp of string

type attributes = (estr (* key *) * estr (* value *)) list

