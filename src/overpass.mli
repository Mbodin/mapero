
(* A response is decomposed into two parts: the one already present in the cache,
  and the one that will be ready once we got the full response from the Internet. *)
type 'a response = 'a * 'a Lwt.t

(* TODO: The request API. *)

