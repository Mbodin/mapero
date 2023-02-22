
module type DirectedSet = begin

  type t

  val join : t -> t -> t

end

