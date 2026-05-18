@@ portable

val permute
  : ('a : value_or_null mod separable).
  ?random_state:Random.State.t -> ?pos:int -> ?len:int -> 'a array @ local -> unit
