@@ portable

open! Import

[%%template:
[@@@alloc.default a @ m = (heap_global, stack_local)]

val int64_u_to_string : int64# -> string @ m
val int_to_string : int -> string @ m
val nativeint_to_string : nativeint @ local -> string @ m
val int32_to_string : int32 @ local -> string @ m
val int64_to_string : int64 @ local -> string @ m]

[@@@ocaml.text {|/*|}]

module Private : sig
  module Constants : sig
    val pow10 : string
    val digit_pairs : string
  end
end
