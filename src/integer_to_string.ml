open! Import

[%%template let int_to_string = Stdlib.string_of_int [@@alloc a = (heap, stack)]]
[%%template let int32_to_string = Stdlib.Int32.to_string [@@alloc a = (heap, stack)]]
[%%template let int64_to_string = Stdlib.Int64.to_string [@@alloc a = (heap, stack)]]

[%%template
  let nativeint_to_string = Stdlib.Nativeint.to_string [@@alloc a = (heap, stack)]]

[%%template let int64_u_to_string = int64_to_string [@@alloc a = (heap, stack)]]
