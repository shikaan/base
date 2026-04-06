@@ portable

[@@@warning "-incompatible-with-upstream"]

[%%template:
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]

[@@@kind_set.define
  all_non_val = (base_non_value, value_or_null & (base_or_null, kr1, kr2, kr3))]

type nonrec ('a : k) t =
  | None
  | Some of 'a
[@@kind k = all_non_val] [@@deriving compare ~localize]

type ('a : value_or_null) t = 'a option =
  | None
  | Some of 'a

(*_ Also expose the main [t] with explicit mangling. *)
type ('a : value_or_null) t = 'a option =
  | None
  | Some of 'a
[@@kind.explicit value_or_null]

[@@@kind.default k = (value_or_null, all_non_val)]

val is_none : ('a : k). ('a t[@kind k]) @ contended local -> bool
val is_some : ('a : k). ('a t[@kind k]) @ contended local -> bool]
