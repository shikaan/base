[%%template:
[@@@kind kr1 = (value_or_null & value_or_null)]
[@@@kind kr2 = (value_or_null & kr1)]

[@@@kind_set.define
  all_ks_non_value = (base_non_value, value_or_null & (base_or_null, kr2))]

[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

type nonrec 'a t =
  | None
  | Some of 'a
[@@kind k = all_ks_non_value] [@@deriving compare ~localize]

type 'a t = 'a option =
  | None
  | Some of 'a

(*_ Also expose the main [t] with explicit mangling. *)
type 'a t = 'a option =
  | None
  | Some of 'a
[@@kind.explicit value_or_null]

[@@@kind.default k = all_ks]

val is_none : 'a. ('a t[@kind k]) -> bool
val is_some : 'a. ('a t[@kind k]) -> bool]
