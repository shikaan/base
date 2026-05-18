@@ portable

[%%template:
[@@@kind_set.define all_ks_non_value = base_non_value]
[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

type ('ok : k, 'err : value_or_null) t =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp ~stackify, compare ~localize, equal ~localize, globalize]
[@@kind k = all_ks_non_value]

type ('ok : value_or_null, 'err : value_or_null) t = ('ok, 'err) Stdlib.result =
  | Ok of 'ok
  | Error of 'err
[@@deriving
  sexp ~stackify, sexp_grammar, compare ~localize, equal ~localize, hash, globalize]
[@@kind k = (value_or_null_with_imm, value mod external_, value mod external64)]]
