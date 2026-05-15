open! Import

[%%template
[@@@kind_set.define all_ks_non_value = base_non_value]
[@@@kind_set.define all_ks = (all_ks_non_value, value_or_null)]

type nonrec ('ok, 'err) t =
  | Ok of 'ok
  | Error of 'err
[@@deriving sexp ~stackify, compare ~localize, equal ~localize, globalize]
[@@kind k = all_ks_non_value]

type nonrec ('a, 'b) t = ('a, 'b) Stdlib.result =
  | Ok of 'a
  | Error of 'b
[@@deriving sexp ~stackify, sexp_grammar, compare ~localize, equal ~localize, hash]
[@@kind k = (value_or_null_with_imm, value mod external_, value mod external64)]

let globalize = globalize_result
[@@kind k = (value_with_imm, value mod external_, value mod external64)]
;;]
