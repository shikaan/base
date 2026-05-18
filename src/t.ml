(** This module defines various abstract interfaces that are convenient when one needs a
    module that matches a bare signature with just a type. This sometimes occurs in
    functor arguments and in interfaces. *)

module type T = sig
  type t
end

[%%template
[@@@kind_set.define smaller_all = (any, value)]
[@@@kind_set.define all = (smaller_all, value_or_null)]

module type T1 = sig
  type ('a : ka) t
end
[@@kind.explicit_plus_unmangled ka = all]

module type T2 = sig
  type ('a : ka, 'b : kb) t
end
[@@kind.explicit_plus_unmangled ka = all, kb = all]

[@@@kind.default ka = smaller_all, kb = smaller_all, kc = smaller_all]

module type T3 = sig
  type ('a : ka, 'b : kb, 'c : kc) t
end

[@@@kind.default kd = smaller_all]

module type T4 = sig
  type ('a : ka, 'b : kb, 'c : kc, 'd : kd) t
end]
