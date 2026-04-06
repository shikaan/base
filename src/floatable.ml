(** Module type with float conversion functions. *)

open! Import

[%%template
[@@@modality.default p = (nonportable, portable)]

module type S = sig @@ p
  type t

  val of_float : float -> t
  val to_float : t -> float
end]

module type S_local_input = sig
  type t

  val of_float : local_ float -> t
  val to_float : local_ t -> float
end
