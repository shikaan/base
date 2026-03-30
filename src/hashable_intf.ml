open! Import
module Sexp = Sexp0

module Definitions = struct
  (** We give a name to [Key__portable], even though it could normally be written as
      [sig @@ portable include Key end]. This is so [of_key__portable] can use it for
      first-class modules.

      @canonical Base.Hashable.Key *)
  module type%template
    [@modality p = (portable, nonportable)] [@mode m = (local, global)] Key = sig
    type t [@@deriving sexp_of]

    val compare : ([%compare: t][@mode.explicit m]) [@@mode m = (global, m)]

    (** Values returned by [hash] must be non-negative. An exception will be raised in the
        case that [hash] returns a negative value. *)
    val hash : t -> int
  end
end

module type Hashable = sig
  include module type of struct
    include Definitions
  end

  type 'a t =
    { hash : 'a -> int
    ; compare : 'a -> 'a -> int
    ; sexp_of_t : 'a -> Sexp.t
    }

  val equal : 'a t -> 'a t -> bool
  val poly : 'a t

  val%template of_key : 'a. ((module Key with type t = 'a)[@modality p]) -> 'a t
  [@@modality p = (portable, nonportable)]

  val%template to_key : 'a. 'a t -> ((module Key with type t = 'a)[@modality p])
  [@@modality p = (portable, nonportable)]

  val hash_param : int -> int -> 'a -> int
  val hash : 'a -> int
end
