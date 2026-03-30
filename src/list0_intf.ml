module type List0 = sig
  module%template Constructors : sig
    type 'a t =
      | []
      | ( :: ) of 'a * ('a t[@kind.explicit k])
    [@@kind.explicit
      k
      = ( base_non_value
        , value_or_null & value_or_null
        , value_or_null & value_or_null & value_or_null
        , value_or_null & value_or_null & value_or_null & value_or_null )]
    [@@deriving compare ~localize, equal ~localize]

    type 'a t = 'a list [@@deriving compare ~localize, equal ~localize]

    (*_ Expose the main [t] with explicit mangling. Having this last also makes it the
        default constructors. *)
    type 'a t = 'a list =
      | []
      | ( :: ) of 'a * 'a t
    [@@kind.explicit value_or_null]
  end

  open Constructors

  val max_non_tailcall : int

  val%template hd_exn : 'a. 'a t -> 'a [@@mode l = (global, local)]
  val%template tl_exn : 'a. 'a t -> 'a t [@@mode l = (global, local)]

  val unzip : 'a 'b. ('a * 'b) t -> 'a t * 'b t
  val is_empty : 'a. 'a list -> bool

  val%template partition_map
    : 'a 'b 'c.
    'a list -> f:('a -> ('b, 'c) Either0.t) -> 'b list * 'c list
  [@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]

  [%%template:
  [@@@kind.default
    k
    = ( base_or_null
      , value_or_null & value_or_null
      , value_or_null & value_or_null & value_or_null
      , value_or_null & value_or_null & value_or_null & value_or_null )]

  val length : 'a. ('a t[@kind k]) -> int
  val exists : 'a. ('a t[@kind k]) -> f:('a -> bool) -> bool [@@mode l = (local, global)]
  val iter : 'a. ('a t[@kind k]) -> f:('a -> unit) -> unit [@@mode l = (local, global)]

  val rev_append : 'a. ('a t[@kind k]) -> ('a t[@kind k]) -> ('a t[@kind k])
  [@@alloc __ @ l = (stack_local, heap_global)]

  val rev : 'a. ('a t[@kind k]) -> ('a t[@kind k])
  [@@alloc __ @ l = (stack_local, heap_global)]

  val for_all : 'a. ('a t[@kind k]) -> f:('a -> bool) -> bool [@@mode l = (local, global)]

  [@@@kind ka = k]

  val fold : 'a 'b. ('a t[@kind ka]) -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  [@@mode ma = (local, global), mb = (local, global)]
  [@@kind
    ka = ka
    , kb
      = ( base_or_null
        , value_or_null & base_or_null
        , value_or_null & value_or_null & value_or_null
        , value_or_null & value_or_null & value_or_null & value_or_null )]

  [@@@kind.default kb = base_or_null]

  val rev_map : 'a 'b. ('a t[@kind ka]) -> f:('a -> 'b) -> ('b t[@kind kb])
  [@@mode ma = (local, global)] [@@alloc __ @ mb = (stack_local, heap_global)]]

  val fold2_ok : 'a 'b 'c. 'a t -> 'b t -> init:'c -> f:('c -> 'a -> 'b -> 'c) -> 'c
  val exists2_ok : 'a 'b. 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool

  val%template iter2_ok : 'a 'b. 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
  [@@mode l = (global, local)]

  val for_all2_ok : 'a 'b. 'a t -> 'b t -> f:('a -> 'b -> bool) -> bool
  val nontail_map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t
  val rev_map2_ok : 'a 'b 'c. 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val nontail_mapi : 'a 'b. 'a t -> f:(int -> 'a -> 'b) -> 'b t
  val partition : 'a. 'a t -> f:('a -> bool) -> 'a t * 'a t

  val%template fold_right : 'a 'acc. 'a t -> f:('a -> 'acc -> 'acc) -> init:'acc -> 'acc
  [@@mode l = (local, global), mcc = (local, global)]

  val fold_right2_ok : 'a 'b 'c. 'a t -> 'b t -> f:('a -> 'b -> 'c -> 'c) -> init:'c -> 'c
end
