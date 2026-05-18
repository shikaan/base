@@ portable

module Uchar := Uchar0

external get : (bytes[@local_opt]) @ read -> (int[@local_opt]) -> char = "%bytes_safe_get"
external length : (bytes[@local_opt]) @ immutable -> int = "%bytes_length"

external unsafe_get
  :  (bytes[@local_opt]) @ read
  -> (int[@local_opt])
  -> char
  = "%bytes_unsafe_get"

external set
  :  (bytes[@local_opt])
  -> (int[@local_opt])
  -> (char[@local_opt])
  -> unit
  = "%bytes_safe_set"

external unsafe_set
  :  (bytes[@local_opt])
  -> (int[@local_opt])
  -> (char[@local_opt])
  -> unit
  = "%bytes_unsafe_set"

external unsafe_blit_string
  :  src:(string[@local_opt])
  -> src_pos:int
  -> dst:(bytes[@local_opt])
  -> dst_pos:int
  -> len:int
  -> unit
  = "caml_blit_string"
[@@noalloc]

external unsafe_get_int64
  :  (bytes[@local_opt]) @ read
  -> (int[@local_opt])
  -> int64
  = "%caml_bytes_get64u"

external unsafe_set_int64
  :  (bytes[@local_opt])
  -> (int[@local_opt])
  -> (int64[@local_opt])
  -> unit
  = "%caml_bytes_set64u"

external unsafe_get_int32
  :  (bytes[@local_opt]) @ read
  -> (int[@local_opt])
  -> int32
  = "%caml_bytes_get32u"

external unsafe_set_int32
  :  (bytes[@local_opt])
  -> (int[@local_opt])
  -> (int32[@local_opt])
  -> unit
  = "%caml_bytes_set32u"

external unsafe_get_int16
  :  (bytes[@local_opt]) @ read
  -> (int[@local_opt])
  -> int
  = "%caml_bytes_get16u"

external unsafe_set_int16
  :  (bytes[@local_opt])
  -> (int[@local_opt])
  -> (int[@local_opt])
  -> unit
  = "%caml_bytes_set16u"

val max_length : int
val blit : src:bytes @ read -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int -> unit

val blit_string
  :  src:string @ local
  -> src_pos:int
  -> dst:bytes @ local
  -> dst_pos:int
  -> len:int
  -> unit

val compare : bytes -> bytes -> int
val set_uchar_utf_8 : bytes @ local -> int -> Uchar.t -> int
val set_uchar_utf_16le : bytes @ local -> int -> Uchar.t -> int
val set_uchar_utf_16be : bytes @ local -> int -> Uchar.t -> int
val set_uchar_utf_32le : bytes @ local -> int -> Uchar.t -> int
val set_uchar_utf_32be : bytes @ local -> int -> Uchar.t -> int

external unsafe_fill
  :  bytes @ local
  -> pos:int
  -> len:int
  -> char
  -> unit
  = "caml_fill_bytes"
[@@noalloc]

val fill : bytes @ local -> pos:int -> len:int -> char -> unit
val empty : bytes
val get_empty : unit -> bytes [@@zero_alloc]

external unsafe_create_local : int -> bytes @ local = "Base_unsafe_create_local_bytes"
[@@noalloc]

val create_local : int -> bytes @ local unique [@@zero_alloc]

[%%template:
[@@@alloc.default a @ l = (heap @ global, stack @ local)]

val create : int -> bytes @ l unique [@@zero_alloc_if_stack a]
val make : int -> char -> bytes @ l unique [@@zero_alloc_if_stack a]
val map : bytes @ local read -> f:(char -> char) @ local -> bytes @ l unique
val mapi : bytes @ local read -> f:(int -> char -> char) @ local -> bytes @ l unique
val copy : bytes @ local read -> bytes @ l unique [@@zero_alloc_if_stack a]

val sub : bytes @ local read -> pos:int -> len:int -> bytes @ l unique
[@@zero_alloc_if_stack a]

val to_string : bytes @ local read -> string @ l unique [@@zero_alloc_if_stack a]
val of_string : string @ local -> bytes @ l unique [@@zero_alloc_if_stack a]]

external unsafe_blit
  :  src:(bytes[@local_opt]) @ read
  -> src_pos:int
  -> dst:(bytes[@local_opt])
  -> dst_pos:int
  -> len:int
  -> unit
  = "caml_blit_bytes"
[@@noalloc]

external unsafe_to_string
  :  no_mutation_while_string_reachable:(bytes[@local_opt])
  -> (string[@local_opt])
  = "%bytes_to_string"

external unique_to_string
  :  (bytes[@local_opt]) @ unique
  -> (string[@local_opt]) @ unique
  = "%bytes_to_string"

external unsafe_of_string_promise_no_mutation
  :  (string[@local_opt])
  -> (bytes[@local_opt])
  = "%bytes_of_string"
