(** 64-bit unsigned integer type. *)

open Rudiments_int0

type t = u64

include Intnb_intf.S with type t := t

val to_i64: t -> i64
(** Convert an unsigned integer to a bitwise identical signed integer. *)

val of_i64: i64 -> t
(** Convert a signed integer to a bitwise identical unsigned integer. *)

val to_usize: t -> usize
(** Convert to default-width unsigned integer, with possible loss. *)

val to_usize_hlt: t -> usize
(** Convert to default-width unsigned integer, or halt if conversion would be
    lossy. *)

val of_usize: usize -> t
(** Initialize from a default-width unsigned integer. *)
