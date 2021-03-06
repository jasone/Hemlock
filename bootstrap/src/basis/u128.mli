(** 128-bit unsigned integer type. *)

open Rudiments_int0

type t = u128

include Intnb_intf.S with type t := t

val to_u64: t -> u64
(** Convert to 64-bit unsigned integer, with possible loss. *)

val to_u64_hlt: t -> u64
(** Convert to 64-bit unsigned integer, or halt if conversion would be lossy. *)

val of_u64: u64 -> t
(** Initialize from a 64-bit unsigned integer. *)

val to_usize: t -> usize
(** Convert to default-width unsigned integer, with possible loss. *)

val to_usize_hlt: t -> usize
(** Convert to default-width unsigned integer, or halt if conversion would be
    lossy. *)

val of_usize: usize -> t
(** Initialize from a default-width unsigned integer. *)
