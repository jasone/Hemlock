(** Kernel compatibility. Some compatibility functions actually compute (in)equality, which enables
    no-op kernel merging. NB: `Compat` need not imply that kernels are unequal. *)

open! Basis
open! Basis.Rudiments

type t =
  | Incompat (** Incompatible. *)
  | Compat (** Compatible, possibly equal. *)
  | Equal (** Compatible and equal. *)

include IdentifiableIntf.S with type t := t
