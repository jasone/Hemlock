(** LALR(1) state transition, used as a key for IELR(1) inadequacy contributions. *)

open Basis
open! Basis.Rudiments

type t = {
  src: StateNub.Index.t;
  dst: StateNub.Index.t;
}

include IdentifiableIntf.S with type t := t

val init: src:StateNub.Index.t -> dst:StateNub.Index.t -> t

val cyclic: t -> bool
(** [cyclic t] returns true if the source and destination of [t] are equal. *)