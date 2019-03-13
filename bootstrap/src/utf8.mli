(* Partial Rudiments. *)
module Int = I63
module Codepoint = U21
module Byte = U8
type int = Int.t
type codepoint = Codepoint.t
type byte = Byte.t

type t

include Cmpable_intf.S with type t := t

val of_codepoint: codepoint -> t
val to_codepoint: t -> codepoint

module Seq : sig
  type outer = t
  module type S = sig
    type t
    val to_utf8: t -> ((outer, byte list) result * t) option
    val to_utf8_hlt: t -> (outer * t) option
  end

  module Make (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t
  module Make_rev (T : Seq_intf.I_mono_indef with type elm := byte) :
    S with type t := T.t
end

val to_bytes: t -> byte list

val length: t -> int