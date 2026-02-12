open Basis
open! Basis.Rudiments

module T = struct
  type t =
    | Incompat
    | Compat
    | Equal

  let to_uns t =
    match t with
    | Incompat -> 0L
    | Compat -> 1L
    | Equal -> 2L

  let to_string t =
    match t with
    | Incompat -> "Incompat"
    | Compat -> "Compat"
    | Equal -> "Equal"

  let hash_fold t state =
    state |> Uns.hash_fold (to_uns t)

  let cmp t0 t1 =
    Uns.cmp (to_uns t0) (to_uns t1)

  let pp t formatter =
    formatter
    |> Fmt.fmt (to_string t)
end
include T
include Identifiable.Make(T)
