(** Collection of state nubs that is organized into isocore sets, where each isocore set's LR(1)
    item sets are mutually incompatible despite having equal LR(0) cores.

    Once an isocores collection is fully populated, each state nub corresponds to a unique
    characteristic finite state machine (CFSM) state. By definition LALR(1) isocore sets are
    singletons, but more sophisticated algorithms may generate incompatible state nubs to prevent
    unnecessary grammar conflicts. *)

open! Basis
open! Basis.Rudiments

type t

val init: compat:(GotoNub.t -> StateNub.t -> bool) -> t
(** [init ~compat] creates an empty isocores collection for which LR(1) item set compatibility is
    determined by the [compat] function. *)

val mem: Lr0Itemset.t -> t -> bool
(** [mem core t] returns true if [t] contains an isocore set with the specified [core]. *)

val mems: Lr0Itemset.t -> t -> (StateNub.Index.t, StateNub.Index.cmper_witness) Ordset.t
(** [mems core t] returns the isocore set corresponding to the specificed [core]. *)

val get: GotoNub.t -> t -> StateNub.Index.t option
(** [get gotonub t] returns the state nub in [t] that is compatible with [gotonub], or [None] if no
    such state nub exists. *)

val get_hlt: GotoNub.t -> t -> StateNub.Index.t
(** [get gotonub t] returns the state nub in [t] that is compatible with [gotonub], or halts if no
    such state nub exists. *)

val get_core_hlt: Lr0Itemset.t -> t -> StateNub.Index.t
(** [get_core_hlt core t] gets the index of the state nub with isocore equal to that of [core],
    under the assumption that [t] was fully generated, using the LALR(1) algorithm. *)

val insert: Symbols.t -> GotoNub.t -> t -> StateNub.Index.t * t
(** [insert symbols gotonub t] constructs a state nub which incorporates [gotonub], inserts it into
    an incremental derivative of [t], and returns its index along with the derivative of [t]. *)

val merge: Symbols.t -> GotoNub.t -> StateNub.Index.t -> t -> bool * t
(** [merge symbols gotonub statenub_index t] merges [gotonub] into the state nub with given
    [statenub_index]. If the resulting state nub is distinct from the input, true is returned along
    with a derivative of [t] containing the resulting state nub; [false, t] otherwise. *)

val length: t -> uns
(** [length t] returns the number of state nubs in [t] (greater than or equal to the number of
    isocore sets). *)

val statenub: StateNub.Index.t -> t -> StateNub.t
(** [statenub statenub_index t] returns the state nub in [t] with given [statenub_index]. *)

val fold: init:'accum -> f:('accum -> StateNub.t -> 'accum) -> t -> 'accum
(** [fold ~init ~f t] iteratively applies [f] to the state nubs in [t], in increasing state nub
    index order. *)