open Basis
open! Basis.Rudiments

module T = struct
  type t = (
    StateIndex.t, (* Conflict state. *)
    Attribs.t, (* Symbol -> {ergo kernel items, contrib} attributions. *)
    StateIndex.cmper_witness
  ) Ordmap.t

  let hash_fold t state =
    state |> Ordmap.hash_fold Attribs.hash_fold t

  let cmp t0 t1 =
    Ordmap.cmp Attribs.cmp t0 t1

  let fmt ?(alt=false) ?(width=0L) t formatter =
    match alt with
    | false -> formatter |> Ordmap.pp Attribs.pp t
    | true -> formatter |> Ordmap.fmt ~alt ~width Attribs.pp t

  let pp t formatter =
    fmt t formatter

  let fmt_hr symbols prods ?(alt=false) ?(width=0L) t formatter =
    List.fmt ~alt ~width (fun (conflict_state_index, attribs) formatter ->
      formatter
      |> StateIndex.pp conflict_state_index
      |> Fmt.fmt " = "
      |> Attribs.fmt_hr ~alt ~width:(width + 4L) symbols prods attribs
    ) (Ordmap.to_alist t) formatter
end
include T
include Identifiable.Make(T)

let length t =
  match Ordmap.is_empty t with
  | true -> 0L
  | false ->
    t
    |> Ordmap.map ~f:(fun (_conflict_state_index, attribs) -> Attribs.length attribs)
    |> Ordmap.reduce_hlt ~f:Uns.(+)

let equal t0 t1 =
  Ordmap.equal Attribs.equal t0 t1

module Seq = struct
  type container = t
  type elm = StateIndex.t * Attribs.Akey.t * Attribs.Aval.t
  type t = {
    l: uns;
    conflict_state_index_opt: uns option;
    seq_inner_opt: Attribs.Seq.t option;
    seq_outer: (
      StateIndex.t,
      Attribs.t,
      StateIndex.cmper_witness
    ) Ordmap.Seq.t;
  }

  let init container =
    {
      l=length container;
      conflict_state_index_opt=None;
      seq_inner_opt=None;
      seq_outer=Ordmap.Seq.init container;
    }

  let length {l; _} =
    l

  let next t =
    let rec normalize ({l; seq_inner_opt; _} as t) = begin
      let advance ({seq_outer; _} as t) = begin
        let (conflict_state_index, attribs), seq_outer =
          Ordmap.Seq.next seq_outer in
        let seq_inner = Attribs.Seq.init attribs in
        normalize {t with
          conflict_state_index_opt=Some conflict_state_index;
          seq_inner_opt=Some seq_inner;
          seq_outer;
        }
      end in
      assert Uns.(l > 0L);
      match seq_inner_opt with
      | Some seq_inner -> begin
          match Attribs.Seq.length seq_inner with
          | 0L -> advance t
          | _ ->
            t.l,
            Option.value_hlt t.conflict_state_index_opt,
            Option.value_hlt t.seq_inner_opt,
            t.seq_outer
        end
      | None -> advance t
    end in
    match normalize t with
    | l, conflict_state_index, seq_inner, seq_outer -> begin
        let (_akey, (akey, aval)), seq_inner = Attribs.Seq.next seq_inner in
        (conflict_state_index, akey, aval), {
          l=pred l;
          conflict_state_index_opt=Some conflict_state_index;
          seq_inner_opt=Some seq_inner;
          seq_outer;
        }
      end

  let next_opt t =
    match length t with
    | 0L -> None
    | _ -> Some (next t)
end

let empty = Ordmap.empty (module StateIndex)

let singleton ~conflict_state_index akey aval =
  let attribs =  Attribs.singleton akey aval in
  match Attribs.is_empty attribs with
  | true -> empty
  | false ->  Ordmap.singleton (module StateIndex) ~k:conflict_state_index ~v:attribs

let reindex index_map t =
  Ordmap.fold ~init:empty ~f:(fun reindexed_t (state_index, attribs) ->
    match Map.get state_index index_map with
    | None -> reindexed_t
    | Some state_index' -> Ordmap.insert ~k:state_index' ~v:attribs reindexed_t
  ) t

let is_empty t =
  Uns.(=) (length t) 0L

let get ~conflict_state_index symbol_index t =
  match Ordmap.get conflict_state_index t with
  | None -> None
  | Some attribs -> begin
      match Attribs.get symbol_index attribs with
      | None -> None
      | Some (_akey, _aval) as akey_aval_opt -> akey_aval_opt
    end

let get_hlt ~conflict_state_index symbol_index t =
  get ~conflict_state_index symbol_index t
  |> Option.value_hlt

let contains ~conflict_state_index symbol_index aval t =
  assert (not (Attribs.Aval.is_empty aval));
  match get ~conflict_state_index symbol_index t with
  | None -> false
  | Some (_akey_existing, aval_existing) -> Attribs.Aval.(inter aval_existing aval = aval)

let amend ~conflict_state_index akey ~f t =
  let attribs = match Ordmap.get conflict_state_index t with
    | None -> Attribs.empty
    | Some attribs -> attribs
  in
  let attribs' = Attribs.amend akey ~f attribs in
  Ordmap.upsert ~k:conflict_state_index ~v:attribs' t

let insert ~conflict_state_index akey aval t =
  match Attribs.Aval.is_empty aval with
  | true -> t
  | false ->
    Ordmap.amend conflict_state_index t ~f:(function
      | None -> Some (Attribs.singleton akey aval)
      | Some attribs -> begin
          Some (
            Attribs.amend akey attribs ~f:(function
              | None -> Some aval
              | Some aval_prev -> Some (Attribs.Aval.union aval aval_prev)
            )
          )
        end
    )

let fold_until ~init ~f t =
  Ordmap.fold_until ~init ~f:(fun accum (conflict_state_index, attribs) ->
    Attribs.fold_until ~init:(accum, false) ~f:(fun (accum, _) (akey, aval) ->
      let accum, until = f accum conflict_state_index akey aval in
      (accum, until), until
    ) attribs
  ) t

let fold ~init ~f t =
  Ordmap.fold ~init ~f:(fun accum (conflict_state_index, attribs) ->
    Attribs.fold ~init:accum ~f:(fun accum (akey, aval) ->
      f accum conflict_state_index akey aval
    ) attribs
  ) t

let merged_of_t t =
  fold ~init:empty ~f:(fun t_merged conflict_state_index akey aval ->
    insert ~conflict_state_index akey aval t_merged
  ) t

let union t0 t1 =
  Ordmap.fold2 ~init:empty ~f:(fun t state_attribs_opt0 state_attribs_opt1 ->
    let conflict_state_index, attribs =
      match state_attribs_opt0, state_attribs_opt1 with
      | Some (conflict_state_index, attribs), None
      | None, Some (conflict_state_index, attribs) -> conflict_state_index, attribs
      | Some (conflict_state_index, attribs0), Some (_, attribs1) ->
        conflict_state_index, Attribs.union attribs0 attribs1
      | None, None -> not_reached ()
    in
    match Attribs.is_empty attribs with
    | true -> t
    | false -> Ordmap.insert_hlt ~k:conflict_state_index ~v:attribs t
  ) (merged_of_t t0) (merged_of_t t1)

let fold2_until ~init ~f t0 t1 =
  let rec inner ~f accum seq0 seq1 = begin
    let left state_index0 symbol_index0 aval0 seq0' = begin
      let accum, until = f accum state_index0 symbol_index0 (Some aval0) None in
      match until with
      | true -> accum
      | false -> inner ~f accum seq0' seq1
    end in
    let right state_index1 symbol_index1 aval1 seq1' = begin
      let accum, until = f accum state_index1 symbol_index1 None (Some aval1) in
      match until with
      | true -> accum
      | false -> inner ~f accum seq0 seq1'
    end in
    match Seq.next_opt seq0, Seq.next_opt seq1 with
    | None, None -> accum
    | Some ((state_index0, akey0, aval0), seq0'), None ->
      left state_index0 akey0 aval0 seq0'
    | None, Some ((state_index1, akey1, aval1), seq1') ->
      right state_index1 akey1 aval1 seq1'
    | Some ((state_index0, akey0, aval0), seq0'),
      Some ((state_index1, akey1, aval1), seq1') -> begin
        let rel = match Uns.cmp state_index0 state_index1 with
          | Cmp.Lt -> Cmp.Lt
          | Eq -> Attribs.Akey.cmp akey0 akey1
          | Gt -> Gt
        in
        match rel with
        | Lt -> left state_index0 akey0 aval0 seq0'
        | Eq -> begin
            let accum, until = f accum state_index0 akey0 (Some aval0) (Some aval1) in
            match until with
            | true -> accum
            | false -> inner ~f accum seq0' seq1'
          end
        | Gt -> right state_index1 akey1 aval1 seq1'
      end
  end in
  inner ~f init (Seq.init t0) (Seq.init t1)

let fold2 ~init ~f t0 t1 =
  fold2_until ~init ~f:(fun accum conflict_state_index symbol_index aval_opt0 aval_opt1 ->
    f accum conflict_state_index symbol_index aval_opt0 aval_opt1, false
  ) t0 t1