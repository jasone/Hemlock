open Rudiments
open Container_common_intf

(* Polymorphic. *)

(* poly3 *)

module Make_poly3_fold (T : I_poly3) : S_poly3_fold_gen
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
   and type 'k key := 'k T.key
   and type 'v value := 'v T.value = struct
  let fold_until ~init ~f t =
    let rec fn accum cursor = begin
      match T.Cursor.(cursor = (tl t)) with
      | true -> accum
      | false -> begin
          let (k, v) = T.Cursor.rget cursor in
          let (accum', until) = f accum (k, v) in
          match until with
          | true -> accum'
          | false -> fn accum' (T.Cursor.succ cursor)
        end
    end in
    fn init (T.Cursor.hd t)

  let fold_right_until ~init ~f t =
    let rec fn t ~f accum cursor = begin
      match T.Cursor.(cursor = (T.Cursor.hd t)) with
      | true -> accum
      | false -> begin
          let (k, v) = T.Cursor.lget cursor in
          let (accum', until) = f (k, v) accum in
          match until with
          | true -> accum'
          | false -> fn t ~f accum' (T.Cursor.pred cursor)
        end
    end in
    fn t ~f init (T.Cursor.tl t)

  let foldi_until ~init ~f t =
    let _, accum = fold_until t ~init:(0, init)
      ~f:(fun (i, accum) (k, v) ->
        let i' = (Usize.succ i) in
        let accum', until = f i accum (k, v) in
        (i', accum'), until
      ) in
    accum

  let fold ~init ~f t =
    fold_until t ~init ~f:(fun accum (k, v) -> (f accum (k, v)), false)

  let fold_right ~init ~f t =
    fold_right_until t ~init ~f:(fun (k, v) accum -> (f (k, v) accum), false)

  let foldi ~init ~f t =
    foldi_until t ~init ~f:(fun i accum (k, v) -> (f i accum (k, v)), false)

  let iter ~f t =
    fold t ~init:() ~f:(fun _ (k, v) -> f (k, v))

  let iteri ~f t =
    foldi t ~init:() ~f:(fun i _ (k, v) -> f i (k, v))

  let count ~f t =
    fold t ~init:0 ~f:(fun accum (k, v) ->
      match f (k, v) with
      | false -> accum
      | true -> (Usize.succ accum)
    )

  let for_any ~f t =
    fold_until t ~init:false ~f:(fun _ (k, v) ->
      let any' = f (k, v) in
      any', any'
    )

  let for_all ~f t =
    fold_until t ~init:true ~f:(fun _ (k, v) ->
      let all' = f (k, v) in
      all', (not all')
    )

  let find ~f t =
    fold_until t ~init:None ~f:(fun _ (k, v) ->
      match f (k, v) with
      | false -> None, false
      | true -> Some (k, v), true
    )

  let find_map ~f t =
    fold_until t ~init:None ~f:(fun _ (k, v) ->
      match f (k, v) with
      | None -> None, false
      | Some a -> Some a, true
    )

  let findi ~f t =
    foldi_until t ~init:None ~f:(fun i _ (k, v) ->
      match f i (k, v) with
      | false -> None, false
      | true -> Some (k, v), true
    )

  let findi_map ~f t =
    foldi_until t ~init:None ~f:(fun i _ (k, v) ->
      match f i (k, v) with
      | None -> None, false
      | Some a -> Some a, true
    )

  let min_elm ~cmp t =
    fold t ~init:None ~f:(fun accum (k, v) ->
      match accum with
      | None -> Some (k, v)
      | Some e -> begin
          match cmp e (k, v) with
          | Cmp.Lt -> Some e
          | Cmp.Eq
          | Cmp.Gt -> Some (k, v)
        end
    )

  let max_elm ~cmp t =
    fold t ~init:None ~f:(fun accum (k, v) ->
      match accum with
      | None -> Some (k, v)
      | Some e -> begin
          match cmp e (k, v) with
          | Cmp.Lt
          | Cmp.Eq -> Some (k, v)
          | Cmp.Gt -> Some e
        end
    )

  let to_list t =
    fold_right t ~init:[] ~f:(fun (k, v) accum -> (k, v) :: accum)

  let to_list_rev t =
    fold t ~init:[] ~f:(fun accum (k, v) -> (k, v) :: accum)
end

(* poly2 *)

module Make_poly2_fold (T : I_poly2) : S_poly2_fold_gen
  with type ('a, 'cmp) t := ('a, 'cmp) T.t
   and type 'a elm := 'a T.elm = struct
  let fold_until ~init ~f t =
    let rec fn accum cursor = begin
      match T.Cursor.(cursor = (tl t)) with
      | true -> accum
      | false -> begin
          let elm = T.Cursor.rget cursor in
          let (accum', until) = f accum elm in
          match until with
          | true -> accum'
          | false -> fn accum' (T.Cursor.succ cursor)
        end
    end in
    fn init (T.Cursor.hd t)

  let fold_right_until ~init ~f t =
    let rec fn t ~f accum cursor = begin
      match T.Cursor.(cursor = (T.Cursor.hd t)) with
      | true -> accum
      | false -> begin
          let elm = T.Cursor.lget cursor in
          let (accum', until) = f elm accum in
          match until with
          | true -> accum'
          | false -> fn t ~f accum' (T.Cursor.pred cursor)
        end
    end in
    fn t ~f init (T.Cursor.tl t)

  let foldi_until ~init ~f t =
    let _, accum = fold_until t ~init:(0, init)
      ~f:(fun (i, accum) elm ->
        let i' = (Usize.succ i) in
        let accum', until = f i accum elm in
        (i', accum'), until
      ) in
    accum

  let fold ~init ~f t =
    fold_until t ~init ~f:(fun accum elm -> (f accum elm), false)

  let fold_right ~init ~f t =
    fold_right_until t ~init ~f:(fun elm accum -> (f elm accum), false)

  let foldi ~init ~f t =
    foldi_until t ~init ~f:(fun i accum elm -> (f i accum elm), false)

  let iter ~f t =
    fold t ~init:() ~f:(fun _ elm -> f elm)

  let iteri ~f t =
    foldi t ~init:() ~f:(fun i _ elm -> f i elm)

  let count ~f t =
    fold t ~init:0 ~f:(fun accum elm ->
      match f elm with
      | false -> accum
      | true -> (Usize.succ accum)
    )

  let for_any ~f t =
    fold_until t ~init:false ~f:(fun _ elm ->
      let any' = f elm in
      any', any'
    )

  let for_all ~f t =
    fold_until t ~init:true ~f:(fun _ elm ->
      let all' = f elm in
      all', (not all')
    )

  let find ~f t =
    fold_until t ~init:None ~f:(fun _ elm ->
      match f elm with
      | false -> None, false
      | true -> Some elm, true
    )

  let find_map ~f t =
    fold_until t ~init:None ~f:(fun _ elm ->
      match f elm with
      | None -> None, false
      | Some a -> Some a, true
    )

  let findi ~f t =
    foldi_until t ~init:None ~f:(fun i _ elm ->
      match f i elm with
      | false -> None, false
      | true -> Some elm, true
    )

  let findi_map ~f t =
    foldi_until t ~init:None ~f:(fun i _ elm ->
      match f i elm with
      | None -> None, false
      | Some a -> Some a, true
    )

  let min_elm ~cmp t =
    fold t ~init:None ~f:(fun accum elm ->
      match accum with
      | None -> Some elm
      | Some e -> begin
          match cmp e elm with
          | Cmp.Lt -> Some e
          | Cmp.Eq
          | Cmp.Gt -> Some elm
        end
    )

  let max_elm ~cmp t =
    fold t ~init:None ~f:(fun accum elm ->
      match accum with
      | None -> Some elm
      | Some e -> begin
          match cmp e elm with
          | Cmp.Lt
          | Cmp.Eq -> Some elm
          | Cmp.Gt -> Some e
        end
    )

  let to_list t =
    fold_right t ~init:[] ~f:(fun elm accum -> elm :: accum)

  let to_list_rev t =
    fold t ~init:[] ~f:(fun accum elm -> elm :: accum)
end

module Make_i_poly2 (T : I_poly) : I_poly2 with type ('a, 'cmp) t = 'a T.t
                                            and type 'a elm = 'a T.elm
= struct
  type ('a, 'cmp) t = 'a T.t
  type 'a elm = 'a T.elm

  module Cursor = struct
    module V = struct
      type ('a, 'cmp) t = 'a T.Cursor.t

      let cmp = T.Cursor.cmp
    end
    include V
    include Cmpable.Make_poly2(V)

    let hd = T.Cursor.hd
    let tl = T.Cursor.tl
    let succ = T.Cursor.succ
    let pred = T.Cursor.pred
    let lget = T.Cursor.lget
    let rget = T.Cursor.rget
  end
end

(* poly[1]. *)

module Make_poly_length (T : I_poly) : S_poly_length_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm = struct
  let length t =
    let rec fn index cursor = begin
      match T.Cursor.(cursor = (tl t)) with
      | true -> index
      | false -> fn (Usize.succ index) (T.Cursor.succ cursor)
    end in
    fn 0 (T.Cursor.hd t)

  let is_empty t =
    (length t) = 0
end

module Make_poly_fold (T : I_poly) : S_poly_fold_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm = struct
  include Make_poly2_fold(Make_i_poly2(T))
end

module Make_poly_mem (T : I_poly_mem) : S_poly_mem_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm = struct
  let mem elm t =
    let rec fn cursor = begin
      match T.Cursor.(cursor = (tl t)) with
      | true -> false
      | false -> begin
          let e = T.Cursor.rget cursor in
          match T.cmp_elm e elm with
          | Eq -> true
          | Lt
          | Gt -> fn (T.Cursor.succ cursor)
        end
    end in
    fn (T.Cursor.hd t)
end

module Make_i_poly (T : I_mono) : I_poly with type 'a t = T.t
                                          and type 'a elm = T.elm
= struct
  type 'a t = T.t
  type 'a elm = T.elm

  module Cursor = struct
    module V = struct
      type 'a t = T.Cursor.t

      let cmp = T.Cursor.cmp
    end
    include V
    include Cmpable.Make_poly(V)

    let hd = T.Cursor.hd
    let tl = T.Cursor.tl
    let succ = T.Cursor.succ
    let pred = T.Cursor.pred
    let lget = T.Cursor.lget
    let rget = T.Cursor.rget
  end
end

(* Monomorphic. *)

module Make_mono_length (T : I_mono) : S_mono_length with type t := T.t
                                                      and type elm := T.elm =
struct
  include Make_poly_length(Make_i_poly(T))
end

module Make_mono_fold (T : I_mono) : S_mono_fold with type t := T.t
                                                  and type elm := T.elm = struct
  include Make_poly_fold(Make_i_poly(T))
end

module Make_i_poly_mem (T : I_mono_mem) : I_poly_mem with type 'a t = T.t
                                                      and type 'a elm = T.elm
= struct
  include Make_i_poly(T)
  let cmp_elm = T.cmp_elm
end

module Make_mono_mem (T : I_mono_mem) : S_mono_mem with type t := T.t
                                                    and type elm := T.elm =
struct
  include Make_poly_mem(Make_i_poly_mem(T))
end
