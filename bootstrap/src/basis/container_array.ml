open Rudiments
open Container_array_intf

(* Polymorphic (poly[1]). *)

module Make_poly_array (T : I_poly_array) : S_poly_array_gen
  with type 'a t := 'a T.t
   and type 'a elm := 'a T.elm = struct
  module Array_seq = struct
    module T = struct
      type 'a t = {
        cursor: 'a T.Cursor.t;
        length: usize;
      }
      type 'a elm = 'a T.elm

      let init container =
        {
          cursor=(T.Cursor.hd container);
          length=(T.length container);
        }

      let length t =
        t.length

      let next t =
        assert (length t > 0);
        let elm = T.Cursor.rget t.cursor in
        let cursor' = T.Cursor.succ t.cursor in
        let length' = (Usize.pred t.length) in
        let t' = {cursor=cursor'; length=length'} in
        elm, t'
    end
    include T
    include Array.Seq.Make_poly(T)
  end

  let to_array t =
    Array_seq.to_array (Array_seq.init t)
end

(* Monomorphic. *)

module Make_i_poly_array (T : I_mono_array) : I_poly_array
  with type 'a t = T.t
   and type 'a elm = T.elm = struct
  include T
  include Container_common.Make_i_poly(T)
end

module Make_mono_array (T : I_mono_array) : S_mono_array
  with type t := T.t
   and type elm := T.elm = struct
  include Make_poly_array(Make_i_poly_array(T))
end

(* Polymorphic (poly2). *)

module Make_poly2_array (T : I_poly2_array) : S_poly2_array_gen
  with type ('a, 'cmp) t := ('a, 'cmp) T.t
   and type 'a elm := 'a T.elm = struct
  module Array_seq = struct
    module T = struct
      type ('a, 'cmp) t = {
        cursor: ('a, 'cmp) T.Cursor.t;
        length: usize;
      }
      type 'a elm = 'a T.elm

      let init container =
        {
          cursor=(T.Cursor.hd container);
          length=(T.length container);
        }

      let length t =
        t.length

      let next t =
        assert (length t > 0);
        let elm = T.Cursor.rget t.cursor in
        let cursor' = T.Cursor.succ t.cursor in
        let length' = (Usize.pred t.length) in
        let t' = {cursor=cursor'; length=length'} in
        elm, t'
    end
    include T
    include Array.Seq.Make_poly2(T)
  end

  let to_array t =
    Array_seq.to_array (Array_seq.init t)
end

(* Polymorphic (poly3). *)

module Make_poly3_array (T : I_poly3_array) : S_poly3_array_gen
  with type ('k, 'v, 'cmp) t := ('k, 'v, 'cmp) T.t
   and type 'k key := 'k T.key
   and type 'v value := 'v T.value = struct
  module Array_seq = struct
    module T = struct
      type ('k, 'v, 'cmp) t = {
        cursor: ('k, 'v, 'cmp) T.Cursor.t;
        length: usize;
      }
      type 'k key= 'k T.key
      type 'v value = 'v T.value

      let init container =
        {
          cursor=(T.Cursor.hd container);
          length=(T.length container);
        }

      let length t =
        t.length

      let next t =
        assert (length t > 0);
        let (k, v) = T.Cursor.rget t.cursor in
        let cursor' = T.Cursor.succ t.cursor in
        let length' = (Usize.pred t.length) in
        let t' = {cursor=cursor'; length=length'} in
        (k, v), t'
    end
    include T
    include Array.Seq.Make_poly3(T)
  end

  let to_array t =
    Array_seq.to_array (Array_seq.init t)
end
