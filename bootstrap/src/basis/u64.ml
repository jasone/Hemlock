include Rudiments_int
open Rudiments

module T = struct
  type t = u64

  let hash_fold t state =
    Hash.State.Gen.init state
    |> Hash.State.Gen.fold_u128 1 ~f:(fun _ -> {hi=Int64.zero; lo=t})
    |> Hash.State.Gen.fini

  let cmp t0 t1 =
    Isize.cmp (Usize.to_isize Int64.(unsigned_compare t0 t1)) (Isize.kv 0)

  let zero = Int64.zero

  let one = Int64.one

  let pp ppf t =
    Format.fprintf ppf "%Luu64" t
end
include T
include Cmpable.Make_zero(T)
include Identifiable.Make(T)

let pp_x ppf t =
  let rec fn x shift = begin
    match shift with
    | 0 -> ()
    | _ -> begin
        if Usize.(shift < 64) then Format.fprintf ppf "_";
        let shift' = shift - 16 in
        Format.fprintf ppf "%04Lx"
          Int64.(logand (shift_right_logical x shift') (of_int 0xffff));
        fn x shift'
      end
  end in
  Format.fprintf ppf "0x";
  fn t 64;
  Format.fprintf ppf "u64"

let of_string s =
  match Caml.String.split_on_char 'x' s with
  | "0" :: _ -> Int64.of_string s (* Has 0x prefix. *)
  | _ -> begin
      (* Prefix with "0u" so that the string is interpreted as unsigned. *)
      Int64.of_string ("0u" ^ s)
    end

let to_string t =
  Format.asprintf "%a" pp t

let cf_ffff_ffff_ffff = Int64.of_string "0xf_ffff_ffff_ffff"
let c7ff = Int64.of_string "0x7ff"
let c10_0000_0000_0000 = Int64.of_string "0x10_0000_0000_0000"

let of_float f =
  match Float.classify f with
  | Normal -> begin
      match Float.(f >= 0.) with
      | false -> zero
      | true -> begin
          let bits = Int64.bits_of_float f in
          let biased_exponent =
            Int64.(to_int (logand (shift_right_logical bits 52) c7ff)) in
          match Rudiments_int.(biased_exponent >= 1023) with
          | false -> zero
          | true -> begin
              let exponent = biased_exponent - 1023 in
              let significand = Int64.(logor c10_0000_0000_0000
                  (logand bits cf_ffff_ffff_ffff)) in
              if Rudiments_int.(exponent < 52) then
                Int64.shift_right_logical significand (52 - exponent)
              else if Rudiments_int.(exponent < 116) then
                Int64.shift_left significand (exponent - 52)
              else
                zero
            end
        end
    end
  | Subnormal
  | Zero -> zero
  | Infinite -> begin
      match Float.(f > 0.) with
      | false -> zero
      | true -> Int64.minus_one
    end
  | Nan -> halt "Not a number"

let c8000_0000_0000_0000 = of_string "0x8000_0000_0000_0000"
let c43e0_0000_0000_0000 = of_string "0x43e0_0000_0000_0000"

let to_float t =
  match Rudiments_int.(Int64.(compare (logand t c8000_0000_0000_0000) zero) = 0)
  with
  | true -> Int64.to_float t
  | false -> begin
      let fraction =
        Int64.(logand (shift_right_logical t 11) cf_ffff_ffff_ffff) in
      let exponent = c43e0_0000_0000_0000 in
      let bits = Int64.logor exponent fraction in
      Int64.float_of_bits bits
    end

let to_i64 t =
  t

let of_i64 i =
  i

let to_usize t =
  Int64.to_int t

let to_usize_hlt t =
  match Int64.unsigned_to_int t with
  | None -> halt "Lossy conversion"
  | Some x -> x

let of_usize u =
  let i = Usize.to_isize u in
  match Isize.(i >= (kv 0)) with
  | true -> Int64.of_int u
  | false -> begin
      let isize_sign_bit = Usize.of_isize Isize.min_value in
      Int64.(add (of_int u)
        (add (of_int isize_sign_bit)
            (of_int isize_sign_bit)))
    end

let min_value = zero

let max_value = Int64.minus_one

let succ t =
  Int64.add t one

let pred t =
  Int64.sub t one

let bit_and t0 t1 =
  Int64.logand t0 t1

let bit_or t0 t1 =
  Int64.logor t0 t1

let bit_xor t0 t1 =
  Int64.logxor t0 t1

let bit_not t =
  Int64.lognot t

let bit_sl ~shift t =
  Int64.shift_left t shift

let bit_usr ~shift t =
  Int64.shift_right_logical t shift

let bit_ssr ~shift t =
  Int64.shift_right t shift

let ( + ) t0 t1 =
  Int64.add t0 t1

let ( - ) t0 t1 =
  Int64.sub t0 t1

let ( * ) t0 t1 =
  Int64.mul t0 t1

let ( / ) t0 t1 =
  Int64.unsigned_div t0 t1

let ( % ) t0 t1 =
  Int64.unsigned_rem t0 t1

let ( ** ) t0 t1 =
  (* Decompose the exponent to limit algorithmic complexity. *)
  let rec fn r p n = begin
    match n = zero with
    | true -> r
    | false -> begin
        let r' =
          match (bit_and n one) = zero with
          | true -> r
          | false -> r * p
        in
        let p' = p * p in
        let n' = bit_usr ~shift:1 n in
        fn r' p' n'
      end
  end in
  fn one t0 t1

let ( // ) t0 t1 =
  (to_float t0) /. (to_float t1)

let c5s = of_string "0x5555_5555_5555_5555"
let c3s = of_string "0x3333_3333_3333_3333"
let c0fs = of_string "0x0f0f_0f0f_0f0f_0f0f"
let c7f = of_string "0x7f"

let bit_pop x =
  let x =
    x - (bit_and (bit_usr ~shift:1 x) c5s) in
  let x = (bit_and x c3s) + (bit_and (bit_usr ~shift:2 x) c3s) in
  let x = bit_and (x + (bit_usr ~shift:4 x)) c0fs in
  let x = x + (bit_usr ~shift:8 x) in
  let x = x + (bit_usr ~shift:16 x) in
  let x = x + (bit_usr ~shift:32 x) in
  to_usize (bit_and x c7f)

let bit_clz x =
  let x = bit_or x (bit_usr ~shift:1 x) in
  let x = bit_or x (bit_usr ~shift:2 x) in
  let x = bit_or x (bit_usr ~shift:4 x) in
  let x = bit_or x (bit_usr ~shift:8 x) in
  let x = bit_or x (bit_usr ~shift:16 x) in
  let x = bit_or x (bit_usr ~shift:32 x) in
  bit_pop (bit_not x)

let bit_ctz t =
  bit_pop (bit_and (bit_not t) (t - one))

module U = struct
  type nonrec t = t

  let num_bits = 64

  let cmp = cmp
  let zero = zero
  let one = one
  let of_usize = of_usize
  let ( + ) = ( + )
  let ( - ) = ( - )
  let bit_and = bit_and
  let bit_sl = bit_sl
  let bit_clz = bit_clz
end
include Intnb.Make_derived(U)

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold us = begin
    match us with
    | [] -> ()
    | u :: us' -> begin
        printf "hash_fold %a -> %a\n"
          pp_x u Hash.pp (Hash.t_of_state (hash_fold u Hash.State.empty));
        test_hash_fold us'
      end
  end in
  let us = [zero; one; min_value; max_value] in
  test_hash_fold us;
  printf "@]";

  [%expect{|
    hash_fold 0x0000_0000_0000_0000u64 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0x0000_0000_0000_0001u64 -> 0x17ed_c9d0_759f_4dce_c1c4_c5ee_1138_72dbu128
    hash_fold 0x0000_0000_0000_0000u64 -> 0xb465_a9ec_cd79_1cb6_4bbd_1bf2_7da9_18d6u128
    hash_fold 0xffff_ffff_ffff_ffffu64 -> 0x913b_a441_dcb5_f088_efa8_6f78_1580_b321u128
    |}]

let%expect_test "limits" =
  let open Format in

  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    min_value=0x0000_0000_0000_0000u64
    max_value=0xffff_ffff_ffff_ffffu64
    |}]

let%expect_test "rel" =
  let open Format in
  let fn x y = begin
    printf "cmp %a %a -> %a\n" pp_x x pp_x y Cmp.pp (cmp x y);
    printf "%a >= %a -> %b\n" pp_x x pp_x y (x >= y);
    printf "%a <= %a -> %b\n" pp_x x pp_x y (x <= y);
    printf "%a = %a -> %b\n" pp_x x pp_x y (x = y);
    printf "%a > %a -> %b\n" pp_x x pp_x y (x > y);
    printf "%a < %a -> %b\n" pp_x x pp_x y (x < y);
    printf "%a <> %a -> %b\n" pp_x x pp_x y (x <> y);
    printf "ascending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (ascending x y);
    printf "descending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (descending x y);
  end in
  fn zero (of_string "0x8000_0000_0000_0000");
  printf "\n";
  fn zero (of_string "0xffff_ffff_ffff_ffff");
  printf "\n";
  fn (of_string "0x8000_0000_0000_0000") (of_string "0x7fff_ffff_ffff_ffff");
  let fn2 t min max = begin
    printf "\n";
    printf "clamp ~min:%a ~max:%a %a -> %a\n"
      pp_x min pp_x max pp_x t pp_x (clamp ~min ~max t);
    printf "between ~low:%a ~high:%a %a -> %b\n"
      pp_x min pp_x max pp_x t (between ~low:min ~high:max t);
  end in
  fn2 (of_string "0x7fff_ffff_ffff_fffe") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x7fff_ffff_ffff_ffff") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0000") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0001") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");
  fn2 (of_string "0x8000_0000_0000_0002") (of_string "0x7fff_ffff_ffff_ffff")
    (of_string "0x8000_0000_0000_0001");

  [%expect{|
    cmp 0x0000_0000_0000_0000u64 0x8000_0000_0000_0000u64 -> Lt
    0x0000_0000_0000_0000u64 >= 0x8000_0000_0000_0000u64 -> false
    0x0000_0000_0000_0000u64 <= 0x8000_0000_0000_0000u64 -> true
    0x0000_0000_0000_0000u64 = 0x8000_0000_0000_0000u64 -> false
    0x0000_0000_0000_0000u64 > 0x8000_0000_0000_0000u64 -> false
    0x0000_0000_0000_0000u64 < 0x8000_0000_0000_0000u64 -> true
    0x0000_0000_0000_0000u64 <> 0x8000_0000_0000_0000u64 -> true
    ascending 0x0000_0000_0000_0000u64 0x8000_0000_0000_0000u64 -> Lt
    descending 0x0000_0000_0000_0000u64 0x8000_0000_0000_0000u64 -> Gt

    cmp 0x0000_0000_0000_0000u64 0xffff_ffff_ffff_ffffu64 -> Lt
    0x0000_0000_0000_0000u64 >= 0xffff_ffff_ffff_ffffu64 -> false
    0x0000_0000_0000_0000u64 <= 0xffff_ffff_ffff_ffffu64 -> true
    0x0000_0000_0000_0000u64 = 0xffff_ffff_ffff_ffffu64 -> false
    0x0000_0000_0000_0000u64 > 0xffff_ffff_ffff_ffffu64 -> false
    0x0000_0000_0000_0000u64 < 0xffff_ffff_ffff_ffffu64 -> true
    0x0000_0000_0000_0000u64 <> 0xffff_ffff_ffff_ffffu64 -> true
    ascending 0x0000_0000_0000_0000u64 0xffff_ffff_ffff_ffffu64 -> Lt
    descending 0x0000_0000_0000_0000u64 0xffff_ffff_ffff_ffffu64 -> Gt

    cmp 0x8000_0000_0000_0000u64 0x7fff_ffff_ffff_ffffu64 -> Gt
    0x8000_0000_0000_0000u64 >= 0x7fff_ffff_ffff_ffffu64 -> true
    0x8000_0000_0000_0000u64 <= 0x7fff_ffff_ffff_ffffu64 -> false
    0x8000_0000_0000_0000u64 = 0x7fff_ffff_ffff_ffffu64 -> false
    0x8000_0000_0000_0000u64 > 0x7fff_ffff_ffff_ffffu64 -> true
    0x8000_0000_0000_0000u64 < 0x7fff_ffff_ffff_ffffu64 -> false
    0x8000_0000_0000_0000u64 <> 0x7fff_ffff_ffff_ffffu64 -> true
    ascending 0x8000_0000_0000_0000u64 0x7fff_ffff_ffff_ffffu64 -> Gt
    descending 0x8000_0000_0000_0000u64 0x7fff_ffff_ffff_ffffu64 -> Lt

    clamp ~min:0x7fff_ffff_ffff_ffffu64 ~max:0x8000_0000_0000_0001u64 0x7fff_ffff_ffff_fffeu64 -> 0x7fff_ffff_ffff_ffffu64
    between ~low:0x7fff_ffff_ffff_ffffu64 ~high:0x8000_0000_0000_0001u64 0x7fff_ffff_ffff_fffeu64 -> false

    clamp ~min:0x7fff_ffff_ffff_ffffu64 ~max:0x8000_0000_0000_0001u64 0x7fff_ffff_ffff_ffffu64 -> 0x7fff_ffff_ffff_ffffu64
    between ~low:0x7fff_ffff_ffff_ffffu64 ~high:0x8000_0000_0000_0001u64 0x7fff_ffff_ffff_ffffu64 -> true

    clamp ~min:0x7fff_ffff_ffff_ffffu64 ~max:0x8000_0000_0000_0001u64 0x8000_0000_0000_0000u64 -> 0x8000_0000_0000_0000u64
    between ~low:0x7fff_ffff_ffff_ffffu64 ~high:0x8000_0000_0000_0001u64 0x8000_0000_0000_0000u64 -> true

    clamp ~min:0x7fff_ffff_ffff_ffffu64 ~max:0x8000_0000_0000_0001u64 0x8000_0000_0000_0001u64 -> 0x8000_0000_0000_0001u64
    between ~low:0x7fff_ffff_ffff_ffffu64 ~high:0x8000_0000_0000_0001u64 0x8000_0000_0000_0001u64 -> true

    clamp ~min:0x7fff_ffff_ffff_ffffu64 ~max:0x8000_0000_0000_0001u64 0x8000_0000_0000_0002u64 -> 0x8000_0000_0000_0001u64
    between ~low:0x7fff_ffff_ffff_ffffu64 ~high:0x8000_0000_0000_0001u64 0x8000_0000_0000_0002u64 -> false
    |}]

let%expect_test "narrowing" =
  let open Format in
  let fifteen = of_string "15" in
  printf "max_value + %a -> %a\n" pp one pp_x (max_value + one);
  printf "min_value - %a -> %a\n" pp one pp_x (min_value - one);
  printf "max_value * %a -> %a\n" pp fifteen pp_x (max_value * fifteen);

  [%expect{|
    max_value + 1u64 -> 0x0000_0000_0000_0000u64
    min_value - 1u64 -> 0xffff_ffff_ffff_ffffu64
    max_value * 15u64 -> 0xffff_ffff_ffff_fff1u64
    |}]

let%expect_test "of_float,to_float" =
  let open Format in
  printf "@[<h>";
  let rec test_fs fs = begin
    match fs with
    | [] -> ()
    | f :: fs' -> begin
        let x = of_float f in
        printf "of_float %h -> %a; to_float -> %h\n"
          f pp_x x (to_float x);
        test_fs fs'
      end
  end in
  let fs = [
    -1.;
    0.;
    0x1.1p-1;
    1.;

    0x1.f_ffff_ffff_ffffp48;
    0x1.f_ffff_ffff_ffffp52;
    0x1.f_ffff_ffff_ffffp56;

    0x1.f_ffff_ffff_ffffp63;
    0x1.f_ffff_ffff_ffffp64;
    0x1.f_ffff_ffff_ffffp68;

    0x1p62;
    0x1p63;
    0x1p64;
  ] in
  test_fs fs;
  printf "\n";
  let rec test_xs xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        let f = to_float x in
        printf "to_float %a -> %h; of_float -> %a\n"
          pp_x x f pp_x (of_float f);
        test_xs xs'
      end
  end in
  let xs = [
    zero;
    one;
    max_value;
  ] in
  test_xs xs;
  printf "@]";

  [%expect{|
    of_float -0x1p+0 -> 0x0000_0000_0000_0000u64; to_float -> 0x0p+0
    of_float 0x0p+0 -> 0x0000_0000_0000_0000u64; to_float -> 0x0p+0
    of_float 0x1.1p-1 -> 0x0000_0000_0000_0000u64; to_float -> 0x0p+0
    of_float 0x1p+0 -> 0x0000_0000_0000_0001u64; to_float -> 0x1p+0
    of_float 0x1.fffffffffffffp+48 -> 0x0001_ffff_ffff_ffffu64; to_float -> 0x1.ffffffffffffp+48
    of_float 0x1.fffffffffffffp+52 -> 0x001f_ffff_ffff_ffffu64; to_float -> 0x1.fffffffffffffp+52
    of_float 0x1.fffffffffffffp+56 -> 0x01ff_ffff_ffff_fff0u64; to_float -> 0x1.fffffffffffffp+56
    of_float 0x1.fffffffffffffp+63 -> 0xffff_ffff_ffff_f800u64; to_float -> 0x1.fffffffffffffp+63
    of_float 0x1.fffffffffffffp+64 -> 0xffff_ffff_ffff_f000u64; to_float -> 0x1.ffffffffffffep+63
    of_float 0x1.fffffffffffffp+68 -> 0xffff_ffff_ffff_0000u64; to_float -> 0x1.fffffffffffep+63
    of_float 0x1p+62 -> 0x4000_0000_0000_0000u64; to_float -> 0x1p+62
    of_float 0x1p+63 -> 0x8000_0000_0000_0000u64; to_float -> 0x1p+63
    of_float 0x1p+64 -> 0x0000_0000_0000_0000u64; to_float -> 0x0p+0

    to_float 0x0000_0000_0000_0000u64 -> 0x0p+0; of_float -> 0x0000_0000_0000_0000u64
    to_float 0x0000_0000_0000_0001u64 -> 0x1p+0; of_float -> 0x0000_0000_0000_0001u64
    to_float 0xffff_ffff_ffff_ffffu64 -> 0x1.fffffffffffffp+63; of_float -> 0xffff_ffff_ffff_f800u64
    |}]

let%expect_test "bit_and,bit_or,bit_xor" =
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "bit_{and,or,xor} %a %a -> %a, %a, %a\n"
          pp_x x pp_x y
          pp_x (bit_and x y)
          pp_x (bit_or x y)
          pp_x (bit_xor x y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0");
    (of_string "0", of_string "0xffff_ffff_ffff_ffff");
    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    bit_{and,or,xor} 0x0000_0000_0000_0000u64 0x0000_0000_0000_0000u64 -> 0x0000_0000_0000_0000u64, 0x0000_0000_0000_0000u64, 0x0000_0000_0000_0000u64
    bit_{and,or,xor} 0xffff_ffff_ffff_ffffu64 0x0000_0000_0000_0000u64 -> 0x0000_0000_0000_0000u64, 0xffff_ffff_ffff_ffffu64, 0xffff_ffff_ffff_ffffu64
    bit_{and,or,xor} 0x0000_0000_0000_0000u64 0xffff_ffff_ffff_ffffu64 -> 0x0000_0000_0000_0000u64, 0xffff_ffff_ffff_ffffu64, 0xffff_ffff_ffff_ffffu64
    bit_{and,or,xor} 0xffff_ffff_ffff_ffffu64 0xffff_ffff_ffff_ffffu64 -> 0xffff_ffff_ffff_ffffu64, 0xffff_ffff_ffff_ffffu64, 0x0000_0000_0000_0000u64
    |}]

let%expect_test "bit_not" =
  let open Format in
  printf "@[<h>";
  let rec test = function
    | [] -> ()
    | x :: xs' -> begin
        printf "bit_not %a -> %a\n"
          pp_x x pp_x (bit_not x);
        test xs'
      end
  in
  let xs = [
    of_string "0";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test xs;
  printf "@]";

  [%expect{|
    bit_not 0x0000_0000_0000_0000u64 -> 0xffff_ffff_ffff_ffffu64
    bit_not 0xffff_ffff_ffff_ffffu64 -> 0x0000_0000_0000_0000u64
    |}]

let%expect_test "bit_pop,bit_clz,bit_ctz" =
  let open Format in
  printf "@[<h>";
  let rec test_u64s = function
    | [] -> ()
    | u :: us' -> begin
        printf "bit_{pop,clz,ctz} %a -> %u, %u, %u\n"
          pp_x u (bit_pop u) (bit_clz u) (bit_ctz u);
        test_u64s us'
      end
  in
  let us = [
    of_string "0";
    of_string "1";
    of_string "0x8000_0000_0000_0000";
    of_string "0xffff_ffff_ffff_ffff"
  ] in
  test_u64s us;
  printf "@]";

  [%expect{|
    bit_{pop,clz,ctz} 0x0000_0000_0000_0000u64 -> 0, 64, 64
    bit_{pop,clz,ctz} 0x0000_0000_0000_0001u64 -> 1, 63, 0
    bit_{pop,clz,ctz} 0x8000_0000_0000_0000u64 -> 1, 0, 63
    bit_{pop,clz,ctz} 0xffff_ffff_ffff_ffffu64 -> 64, 0, 0
    |}]

let%expect_test "**" =
  let open Format in
  printf "@[<h>";
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        printf "%a ** %a -> %a\n" pp_x x pp_x y pp_x (x ** y);
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");

    (of_string "0xffff_ffff_ffff_ffff", of_string "0");
    (of_string "0xffff_ffff_ffff_ffff", of_string "1");

    (of_string "2", of_string "31");
    (of_string "2", of_string "32");
    (of_string "2", of_string "63");
    (of_string "2", of_string "64");

    (of_string "0xf", of_string "0xf");
    (of_string "0xff", of_string "0xff");

    (of_string "1", of_string "0xffff_ffff_ffff_ffff");

    (of_string "0xffff_ffff_ffff_ffff", of_string "0xffff_ffff_ffff_ffff");
  ] in
  test_pairs pairs;
  printf "@]";

  [%expect{|
    0x0000_0000_0000_0000u64 ** 0x0000_0000_0000_0000u64 -> 0x0000_0000_0000_0001u64
    0x0000_0000_0000_0000u64 ** 0x0000_0000_0000_0001u64 -> 0x0000_0000_0000_0000u64
    0xffff_ffff_ffff_ffffu64 ** 0x0000_0000_0000_0000u64 -> 0x0000_0000_0000_0001u64
    0xffff_ffff_ffff_ffffu64 ** 0x0000_0000_0000_0001u64 -> 0xffff_ffff_ffff_ffffu64
    0x0000_0000_0000_0002u64 ** 0x0000_0000_0000_001fu64 -> 0x0000_0000_8000_0000u64
    0x0000_0000_0000_0002u64 ** 0x0000_0000_0000_0020u64 -> 0x0000_0001_0000_0000u64
    0x0000_0000_0000_0002u64 ** 0x0000_0000_0000_003fu64 -> 0x8000_0000_0000_0000u64
    0x0000_0000_0000_0002u64 ** 0x0000_0000_0000_0040u64 -> 0x0000_0000_0000_0000u64
    0x0000_0000_0000_000fu64 ** 0x0000_0000_0000_000fu64 -> 0x0613_b62c_5977_07efu64
    0x0000_0000_0000_00ffu64 ** 0x0000_0000_0000_00ffu64 -> 0x5997_756b_007f_feffu64
    0x0000_0000_0000_0001u64 ** 0xffff_ffff_ffff_ffffu64 -> 0x0000_0000_0000_0001u64
    0xffff_ffff_ffff_ffffu64 ** 0xffff_ffff_ffff_ffffu64 -> 0xffff_ffff_ffff_ffffu64
    |}]
